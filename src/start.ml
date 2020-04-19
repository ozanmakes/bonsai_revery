open! Core_kernel
open! Import

let fill_if_empty t x = if Lwt_mvar.is_empty t then Lwt.async (fun () -> Lwt_mvar.put t x)

module Handle = struct
  module Injector = struct
    type 'a t =
      | Before_app_start of 'a Queue.t
      | Inject of ('a -> Event.t)
  end

  type ('input, 'extra, 'incoming, 'outgoing) t =
    { mutable injector : 'incoming Injector.t
    ; stop : unit Lwt_mvar.t
    ; started : unit Lwt_mvar.t
    ; input_var : 'input Incr.Var.t
    ; outgoing_pipe : 'outgoing Lwt_stream.t
    ; extra : ('extra -> unit) Bus.Read_write.t
    ; last_extra : 'extra Moption.t
    }

  let create ~input_var ~outgoing_pipe =
    let extra =
      Bus.create
        [%here]
        Arity1
        ~on_subscription_after_first_write:Allow_and_send_last_value
        ~on_callback_raise:(fun error -> eprint_s [%sexp (error : Error.t)]) in
    let last_extra = Moption.create () in
    Bus.iter_exn extra [%here] ~f:(fun extra -> Moption.set_some last_extra extra);
    { injector = Before_app_start (Queue.create ())
    ; stop = Lwt_mvar.create_empty ()
    ; started = Lwt_mvar.create_empty ()
    ; input_var
    ; outgoing_pipe
    ; extra
    ; last_extra
    }


  let stop t = fill_if_empty t.stop ()
  let started t = Lwt_mvar.take t.started

  let schedule t a =
    match t.injector with
    | Inject f -> f a |> Event.Expert.handle
    | Before_app_start queue -> Queue.enqueue queue a


  let set_started t = fill_if_empty t.started ()

  let set_inject t inject =
    let prev = t.injector in
    t.injector <- Inject inject;
    match prev with
    | Inject _ -> ()
    | Before_app_start queue -> Queue.iter queue ~f:(schedule t)


  let input t = Incr.Var.value t.input_var
  let set_input t input = Incr.Var.set t.input_var input
  let update_input t ~f = set_input t (f (input t))
  let outgoing { outgoing_pipe; _ } = outgoing_pipe
  let extra t = Bus.read_only t.extra
  let last_extra t = Moption.get t.last_extra
end

module App_input = struct
  type ('input, 'outgoing) t =
    { input : 'input
    ; inject_outgoing : 'outgoing -> Event.t
    }
  [@@deriving fields]

  let create = Fields.create
end

module App_result = struct
  type ('extra, 'incoming) t =
    { view : Element.t
    ; extra : 'extra
    ; inject_incoming : 'incoming -> Event.t
    }
  [@@deriving fields]

  let create = Fields.create
end

let start_generic_poly
    (type input input_and_inject model action result extra incoming outgoing)
    ~(with_handle : ((input, extra, incoming, outgoing) Handle.t -> unit Lwt.t) option)
    ~(get_app_result : result -> (extra, incoming) App_result.t)
    ~(get_app_input : input:input -> inject_outgoing:(outgoing -> Event.t) -> input_and_inject)
    ~(initial_input : input)
    ~(initial_model : model)
    ~(component :
       ( input_and_inject
       , model
       , action
       , result
       , Incr.state_witness
       , Event.t )
       Bonsai_lib.Generic.Expert.unpacked)
    ~(action_type_id : action Type_equal.Id.t)
    : unit
  =
  let outgoing_pipe, pipe_write = Lwt_stream.create () in
  let module Out_event =
    Event.Define (struct
      module Action = struct
        type t = outgoing
      end

      let handle action = pipe_write (Some action)
    end)
  in
  let input_var = Incr.Var.create initial_input in
  let handle = Handle.create ~input_var ~outgoing_pipe in
  let module Revery_app = struct
    module Action = struct
      type t = action
    end

    let create model ~old_model ~inject =
      let open Incr.Let_syntax in
      let old_model = old_model >>| Option.some in
      let input =
        let%map input = Incr.Var.watch input_var in
        get_app_input ~input ~inject_outgoing:Out_event.inject in

      let%map snapshot =
        Bonsai_lib.Generic.Expert.eval
          ~input
          ~old_model
          ~model
          ~inject
          ~action_type_id
          ~incr_state:Incr.State.t
          component
      and model = model in
      let apply_action = Bonsai_lib.Generic.Expert.Snapshot.apply_action snapshot in
      let apply_action action () ~schedule_action:_ =
        apply_action ~schedule_event:Event.Expert.handle action in
      let result = Bonsai_lib.Generic.Expert.Snapshot.result snapshot in
      let { App_result.view; extra; inject_incoming } = get_app_result result in
      Handle.set_inject handle inject_incoming;
      Bus.write handle.extra extra;
      let on_display () ~schedule_action:_ = Handle.set_started handle in

      view, apply_action, on_display, model
  end
  in
  let _stopEventLoop = Revery_Lwt.startEventLoop () in

  Revery.App.start (fun reveryApp ->
      let model_v = Incr.Var.create initial_model in
      let model = Incr.Var.watch model_v in
      let model_from_last_display_v = Incr.Var.create initial_model in
      let model_from_last_display = Incr.Var.watch model_from_last_display_v in
      let cutoff =
        Incr.Cutoff.create (fun ~old_value ~new_value -> phys_equal old_value new_value) in
      Incr.set_cutoff model cutoff;
      Incr.set_cutoff model_from_last_display cutoff;

      let r, w = Lwt_stream.create () in
      let schedule_action action = w (Some action) in

      let module Event =
        Import.Event.Define (struct
          module Action = Revery_app.Action

          let handle action = w (Some action)
        end)
      in
      let app =
        Incr.observe
          (Revery_app.create model ~old_model:model_from_last_display ~inject:Event.inject) in
      Timber.Log.perf "initial stabilize" Incr.stabilize;

      let window = Revery.App.createWindow reveryApp "Bonsai_revery app" in

      let node, _apply_action, _on_display, _model = Incr.Observer.value_exn app in
      let redraw = Revery.UI.start window node in

      Incr.Observer.on_update_exn app ~f:(function
        | Initialized (node, _, _, _) | Changed (_, (node, _, _, _)) -> redraw node
        | Invalidated -> ());

      let apply_action action =
        let _node, apply_action, _on_display, _model = Incr.Observer.value_exn app in
        let new_model = apply_action action () ~schedule_action in
        Incr.Var.set model_v new_model;
        Timber.Log.perf "post-action stabilize" Incr.stabilize in

      Option.iter with_handle ~f:(fun f -> Lwt.async (fun () -> f handle));

      Lwt.async (fun () ->
          Lwt_stream.iter
            (fun action ->
              Incr.Clock.advance_clock Incr.clock ~to_:(Time_ns.now ());
              Timber.Log.perf "pre-action stabilize" Incr.stabilize;

              apply_action action)
            r))


let start_generic ~with_handle ~get_app_result ~initial_input ~component =
  let (T { unpacked; action_type_id; model }) =
    component |> Bonsai.to_generic |> Bonsai_lib.Generic.Expert.reveal in
  start_generic_poly
    ~with_handle
    ~get_app_result
    ~initial_input
    ~initial_model:model.default
    ~component:unpacked
    ~action_type_id


let start_standalone ~initial_input component =
  start_generic
    ~with_handle:None
    ~get_app_result:(fun view ->
      { App_result.view; extra = (); inject_incoming = Nothing.unreachable_code })
    ~get_app_input:(fun ~input ~inject_outgoing:_ -> input)
    ~initial_input
    ~component


let start ~initial_input ?with_handle component =
  start_generic
    ~with_handle
    ~get_app_result:Fn.id
    ~get_app_input:App_input.create
    ~initial_input
    ~component
