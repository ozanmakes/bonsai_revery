open Core_kernel
open Bonsai_revery
open Bonsai_revery.Components
open Bonsai.Infix

module Filter = struct
  type t =
    | All
    | Active
    | Completed
  [@@deriving sexp, equal]

  let to_string (v : t) =
    match v with
    | All -> "All"
    | Active -> "Active"
    | Completed -> "Completed"
end

module Todo = struct
  type t =
    { title : string
    ; completed : bool
    }
  [@@deriving sexp, equal, fields]

  let toggle todo = { todo with completed = not todo.completed }

  let is_visible { completed; _ } ~filter =
    match filter with
    | Filter.All -> true
    | Active -> not completed
    | Completed -> completed
end

module Input = struct
  type t = string * (string -> Event.t) * Element.t
end

module Model = struct
  type t =
    { todos : Todo.t Map.M(Int).t
    ; filter : Filter.t
    }
  [@@deriving sexp, equal]

  let default =
    { todos =
        Todo.
          [ 0, { title = "Buy Milk"; completed = false }
          ; 1, { title = "Wag the Dog"; completed = true }
          ]
        |> Map.of_alist_exn (module Int)
    ; filter = Filter.All
    }
end

module Action = struct
  type t =
    | Add of string
    | Set_filter of Filter.t
    | Toggle of int
    | Remove of int
    | Toggle_all
    | Clear_completed
  [@@deriving sexp_of]
end

module Result = Element

module Theme = struct
  let font_size = 25.
  let rem factor = font_size *. factor
  let remi factor = rem factor |> Float.to_int
  let app_background = Color.hex "#f4edfe"
  let text_color = Color.hex "#513B70"
  let dimmed_text_color = Color.hex "#DAC5F7"
  let title_text_color = Color.hex "#EADDFC"
  let panel_background = Color.hex "#F9F5FF"
  let panel_border_color = Color.hex "#EADDFC"
  let panel_border = Style.border ~width:1 ~color:panel_border_color
  let button_color = Color.hex "#9573C4"
  let hovered_button_color = Color.hex "#C9AEF0"
  let danger_color = Color.hex "#f7c5c6"
  let font_info = Attr.KindSpec.(TextNode (Text.make ~size:font_size ()))
  let bonsai_path = "bonsai.png"
end

module Styles = struct
  let app_container =
    Style.
      [ position `Absolute
      ; top 0
      ; bottom 0
      ; left 0
      ; right 0
      ; align_items `Stretch
      ; justify_content `Center
      ; flex_direction `Column
      ; background_color Theme.app_background
      ; padding_vertical 2
      ; padding_horizontal 6
      ; overflow `Hidden
      ]


  let bonsai =
    Style.
      [ align_self `FlexStart
      ; margin_top (Theme.remi 2.)
      ; margin_bottom (Theme.remi 2.)
      ; margin_left 50
      ; margin_right 100
      ; width 150
      ; height 150
      ]


  let title =
    Style.
      [ color Theme.title_text_color
      ; align_self `Center
      ; margin_top (Theme.remi 2.)
      ; text_wrap NoWrap
      ]


  let title_font =
    Attr.KindSpec.update_text ~f:(fun a -> { a with size = Theme.rem 4. }) Theme.font_info
end

module Components = struct
  module Button = struct
    module Styles = struct
      let box ~selected ~hovered =
        Style.
          [ position `Relative
          ; justify_content `Center
          ; align_items `Center
          ; padding_vertical (Theme.remi 0.15)
          ; padding_horizontal (Theme.remi 0.5)
          ; margin_horizontal (Theme.remi 0.2)
          ; border
              ~width:1
              ~color:
                ( match selected, hovered with
                | true, _ -> Theme.button_color
                | false, true -> Theme.hovered_button_color
                | false, false -> Colors.transparent_white )
          ; border_radius 2.
          ]


      let text = Style.[ color Theme.button_color; text_wrap NoWrap ]

      let font =
        Attr.KindSpec.update_text ~f:(fun a -> { a with size = Theme.rem 0.8 }) Theme.font_info
    end

    let view ~selected on_click title =
      button
        (fun ~hovered ->
          [ Attr.style (List.append Styles.text (Styles.box ~selected ~hovered))
          ; Attr.on_click on_click
          ; Attr.kind Styles.font
          ])
        title
  end

  module Checkbox = struct
    module Input = struct
      type t =
        { checked : bool
        ; on_toggle : Event.t
        }
    end

    module Styles = struct
      let box =
        Style.
          [ width (Theme.remi 1.5)
          ; height (Theme.remi 1.5)
          ; justify_content `Center
          ; align_items `Center
          ; Theme.panel_border
          ]


      let checkmark =
        Style.[ color Theme.hovered_button_color; text_wrap NoWrap; transform [ TranslateY 2. ] ]


      let checkmark_font =
        Attr.KindSpec.update_text
          ~f:(fun a -> { a with family = Revery.Font.Family.fromFile "FontAwesome5FreeSolid.otf" })
          Theme.font_info
    end

    let view ~checked ~on_toggle =
      box
        Attr.[ on_click on_toggle; style Styles.box ]
        [ text
            Attr.[ style Styles.checkmark; kind Styles.checkmark_font ]
            (if checked then {||} else "")
        ]
  end

  module Todo = struct
    module Styles = struct
      let box =
        Style.
          [ flex_direction `Row
          ; margin 2
          ; padding_vertical 4
          ; padding_horizontal 8
          ; align_items `Center
          ; background_color Theme.panel_background
          ; Theme.panel_border
          ]


      let text is_checked =
        Style.
          [ margin 6
          ; color (if is_checked then Theme.dimmed_text_color else Theme.text_color)
          ; flex_grow 1
          ]


      let remove_button is_hovered =
        Style.
          [ color
              ( match is_hovered with
              | true -> Theme.danger_color
              | false -> Colors.transparent_white )
          ; transform [ TranslateY 2. ]
          ; margin_right 6
          ]


      let remove_button_font =
        Attr.KindSpec.update_text
          ~f:(fun a -> { a with family = Revery.Font.Family.fromFile "FontAwesome5FreeSolid.otf" })
          Theme.font_info
    end

    let view ~task:_ = box Attr.[ style Styles.box ] []

    let component =
      Bonsai.pure ~f:(fun ((key : int), (todo : Todo.t), (inject : Action.t -> Event.t)) ->
          box
            Attr.[ style Styles.box ]
            [ Checkbox.view ~checked:todo.completed ~on_toggle:(inject (Action.Toggle key))
            ; text Attr.[ style (Styles.text todo.completed); kind Theme.font_info ] todo.title
            ; box
                Attr.[ on_click Event.no_op ]
                [ text
                    Attr.[ style (Styles.remove_button false); kind Styles.remove_button_font ]
                    {||}
                ]
            ])
  end

  module Add_todo = struct
    module Styles = struct
      let container =
        Style.
          [ flex_direction `Row
          ; background_color Theme.panel_background
          ; Theme.panel_border
          ; margin 2
          ; align_items `Center
          ; overflow `Hidden
          ]


      let toggle_all all_completed =
        Style.
          [ color (if all_completed then Theme.text_color else Theme.dimmed_text_color)
          ; transform [ TranslateY 2. ]
          ; margin_left 12
          ]


      let toggle_all_font =
        Attr.KindSpec.update_text
          ~f:(fun a -> { a with family = Revery.Font.Family.fromFile "FontAwesome5FreeSolid.otf" })
          Theme.font_info


      let input = Style.[ border ~width:0 ~color:Colors.transparent_white; width 4000 ]
    end

    let view ~all_completed ~on_toggle_all children =
      box
        Attr.[ style Styles.container ]
        ( box
            Attr.[ on_click on_toggle_all ]
            [ text
                Attr.[ style (Styles.toggle_all all_completed); kind Styles.toggle_all_font ]
                {||}
            ]
        :: children )
  end

  module Footer = struct
    module Styles = struct
      let container =
        let open Style in
        [ flex_direction `Row; justify_content `SpaceBetween ]


      let filterButtonsContainer =
        let open Style in
        [ flex_grow 1
        ; width 0
        ; flex_direction `Row
        ; align_items `Center
        ; justify_content `Center
        ; align_self `Center
        ; transform [ TranslateY (-2.) ]
        ]


      let left_flex_container = Style.[ flex_grow 1; width 0 ]

      let right_flex_container =
        Style.[ flex_grow 1; width 0; flex_direction `Row; justify_content `FlexEnd ]


      let items_left = Style.[ color Theme.button_color; text_wrap NoWrap ]

      let clear_completed isHovered =
        Style.
          [ color (if isHovered then Theme.hovered_button_color else Theme.button_color)
          ; text_wrap NoWrap
          ]


      let font =
        Attr.KindSpec.update_text ~f:(fun a -> { a with size = Theme.rem 0.85 }) Theme.font_info
    end

    let view ~inject ~active_count ~completed_count ~current_filter =
      let items_left =
        text Attr.[ style Styles.items_left; kind Styles.font ]
        @@
        match active_count with
        | 1 -> "1 item left"
        | n -> Printf.sprintf "%i items left" n in
      let filter_buttons_view =
        let button filter =
          Button.view
            ~selected:(Filter.equal current_filter filter)
            (inject (Action.Set_filter filter))
            (Filter.to_string filter) in
        box
          Attr.[ style Styles.filterButtonsContainer ]
          [ button All; button Active; button Completed ] in

      let clear_completed =
        let text =
          match completed_count with
          | 0 -> ""
          | n -> Printf.sprintf "Clear completed (%i)" n in

        button
          (fun ~hovered ->
            Attr.
              [ on_click (inject Action.Clear_completed)
              ; style (Styles.clear_completed hovered)
              ; kind Styles.font
              ])
          text in

      box
        Attr.[ style Styles.container ]
        [ box Attr.[ style Styles.left_flex_container ] [ items_left ]
        ; filter_buttons_view
        ; box Attr.[ style Styles.right_flex_container ] [ clear_completed ]
        ]
  end
end

let todo_list =
  let%map.Bonsai todos =
    Tuple2.map_fst ~f:(fun model ->
        Map.filter model.Model.todos ~f:(Todo.is_visible ~filter:model.filter))
    @>> Bonsai.Map.associ_input_with_extra (module Int) Components.Todo.component in
  box Attr.[ style Style.[ flex_grow 1 ] ] (Map.data todos)


let text_input =
  Bonsai.pure ~f:(fun (_model, inject) ->
      Text_input.props
        ~placeholder:"Add your Todo here!"
        ~autofocus:true
        ~on_key_down:(fun event value set_value ->
          match event.key with
          | Return when not (String.is_empty value) ->
            Event.Many [ inject (Action.Add value); set_value "" ]
          | _ -> Event.no_op)
        Attr.[ kind Theme.font_info ])
  >>> Text_input.component


let add_todo =
  let%map.Bonsai model, inject = Bonsai.pure ~f:Fn.id
  and _value, _set_value, text_input = text_input in
  let all_completed = Map.for_all model.Model.todos ~f:Todo.completed in

  Components.Add_todo.view ~all_completed ~on_toggle_all:(inject Action.Toggle_all) [ text_input ]


let footer =
  Bonsai.pure ~f:(fun (model, inject) ->
      let completed_count = Map.count model.Model.todos ~f:Todo.completed in
      let active_count = Map.length model.todos - completed_count in

      Components.Footer.view ~inject ~active_count ~completed_count ~current_filter:model.filter)


let state_component =
  Bonsai.state_machine
    (module Model)
    (module Action)
    [%here]
    ~default_model:Model.default
    ~apply_action:(fun ~inject:_ ~schedule_event:_ () model -> function
      | Add title ->
        let key =
          match Map.max_elt model.todos with
          | Some (key, _) -> key + 1
          | None -> 0 in
        let todos = Map.add_exn model.todos ~key ~data:{ title; completed = false } in
        { model with todos }
      | Toggle key ->
        let todos = Map.change model.todos key ~f:(Option.map ~f:Todo.toggle) in
        { model with todos }
      | Remove key ->
        let todos = Map.remove model.todos key in
        { model with todos }
      | Set_filter filter -> { model with filter }
      | Toggle_all ->
        let are_all_completed = Map.for_all model.todos ~f:Todo.completed in
        let todos =
          Map.map model.todos ~f:(fun todo -> { todo with completed = not are_all_completed }) in
        { model with todos }
      | Clear_completed ->
        let todos = Map.filter model.todos ~f:(Fun.negate Todo.completed) in
        { model with todos })


let app : (unit, Element.t) Bonsai_revery.Bonsai.t =
  state_component
  >>> let%map.Bonsai todo_list = todo_list
      and add_todo = add_todo
      and footer = footer in
      let title = text Attr.[ style Styles.title; kind Styles.title_font ] "todoMVC" in
      let bonsai =
        image
          Attr.
            [ style Styles.bonsai
            ; kind KindSpec.(ImageNode (Image.make ~source:(Image.File Theme.bonsai_path) ()))
            ] in
      let header =
        box
          Attr.[ style Style.[ justify_content `FlexStart; flex_direction `Row ] ]
          [ bonsai; title ] in

      box Attr.[ style Styles.app_container ] [ header; add_todo; todo_list; footer ]
