(** [Bonsai] instance for use with [Bonsai_revery].

    @see <https://github.com/janestreet/bonsai> for an introduction to Bonsai. *)
module Bonsai = struct
  include Import.Bonsai (** @inline *)
end

module Start = Start

(** UI element type [Bonsai_revery] can render *)
module Element = struct
  include Import.Element (** @inline *)
end

(** UI events *)
module Event : sig
  type t = Import.Event.t = private ..
  type t += Ignore | Many of t list

  val sequence : t list -> t
  val no_op : t

  module Expert : sig
    val handle : t -> unit
  end
end = struct
  include Import.Event (** @inline *)
end

(** Element attributes accepted by {!Components} *)
module Attr : sig
  type kind_spec = Attributes.kind_spec
  type t = Attributes.t

  open Node_events

  val make_text_kind
    :  ?family:Revery.Font.Family.t
    -> ?weight:Revery.Font.Weight.t
    -> ?size:float
    -> ?smoothing:Revery.Font.Smoothing.t
    -> ?italicized:bool
    -> ?underlined:bool
    -> unit
    -> kind_spec

  val map_text_kind : f:(Attributes.text_spec -> Attributes.text_spec) -> kind_spec -> kind_spec
  val empty : t
  val node_ref : (UI.node -> Event.t) -> t
  val on_mouse_down : (Mouse_button.t -> Event.t) -> t
  val on_mouse_move : (Mouse_move.t -> Event.t) -> t
  val on_mouse_up : (Mouse_button.t -> Event.t) -> t
  val on_mouse_wheel : (Mouse_wheel.t -> Event.t) -> t
  val on_key_down : (Keyboard.t -> Event.t) -> t
  val on_key_up : (Keyboard.t -> Event.t) -> t
  val on_text_input : (Text_input.t -> Event.t) -> t
  val on_text_edit : (Text_edit.t -> Event.t) -> t
  val on_mouse_enter : (Mouse_move.t -> Event.t) -> t
  val on_mouse_leave : (Mouse_move.t -> Event.t) -> t
  val on_mouse_over : (Mouse_move.t -> Event.t) -> t
  val on_mouse_out : (Mouse_move.t -> Event.t) -> t
  val on_dimensions_changed : (Dimensions_changed.t -> Event.t) -> t
  val on_focus : Event.t -> t
  val on_blur : Event.t -> t
  val on_click : Event.t -> t
  val on_right_click : Event.t -> t
  val on_any_click : Event.t -> t
  val tab_index : int -> t
  val style : Style.t list -> t
  val kind : Attributes.kind_spec -> t
end =
  Attributes

module Style = Style

(** [Components] contains built-in primitives and components useful for building user interfaces
    with Bonsai_revery. *)
module Components : sig
  val box : Attr.t list -> Element.t list -> Element.t
  val text : Attr.t list -> string -> Element.t
  val image : Attr.t list -> string -> Element.t
  val opacity : ?opacity:float -> Element.t list -> Element.t
  val tick : Element.t -> every:Core_kernel.Time.Span.t -> Element.t

  val button
    :  ?disabled:bool
    -> ?disabled_attr:Attr.t list
    -> (hovered:bool -> Attr.t list)
    -> string
    -> Element.t

  module Text_input : sig
    type props =
      { autofocus : bool
      ; cursor_color : Color.t
      ; placeholder : string
      ; placeholder_color : Color.t
      ; default_value : string option
      ; on_key_down : Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t
      ; attributes : Attr.t list
      }

    val props
      :  ?autofocus:bool
      -> ?cursor_color:Color.t
      -> ?placeholder:string
      -> ?placeholder_color:Color.t
      -> ?default_value:string
      -> ?on_key_down:(Node_events.Keyboard.t -> string -> (string -> Event.t) -> Event.t)
      -> Attr.t list
      -> props

    val component : (props, string * (string -> Event.t) * Element.t) Bonsai.t
  end

  module Expert : sig
    type 'a component =
      ?key:UI.React.Key.t
      -> (('a, 'a) UI.React.Hooks.t -> Element.t * (UI.React.Hooks.nil, 'a) UI.React.Hooks.t)
      -> Element.t

    val make_component : use_dynamic_key:bool -> 'a component

    val box
      :  ?key:int
      -> 'a component
      -> Attr.t list
      -> (('a, 'a) UI.React.Hooks.t -> Element.t list * (UI.React.Hooks.nil, 'a) UI.React.Hooks.t)
      -> Element.t
  end
end =
  Components

(** Constructing colors. *)
module Color = Color

(** Predefined colors. *)
module Colors = Colors

module Keyboard = Keyboard
module Log = Log

(** Parameter types passed to UI event handlers. *)
module Node_events = Node_events

(** For direct [Incremental] usage. *)
module Incr = struct
  include Import.Incr (** @inline *)
end

(** Mirror of [Revery_UI] for advanced uses *)
module UI = UI
