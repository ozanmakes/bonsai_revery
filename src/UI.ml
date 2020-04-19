module Animation = struct
  include Revery_UI.Animation (** @inline *)
end

module Spring = struct
  include Revery_UI.Spring (** @inline *)
end

module Easing = struct
  include Revery_UI.Easing (** @inline *)
end

module ImageResizeMode = struct
  include Revery_UI.ImageResizeMode (** @inline *)
end

module Style = struct
  include Revery_UI.Style (** @inline *)
end

module Transform = struct
  include Revery_UI.Transform (** @inline *)
end

module Selector = struct
  include Revery_UI.Selector (** @inline *)
end

module NodeDrawContext = struct
  include Revery_UI.NodeDrawContext (** @inline *)
end

module Keyboard = struct
  include Revery_UI.Keyboard (** @inline *)
end

module Mouse = struct
  include Revery_UI.Mouse (** @inline *)
end

module NodeEvents = struct
  include Revery_UI.NodeEvents (** @inline *)
end

module UiEvents = struct
  include Revery_UI.UiEvents (** @inline *)
end

module Container = struct
  include Revery_UI.Container (** @inline *)
end

module React = struct
  include Revery_UI.React (** @inline *)
end

module Focus = struct
  include Revery_UI.Focus (** @inline *)
end

module Dimensions = struct
  include Revery_UI.Dimensions (** @inline *)
end

module Offset = struct
  include Revery_UI.Offset (** @inline *)
end

type element = Revery_UI.element
type node = Revery_UI.node

let get_active_window = Revery.UI.getActiveWindow
