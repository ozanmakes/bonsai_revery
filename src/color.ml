type t = Revery.Color.t

exception Color_hex_parse_exception = Revery.Color.ColorHexParseException

let rgba = Revery.Color.rgba
let rgb = Revery.Color.rgb
let rgba_int = Revery.Color.rgba_int
let rgb_int = Revery.Color.rgb_int
let hex = Revery.Color.hex
let multiply_alpha = Revery.Color.multiplyAlpha
let mix = Revery.Color.mix
let opposite = Revery.Color.opposite
let to_rgba = Revery.Color.toRgba
let get_alpha = Revery.Color.getAlpha
let equals = Revery.Color.equals
let to_string = Revery.Color.toString
let to_skia = Revery.Color.toSkia
