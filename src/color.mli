exception Color_hex_parse_exception of string

type t = Revery.Color.t

val rgba : float -> float -> float -> float -> t
val rgb : float -> float -> float -> t
val rgba_int : int -> int -> int -> int -> t
val rgb_int : int -> int -> int -> t

(** @raise Color_hex_parse_exception on invalid hex color strings *)
val hex : string -> t

val multiply_alpha : float -> t -> t
val mix : start:t -> stop:t -> amount:float -> t
val opposite : t -> t
val to_rgba : t -> float * float * float * float
val get_alpha : t -> float
val equals : t -> t -> bool
val to_string : t -> string
val to_skia : t -> Skia.Color.t
