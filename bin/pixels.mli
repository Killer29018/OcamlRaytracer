open Vec3

val mapPixels : (int -> int -> 'a -> 'b) -> 'a array array -> 'b array array

val createPixelArraySpecific : int -> int -> vec3 -> (unit -> vec3 array array)

val createPixelArray : int -> int -> (unit -> vec3 array array)

val printPixels : vec3 array array -> unit
