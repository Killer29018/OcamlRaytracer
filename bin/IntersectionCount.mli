module IntersectionCount : sig
    type count = {
        mutable total: int;
        mutable sphere: int;
        mutable box: int
    }

    val increment_sphere : (unit -> unit)
    val increment_box : (unit -> unit)

    val get_total : (unit -> int)
    val get_sphere : (unit -> int)
    val get_box : (unit -> int)

    val print_intersections : (unit -> unit)
end
