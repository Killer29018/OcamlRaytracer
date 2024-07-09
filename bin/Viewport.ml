open Vec3
open Image
module Viewport = struct
    type viewport_T = {
        width: float;
        height: float;
        depth: float
    }

    let create_null =
        fun () -> {
            width = 0.;
            height = 0.;
            depth = 0.
        }

    let create width height depth =
        {
            width = width;
            height = height;
            depth = depth
        }

    let get_components v (i : 'a Image.image) =
        let half_width = v.width /. 2. in
        let half_height = v.height /. 2. in

        let origin = Vec3.zero in
        let front = Vec3.create 0. 0. 1. in
        let right = Vec3.create 1. 0. 0. in
        let up = Vec3.create 0. ~-.1. 0. in

        let center = Vec3.add (Vec3.scalar front v.depth) origin in
        let left = Vec3.add (Vec3.scalar (Vec3.negate right) half_width) center in

        let top_left = Vec3.add (Vec3.scalar up half_height) left in
        let delta_right = Vec3.scalar right (v.width /. (float_of_int i.width)) in
        let delta_down = Vec3.scalar (Vec3.negate up) (v.height /. (float_of_int i.height)) in

        (top_left, delta_right, delta_down)
end
