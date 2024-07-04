open Vec3
open Misc

module Camera = struct
    type camera_T = {
        mutable pos: Vec3.vec3;
        mutable look_at: Vec3.vec3;

        world_up: Vec3.vec3;
        world_front: Vec3.vec3;
        world_right: Vec3.vec3;

        up: Vec3.vec3;
        front: Vec3.vec3;
        right: Vec3.vec3;

        mutable defocus_angle: float;
        mutable focus_dist: float;
    }

    let create_null =
        fun () -> {
            pos = Vec3.zero;
            look_at = Vec3.zero;

            world_up = Vec3.n_y;
            world_front = Vec3.p_z;
            world_right = Vec3.p_x;

            up = Vec3.zero;
            front = Vec3.zero;
            right = Vec3.zero;

            defocus_angle = 0.;
            focus_dist = 10.;
        }

    let calculate_directions c =
        let front = Vec3.norm (Vec3.sub c.look_at c.pos) in
        let right = Vec3.norm (Vec3.cross front c.world_up) in
        let up = Vec3.norm (Vec3.cross right front) in
        {
            pos = c.pos;
            look_at = c.look_at;
            world_up = c.world_up;
            world_front = c.world_front;
            world_right = c.world_right;
            up = up;
            front = front;
            right = right;
            defocus_angle = 0.;
            focus_dist = 10.;
        }

    let create p l =
        let c = {
            pos = p;
            look_at = l;

            world_up = Vec3.n_y;
            world_front = Vec3.p_z;
            world_right = Vec3.p_x;

            up = Vec3.zero;
            front = Vec3.zero;
            right = Vec3.zero;

            defocus_angle = 0.;
            focus_dist = 10.;
        } in
        calculate_directions c


    let create_full p l w_u w_f w_r =
        let c = {
            pos = p;
            look_at = l;

            world_up = Vec3.norm w_u;
            world_front = Vec3.norm w_f;
            world_right = Vec3.norm w_r;

            up = Vec3.zero;
            front = Vec3.zero;
            right = Vec3.zero;

            defocus_angle = 0.;
            focus_dist = 10.;
        } in
        calculate_directions c

    let get_defocus_radius c =
        c.focus_dist *. (tan (radians_of_deg (c.defocus_angle /. 2.)))

    let string_of_camera c =
        Printf.sprintf "CAMERA | (%s) | (%s) | [(%s) : (%s) : (%s)] | [(%s) : (%s) : (%s)] | %.3f %.3f"
            (Vec3.string_of_vec3 c.pos)
            (Vec3.string_of_vec3 c.look_at)
            (Vec3.string_of_vec3 c.world_up)
            (Vec3.string_of_vec3 c.world_front)
            (Vec3.string_of_vec3 c.world_right)
            (Vec3.string_of_vec3 c.up)
            (Vec3.string_of_vec3 c.front)
            (Vec3.string_of_vec3 c.right)
            c.defocus_angle
            c.focus_dist
end
