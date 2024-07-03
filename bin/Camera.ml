open Vec3

module Camera = struct
    type camera_T = {
        pos: Vec3.vec3;
        look_at: Vec3.vec3;

        world_up: Vec3.vec3;
        world_front: Vec3.vec3;
        world_right: Vec3.vec3;

        up: Vec3.vec3;
        front: Vec3.vec3;
        right: Vec3.vec3;
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
            right = right
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
        } in
        calculate_directions c

    let string_of_camera c =
        Printf.sprintf "CAMERA | (%s) | (%s) | [(%s) : (%s) : (%s)] | [(%s) : (%s) : (%s)]"
            (Vec3.string_of_vec3 c.pos)
            (Vec3.string_of_vec3 c.look_at)
            (Vec3.string_of_vec3 c.world_up)
            (Vec3.string_of_vec3 c.world_front)
            (Vec3.string_of_vec3 c.world_right)
            (Vec3.string_of_vec3 c.up)
            (Vec3.string_of_vec3 c.front)
            (Vec3.string_of_vec3 c.right)
end
