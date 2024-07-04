open Vec3

module Camera : sig
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

    val calculate_directions : camera_T -> camera_T

    val create_null : (unit -> camera_T)

    val create : Vec3.vec3 -> Vec3.vec3 -> camera_T

    val create_full : Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> camera_T

    val get_defocus_radius : camera_T -> float

    val string_of_camera : camera_T -> string
end
