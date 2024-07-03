open Vec3

module Camera : sig
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

    val create_null : (unit -> camera_T)

    val create : Vec3.vec3 -> Vec3.vec3 -> camera_T

    val create_full : Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> Vec3.vec3 -> camera_T

    val string_of_camera : camera_T -> string
end
