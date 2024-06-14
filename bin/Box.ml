open Vec3
open Ray

module Box = struct
    type box = {
        min: Vec3.vec3;
        max: Vec3.vec3
    }

    let emptyBox =
        fun () -> { min = Vec3.vec3Zero; max = Vec3.vec3Zero }

    let intersection b r =
        let tx1 = (b.min.x -. r.Ray.origin.x) *. r.invDir.x in
        let tx2 = (b.max.x -. r.Ray.origin.x) *. r.invDir.x in

        let tMin = ref (Float.min tx1 tx2) in
        let tMax = ref (Float.max tx1 tx2) in

        let ty1 = (b.min.y -. r.Ray.origin.y) *. r.invDir.y in
        let ty2 = (b.max.y -. r.Ray.origin.y) *. r.invDir.y in

        tMin := Float.max !tMin (Float.min ty1 ty2);
        tMax := Float.min !tMax (Float.max ty1 ty2);

        let tz1 = (b.min.z -. r.Ray.origin.z) *. r.invDir.z in
        let tz2 = (b.max.z -. r.Ray.origin.z) *. r.invDir.z in

        tMin := Float.max !tMin (Float.min tz1 tz2);
        tMax := Float.min !tMax (Float.max tz1 tz2);

        (Float.max 0. !tMin) <= !tMax
end
