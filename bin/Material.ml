open Vec3
open HitRecord
open Ray

module Material = struct
    exception MaterialError

    type materialGeneral = {
        albedo : Vec3.vec3
    }

    (* type materialLambertian = { *)
    (*     (* albedo : Vec3.vec3 *) *)
    (* } *)

    type materialType =
        | None
        | Lambertian of materialGeneral
        | Metal

    let scatter material _ray (hit : HitRecord.hitRecord) :  (Vec3.vec3 * Ray.ray) option =
        match material with
        | None -> None
        | Lambertian g ->
            let scatterDirection_T = Vec3.add hit.normal (Vec3.randomUnitVec3 ()) in
            let scatterDirection =
                if Vec3.nearZero scatterDirection_T then
                    hit.normal
                else
                    scatterDirection_T in
            let scattered =
                Ray.newRay (Vec3.add hit.pos (Vec3.scalar scatterDirection 0.001)) scatterDirection in
            Some (g.albedo, scattered)
        | Metal -> raise MaterialError
end
