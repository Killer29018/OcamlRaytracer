open Vec3
open HitRecord
open Ray

module Material = struct
    exception MaterialError

    type materialGeneral = {
        albedo : Vec3.vec3
    }

    type materialMetal = {
        fuzz : float
    }

    type materialType =
        | None
        | Lambertian of materialGeneral
        | Metal of materialGeneral * materialMetal

    let scatter material ray (hit : HitRecord.hitRecord) : (Vec3.vec3 * Ray.ray) option =
        match material with
        | None -> None
        | Lambertian g ->
            let scatterDirection_T = Vec3.add hit.normal (Vec3.randomUnitVec3 ()) in
            let scatterDirection =
                if Vec3.nearZero scatterDirection_T then
                    hit.normal
                else
                    scatterDirection_T
            in
            let scattered =
                Ray.newRay (Vec3.add hit.pos (Vec3.scalar scatterDirection 0.001)) scatterDirection in
            Some (g.albedo, scattered)
        | Metal (g, m) ->
            let reflected_T = Vec3.reflect (ray.Ray.direction) hit.normal in
            let reflected = Vec3.add (Vec3.norm reflected_T) (Vec3.scalar (Vec3.randomUnitVec3 ()) m.fuzz) in
            let scattered =
                Ray.newRay (Vec3.add hit.pos (Vec3.scalar reflected 0.001)) reflected in
            if (Vec3.dot scattered.Ray.direction hit.normal) > 0. then
                Some (g.albedo, scattered)
            else
                None
end
