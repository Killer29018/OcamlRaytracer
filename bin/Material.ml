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

    type materialDielectric = {
        refraction : float
    }

    type materialType =
        | None
        | Lambertian of materialGeneral
        | Metal of materialGeneral * materialMetal
        | Dielectric of materialGeneral * materialDielectric

    let nullMaterialGeneral =
        fun () -> { albedo = Vec3.vec3Zero; }

    let reflectance cosine refraction =
        let r0 = (1. -. refraction) /. (1. +. refraction) in
        let r02 = r0 *. r0 in
        r02 +. (1. -. r02) *. (Float.pow (1. -. cosine) 5.)

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
        | Dielectric (_, d) ->
            let attenuation = Vec3.vec3One in
            let ri =
                if hit.frontFace then
                    1. /. d.refraction
                else
                    d.refraction
                in

            let unitDir = Vec3.norm ray.direction in
            let cosTheta = Float.min (Vec3.dot (Vec3.negate unitDir) hit.normal) 1.0 in
            let sinTheta = sqrt (1. -. (cosTheta *. cosTheta)) in

            let cannotRefract = (ri *. sinTheta) > 1. in
            let direction =
                if cannotRefract || ((reflectance cosTheta ri) > (Random.float 1.))then
                    Vec3.reflect unitDir hit.normal
                else
                    Vec3.refract unitDir hit.normal ri
                in
            let scattered =
                Ray.newRay hit.pos direction in
            Some (attenuation, scattered)


end
