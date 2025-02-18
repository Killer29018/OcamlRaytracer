open Vec3
open Ray
open HitRecord

open Texture

module Material = struct
    type material_lambertian = {
        texture : Texture.texture_T
    }

    type material_metal = {
        albedo: Vec3.vec3;
        fuzz: float;
    }

    type material_dielectric = {
        refraction_index: float;
    }

    type material_diffuse = {
        texture : Texture.texture_T
    }

    type material_T = None
                    | Lambertian of material_lambertian
                    | Metal of material_metal
                    | Dielectric of material_dielectric
                    | Diffuse of material_diffuse

    exception MaterialError of string

    let create_null =
        fun () -> None

    let create_lambertian texture =
        Lambertian { texture = texture }
    let create_lambertian_colour albedo =
        Lambertian { texture = Texture.create_solid albedo }

    let create_metal albedo fuzz =
        Metal { albedo = albedo; fuzz = fuzz }

    let create_dielectric refraction =
        Dielectric { refraction_index = refraction }

    let create_diffuse texture =
        Diffuse { texture = texture }
    let create_diffuse_colour emit =
        Diffuse { texture = Texture.create_solid emit }

    let scatter_lambertian (lambertian : material_lambertian) (hit : HitRecord.hit_record) =
        let scatter_direction = Vec3.add hit.normal (Vec3.random_unit ()) in
        let scatter_direction =
            if Vec3.near_zero scatter_direction then
                hit.normal
            else
                scatter_direction
        in
        let scattered_ray = Ray.create hit.pos scatter_direction in
        let colour = Texture.get_colour lambertian.texture hit.uv hit.pos in
        Some (colour, scattered_ray)

    let scatter_metal (metal : material_metal) (ray : Ray.ray) (hit : HitRecord.hit_record) =
        let reflected = Vec3.reflect (ray.direction) hit.normal in
        let reflected = Vec3.add (Vec3.norm reflected) (Vec3.scalar (Vec3.random_unit ()) metal.fuzz) in
        let scattered_ray = Ray.create hit.pos reflected in
        Some (metal.albedo, scattered_ray)

    let reflectance cosine refraction_index =
        let r0 = (1. /. refraction_index) /. (1. +. refraction_index) in
        let r0 = r0 *. r0 in
        r0 +. (1. -. r0) *. (Float.pow (1. -. cosine) 5.)

    let scatter_dielectric (dielectric : material_dielectric) (ray : Ray.ray) (hit : HitRecord.hit_record) =
        let colour = Vec3.one in
        let ri =
            if hit.is_front_face then
                1. /. dielectric.refraction_index
            else
                dielectric.refraction_index
        in
        let unit_direction = Vec3.norm ray.direction in
        let cos_theta = min (Vec3.dot (Vec3.negate unit_direction) hit.normal) 1. in
        let sin_theta = sqrt (1. -. cos_theta *. cos_theta) in
        let cannot_refract = (ri *. sin_theta) > 1. in

        let direction =
            if cannot_refract || (reflectance cos_theta ri) > Random.float 1. then
                Vec3.reflect unit_direction hit.normal
            else
                Vec3.refract unit_direction hit.normal ri
        in

        let scattered_ray = Ray.create hit.pos direction in
        Some (colour, scattered_ray)

    let scatter mat ray hit =
        match mat with
        (* | None -> Option.None *)
        | Lambertian l -> scatter_lambertian l hit
        | Metal m -> scatter_metal m ray hit
        | Dielectric d -> scatter_dielectric d ray hit
        | Diffuse _ -> None
        | _ -> raise (MaterialError "No Material Scatter defined")

    let emitted mat uv point =
        match mat with
        | Lambertian _
        | Metal _
        | Dielectric _ -> Vec3.zero
        | Diffuse d -> Texture.get_colour d.texture uv point
        | _ -> raise (MaterialError "No material emitted defined")
end
