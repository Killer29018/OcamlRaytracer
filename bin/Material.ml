open Vec3
open Ray
open HitRecord

module Material = struct
    type material_lambertian = {
        albedo: Vec3.vec3;
    }

    type material_metal = {
        albedo: Vec3.vec3;
        fuzz: float;
    }

    type material_T = None
                    | Lambertian of material_lambertian
                    | Metal of material_metal

    exception MaterialError of string

    let create_null =
        fun () -> None

    let create_lambertian albedo =
        Lambertian { albedo = albedo }

    let create_metal albedo fuzz =
        Metal { albedo = albedo; fuzz = fuzz }

    let scatter_lambertian (lambertian : material_lambertian) (hit : HitRecord.hit_record) =
        let scatter_direction = Vec3.add hit.normal (Vec3.random_unit ()) in
        let scatter_direction =
            if Vec3.near_zero scatter_direction then
                hit.normal
            else
                scatter_direction
        in
        let scattered_ray = Ray.create hit.pos scatter_direction in
        Some (lambertian.albedo, scattered_ray)

    let scatter_metal (metal : material_metal) (ray : Ray.ray) (hit : HitRecord.hit_record) =
        let reflected = Vec3.reflect (ray.direction) hit.normal in
        let reflected = Vec3.add (Vec3.norm reflected) (Vec3.scalar (Vec3.random_unit ()) metal.fuzz) in
        let scattered_ray = Ray.create hit.pos reflected in
        Some (metal.albedo, scattered_ray)

    let scatter mat ray hit =
        match mat with
        (* | None -> Option.None *)
        | Lambertian l -> scatter_lambertian l hit
        | Metal m -> scatter_metal m ray hit
        | _ -> raise (MaterialError "No Material Scatter defined")
end
