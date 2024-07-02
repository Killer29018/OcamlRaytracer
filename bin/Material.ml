open Vec3
open Ray
open HitRecord

module Material = struct
    type material_lambertian = {
        albedo: Vec3.vec3;
    }

    type material_T = None
                    | Lambertian of material_lambertian

    exception MaterialError of string

    let create_null =
        fun () -> None

    let create_lambertian albedo =
        Lambertian { albedo = albedo }

    let scatter_lambertian lambertian (hit : HitRecord.hit_record) =
        let scatter_direction = Vec3.add hit.normal (Vec3.random_unit ()) in
        let scatter_direction =
            if Vec3.near_zero scatter_direction then
                hit.normal
            else
                scatter_direction
        in
        let ray_position = Vec3.add hit.pos (Vec3.scalar scatter_direction 0.001) in
        let scattered_ray = Ray.create ray_position scatter_direction in
        Some (lambertian.albedo, scattered_ray)

    let scatter mat _ray hit =
        match mat with
        (* | None -> Option.None *)
        | Lambertian l -> scatter_lambertian l hit
        | _ -> raise (MaterialError "No Material Scatter defined")
end
