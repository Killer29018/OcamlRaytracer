open Vec3
open Ray
open HitRecord

module Material : sig
    type material_lambertian = {
        albedo: Vec3.vec3;
    }

    type material_metal = {
        albedo: Vec3.vec3;
        fuzz: float;
    }

    type material_dielectric = {
        refraction_index: float;
    }

    type material_T = None
                    | Lambertian of material_lambertian
                    | Metal of material_metal
                    | Dielectric of material_dielectric

    exception MaterialError of string

    val create_null : (unit -> material_T)

    val create_lambertian : Vec3.vec3 -> material_T
    val create_metal : Vec3.vec3 -> float -> material_T
    val create_dielectric : float -> material_T

    val scatter : material_T -> Ray.ray -> HitRecord.hit_record -> (Vec3.vec3 * Ray.ray) option
end
