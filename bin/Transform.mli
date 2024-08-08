open Vec3
open Ray
open HitRecord
open AABB

module Transform : sig
    type translate_data = {
        offset: Vec3.vec3;
    }

    type transform_T = None
                     | Translate of translate_data

    val create_null : transform_T

    val create_translate : Vec3.vec3 -> transform_T

    val apply : transform_T -> Ray.ray -> Ray.ray

    val apply_aabb : transform_T -> AABB.aabb_T -> AABB.aabb_T

    val unapply : transform_T -> HitRecord.hit_record -> HitRecord.hit_record
end
