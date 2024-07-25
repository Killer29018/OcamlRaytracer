open Object
open Ray
open Vec3
open Viewport
open Image
open HitRecord
module Scene = struct
    type scene_definition = {
        mutable objects: Object.object_T array;

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport: Viewport.viewport_T;

        mutable max_depth: int;
    }

    let create_null =
        fun () -> {
            objects = [||];
            image_width = 0;
            image_height = 0;
            viewport = Viewport.create_null ();
            max_depth = 1;
        }

    let add_object def o =
        def.objects <- (Array.append def.objects [| o |]);
        def

    let miss_colour _r =
        Vec3.create 1. 1. 1.

    let rec calculate_colour scene ray depth =
        if depth <= 0 then
            Vec3.zero
        else
            let collisions = Array.mapi (fun i o -> (i, Object.check_collision o ray)) scene.objects in
            let (index, closest) = Array.fold_left
                (fun (index, hit) (i, x) ->
                    match hit, x with
                    | HitRecord.Miss, _ -> (i, x)
                    | HitRecord.Hit _h, HitRecord.Miss -> (index, hit)
                    | HitRecord.Hit h1, HitRecord.Hit h2 ->
                        if h1.t < h2.t then
                            (index, hit)
                        else
                            (i, x)
                ) (-1, HitRecord.Miss) collisions in
            match closest with
                | HitRecord.Miss ->
                        miss_colour ray
                | HitRecord.Hit h ->
                    let obj = scene.objects.(index) in
                    let result = Object.scatter_ray obj ray h in
                    match result with
                    | Some (c, r) ->
                            Vec3.comp_mul c (calculate_colour scene r (depth - 1))
                    | None ->
                            Vec3.zero

    let per_pixel x y scene origin top_left right_delta down_delta =
        let right = Vec3.scalar right_delta (float_of_int x) in
        let down = Vec3.scalar down_delta (float_of_int y) in

        let ray_origin = origin in
        let target = Vec3.add top_left (Vec3.add right down) in
        let ray = Ray.create ray_origin (Vec3.sub target ray_origin) in

        let colour = calculate_colour scene ray scene.max_depth in
        colour

    let render_scene scene =
        let origin = Vec3.zero in
        let image = Image.create_vec3_image scene.image_width scene.image_height in
        let (top_left, right, down) = Viewport.get_components scene.viewport image in
        let output_image = Image.mapi
            (fun _c x y -> per_pixel x y scene origin top_left right down) image in
        Image.ppm_of_pixel_image (Image.pixel_image_of_vec3_image output_image)
end
