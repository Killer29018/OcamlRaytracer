open Viewport
open Image
open Vec3
open Shape
open HitRecord
open Ray

module Scene = struct
    type scene_definition = {
        mutable name: string;
        mutable objects: Shape.shape_T array; (* Objects *)

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport_width: float;
        mutable viewport_height: float;
        mutable viewport_depth: float;

        mutable max_depth: int;
    }

    let create_null_definition =
        fun () -> {
            name = "";
            objects = [||];
            image_width = 0;
            image_height = 0;
            viewport_width = 0.;
            viewport_height = 0.;
            viewport_depth = 0.;
            max_depth = 0;
        }

    let add_object def o =
        def.objects <- (Array.append def.objects [| o |]);
        def

    let calculate_colour scene ray depth =
        if depth > scene.max_depth then
            Vec3.zero
        else
            let collisions = Array.mapi (fun i o -> (i, Shape.check_collision ray o)) scene.objects in
            let (_index, closest) = Array.fold_left
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
            | HitRecord.Miss -> Vec3.zero
            | HitRecord.Hit _h ->
                Vec3.newV 1. 0. 0.
                (* Get new ray from obkect *)


    let per_pixel x y scene origin top_left right_off down_off =
        let right = Vec3.scalar right_off (float_of_int x) in
        let down = Vec3.scalar down_off (float_of_int y) in
        let pos = Vec3.add_list [ top_left; right; down ] in
        let ray = Ray.create origin (Vec3.sub pos origin) in
        calculate_colour scene ray 0

    let render_scene scene =
        let origin = Vec3.zero in
        let viewport = Viewport.create scene.viewport_width scene.viewport_height scene.viewport_depth in
        let image = Image.generate_vec3_image scene.image_width scene.image_height in
        let (top_left, right, down) = Viewport.get_components viewport image in
        let output_image = Image.mapi (fun _c x y -> per_pixel x y scene origin top_left right down) image in

        Image.pixel_image_to_file scene.name (Image.pixel_image_of_vec3_image output_image)
end
