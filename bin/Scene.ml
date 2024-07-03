open Viewport
open Camera
open Image
open Vec3
open Object
open HitRecord
open Ray

module Scene = struct
    type scene_definition = {
        mutable name: string;
        mutable objects: Object.object_T array;

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport: Viewport.viewport_T;

        mutable camera: Camera.camera_T;

        mutable max_depth: int;
    }

    let create_null_definition =
        fun () -> {
            name = "";
            objects = [||];
            image_width = 0;
            image_height = 0;
            viewport = Viewport.create_null ();
            camera = Camera.create_null ();
            max_depth = 1;
        }

    let create_null_definition_with_objects objs =
        {
            name = "";
            objects = Array.of_list objs;
            image_width = 0;
            image_height = 0;
            viewport = Viewport.create_null ();
            camera = Camera.create_null ();
            max_depth = 1;
        }

    let add_object def o =
        def.objects <- (Array.append def.objects [| o |]);
        def

    let miss_colour _ray =
        Vec3.newV 0.8 0.8 1.0

    let rec calculate_colour scene ray depth =
        if depth > scene.max_depth then
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
                    Vec3.comp_mul c (calculate_colour scene r (depth + 1))
                | None ->
                    Vec3.zero


    let per_pixel x y scene origin top_left right_off down_off =
        let right = Vec3.scalar right_off (float_of_int x) in
        let down = Vec3.scalar down_off (float_of_int y) in
        let pos = Vec3.add_list [ top_left; right; down ] in
        let ray = Ray.create origin (Vec3.sub pos origin) in
        calculate_colour scene ray 0

    let render_scene scene =
        let origin = Vec3.zero in
        let image = Image.generate_vec3_image scene.image_width scene.image_height in
        let (top_left, right, down) = Viewport.get_components scene.viewport image scene.camera in
        let output_image = Image.mapi (fun _c x y -> per_pixel x y scene origin top_left right down) image in

        Image.pixel_image_to_file scene.name (Image.pixel_image_of_vec3_image output_image)
end
