open Viewport
open Camera
open Image
open Vec3
open Object
open HitRecord
open Material
open Ray
open Interval
open BVH_Node

module Scene = struct
    type scene_definition = {
        mutable name: string;
        mutable bvh: BVH_Node.bvh_node;
        objects: Object.object_T array;

        mutable image_width: int;
        mutable image_height: int;

        mutable viewport: Viewport.viewport_T;

        mutable camera: Camera.camera_T;

        mutable max_depth: int;
        mutable sample_count: int;

        mutable background: Vec3.vec3;
    }

    let create_null_definition =
        fun () -> {
            name = "";
            bvh = BVH_Node.create_null ();
            objects = [||];
            image_width = 0;
            image_height = 0;
            viewport = Viewport.create_null ();
            camera = Camera.create_null ();
            max_depth = 1;
            sample_count = 1;
            background = Vec3.zero;
        }

    let create_null_definition_with_bvh bvh objects =
        {
            name = "";
            bvh = bvh;
            objects = objects;
            image_width = 0;
            image_height = 0;
            viewport = Viewport.create_null ();
            camera = Camera.create_null ();
            max_depth = 1;
            sample_count = 1;
            background = Vec3.zero;
        }

    let miss_colour scene =
        scene.background

    let rec calculate_colour scene ray depth =
        if depth <= 0 then
            Vec3.zero
        else
            let closest = BVH_Node.check_collision ray scene.bvh Interval.zero_infinite in
            match closest with
            | HitRecord.Miss ->
                miss_colour scene
            | HitRecord.Hit h ->
                let obj =
                    List.hd (List.filter (fun (a : Object.object_T) -> a.id = h.id) (Array.to_list scene.objects)) in

                let color_from_emission = Material.emitted obj.material h.uv h.pos in

                let result = Object.scatter_ray obj ray h in
                match result with
                | Some (c, r) ->
                    Vec3.add
                        color_from_emission
                        (Vec3.comp_mul c (calculate_colour scene r (depth - 1)))
                | None ->
                    color_from_emission


    let per_pixel x y scene origin top_left right_off down_off defocus_radius =
        let right = Vec3.scalar right_off (float_of_int x) in
        let down = Vec3.scalar down_off (float_of_int y) in

        let defocus_disk_right = Vec3.scalar scene.camera.right defocus_radius in
        let defocus_disk_up    = Vec3.scalar scene.camera.up    defocus_radius in

        let defocus_disk_sample =
            fun () ->
                let p = Vec3.random_in_unit_disk () in
                Vec3.add_list [ origin; (Vec3.scalar defocus_disk_right p.x); (Vec3.scalar defocus_disk_up p.y) ]
        in

        let generate_ray _ =
            let scalar = Vec3.random_bounds ~-.0.5 0.5 in
            let off_x = Vec3.scalar right_off scalar.x in
            let off_y = Vec3.scalar down_off scalar.y in
            let pos = Vec3.add_list [ top_left; right; down; off_x; off_y ] in
            let ray_origin =
                if scene.camera.defocus_angle <= 0. then
                    origin
                else
                    defocus_disk_sample ()
            in
            Ray.create ray_origin (Vec3.sub pos ray_origin)
        in

        let sample_power = 1. /. (float_of_int scene.sample_count) in
        let rays = Array.init scene.sample_count generate_ray in
        let colours = Array.map (fun r -> calculate_colour scene r scene.max_depth) rays in
        let final_colour = Array.fold_left (fun acc r -> Vec3.add acc (Vec3.scalar r sample_power)) Vec3.zero colours in
        final_colour

    let render_scene scene =
        let origin = scene.camera.pos in
        let image = Image.generate_vec3_image scene.image_width scene.image_height in
        let (top_left, right, down) = Viewport.get_components scene.viewport image scene.camera in
        let defocus_radius = Camera.get_defocus_radius scene.camera in
        let output_image = Image.mapi (fun _c x y -> per_pixel x y scene origin top_left right down defocus_radius) image in

        Image.pixel_image_to_file scene.name (Image.pixel_image_of_vec3_image_gamma output_image)
end
