open Camera
open Scene
open Shape
open Object
open Material
open Vec3
open Viewport
open BVH_Node

open Transform

open Texture
open IntersectionCount

(* open Ray *)
(* open Interval *)
(* open HitRecord *)

(*
+x : right
+y : down
+z : in
*)

let _three_spheres = fun () ->
    let checker = Texture.create_checker 0.32
        (Texture.create_solid (Vec3.create 0.2 0.3 0.1))
        (Texture.create_solid (Vec3.create 0.9 0.9 0.9)) in

    let material_ground = Material.create_lambertian checker in
    let material_left   = Material.create_dielectric (1.5) in
    let material_bubble = Material.create_dielectric (1. /. 1.5) in
    let material_centre = Material.create_lambertian_colour (Vec3.create 0.1 0.2 0.5)     in
    let material_right  = Material.create_metal      (Vec3.create 0.8 0.6 0.2) 1.  in

    let sphere_ground = Shape.create_sphere (Vec3.create    0. 100.5 1. ) 100.  in
    let sphere_left   = Shape.create_sphere (Vec3.create ~-.1.   0.  1.0)   0.5 in
    let sphere_bubble = Shape.create_sphere (Vec3.create ~-.1.   0.  1.0)   0.4 in
    let sphere_centre = Shape.create_sphere (Vec3.create    0.   0.  1.2)   0.5 in
    let sphere_right  = Shape.create_sphere (Vec3.create    1.   0.  1.0)   0.5 in

    let object_ground = Object.create sphere_ground material_ground in
    let object_left   = Object.create sphere_left   material_left   in
    let object_bubble = Object.create sphere_bubble material_bubble in
    let object_centre = Object.create sphere_centre material_centre in
    let object_right  = Object.create sphere_right  material_right  in

    let aspect = 16. /. 9. in

    let image_width = 400 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 70. in

    let camera_pos = Vec3.create 0. ~-.1. 0. in
    let camera_look_at = Vec3.create 0. 0. 1. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;
    camera.focus_dist <- 3.4;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let objects = [|
        object_ground;
        object_left;
        object_centre;
        object_right;
        object_bubble
    |] in

    let bvh = BVH_Node.create objects in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    scene.background <- Vec3.create 0.7 0.8 1.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let _checkered_spheres = fun () ->
    let checker = Texture.create_checker 0.32
        (Texture.create_solid (Vec3.create 0.2 0.3 0.1))
        (Texture.create_solid (Vec3.create 0.9 0.9 0.9)) in

    let material = Material.create_lambertian checker in

    let sphere_top = Shape.create_sphere (Vec3.create 0.    10. 0. ) 10. in
    let sphere_bot = Shape.create_sphere (Vec3.create 0. ~-.10. 0. ) 10. in

    let object_top = Object.create sphere_top material in
    let object_bot = Object.create sphere_bot material in

    let objects = [|
        object_top;
        object_bot;
    |] in

    let bvh = BVH_Node.create objects in

    let aspect = 16. /. 9. in

    let image_width = 400 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 20. in

    let camera_pos = Vec3.create 13. ~-.2. ~-.3. in
    let camera_look_at = Vec3.create 0. 0. 0. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;
    camera.focus_dist <- 3.4;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    scene.background <- Vec3.create 0.7 0.8 1.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let _perlin_spheres = fun () ->
    let perlin_texture = Texture.create_noise 4. in

    let material = Material.create_lambertian perlin_texture in

    let sphere_top = Shape.create_sphere (Vec3.create 0. 1000. 0. ) 1000. in
    let sphere_bot = Shape.create_sphere (Vec3.create 0. ~-.2. 0. ) 2. in

    let object_top = Object.create sphere_top material in
    let object_bot = Object.create sphere_bot material in

    let objects = [|
        object_top;
        object_bot;
    |] in

    let bvh = BVH_Node.create objects in

    let aspect = 16. /. 9. in

    let image_width = 400 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 20. in

    let camera_pos = Vec3.create 13. ~-.2. ~-.3. in
    let camera_look_at = Vec3.create 0. 0. 0. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;
    camera.focus_dist <- 3.4;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    scene.background <- Vec3.create 0.7 0.8 1.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let _quads = fun () ->
    let mat_left  = Material.create_lambertian_colour (Vec3.create 1.0 0.2 0.2) in
    let mat_back  = Material.create_lambertian_colour (Vec3.create 0.2 1.0 0.2) in
    let mat_right = Material.create_lambertian_colour (Vec3.create 0.2 0.2 1.0) in
    let mat_top   = Material.create_lambertian_colour (Vec3.create 1.0 0.5 0.0) in
    let mat_bot   = Material.create_lambertian_colour (Vec3.create 0.2 0.8 0.8) in

    let quad_left  = Shape.create_quad (Vec3.create ~-.3.    2. ~-.5.)
                                       (Vec3.create    0. ~-.4.    0.)
                                       (Vec3.create    0.    0.    4.)
    in
    let quad_back  = Shape.create_quad (Vec3.create ~-.2.    2.    0.)
                                       (Vec3.create    4.    0.    0.)
                                       (Vec3.create    0. ~-.4.    0.)
    in
    let quad_right = Shape.create_quad (Vec3.create    3.    2. ~-.5.)
                                       (Vec3.create    0. ~-.4.    0.)
                                       (Vec3.create    0.    0.    4.)
    in
    let quad_top   = Shape.create_quad (Vec3.create ~-.2. ~-.3. ~-.1.)
                                       (Vec3.create    0.    0. ~-.4.)
                                       (Vec3.create    4.    0.    0.)
    in
    let quad_bot   = Shape.create_quad (Vec3.create ~-.2.    3. ~-.5.)
                                       (Vec3.create    4.    0.    0.)
                                       (Vec3.create    0.    0.    4.)
    in

    let _object_left  = Object.create quad_left  mat_left  in
    let _object_back  = Object.create quad_back  mat_back  in
    let _object_right = Object.create quad_right mat_right in
    let _object_top   = Object.create quad_top   mat_top   in
    let _object_bot   = Object.create quad_bot   mat_bot   in

    let objects = [|
        _object_left;
        _object_back;
        _object_right;
        _object_top;
        _object_bot
    |] in

    let bvh = BVH_Node.create objects in

    let aspect = 1. in

    let image_width = 400 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 80. in

    let camera_pos = Vec3.create 0. 0. ~-.9. in
    let camera_look_at = Vec3.create 0. 0. 0. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;
    camera.focus_dist <- 3.4;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    scene.background <- Vec3.create 0.7 0.8 1.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let _simple_lights = fun () ->
    let perlin_texture = Texture.create_noise 4. in

    let mat_perlin  = Material.create_lambertian perlin_texture in
    let mat_diffuse = Material.create_diffuse_colour (Vec3.create 4. 4. 4.) in

    let sphere_ground = Shape.create_sphere (Vec3.create 0. 1000. 0. ) 1000. in
    let sphere_above = Shape.create_sphere (Vec3.create 0. ~-.2. 0. ) 2. in
    let sphere_light = Shape.create_sphere (Vec3.create 0. ~-.7. 0. ) 2. in

    let shape_diffuse = Shape.create_quad (Vec3.create 3. ~-.1. 2.) (Vec3.create 2. 0. 0.) (Vec3.create 0. ~-.2. 0.) in

    let _object_1 = Object.create sphere_ground mat_perlin in
    let _object_2 = Object.create sphere_above mat_perlin in
    let _object_3 = Object.create shape_diffuse mat_diffuse in
    let _object_4 = Object.create sphere_light mat_diffuse in

    let translate = Transform.create_translate (Vec3.create 0. ~-.1. 0.) in
    Object.add_transform _object_3 translate;


    let objects = [|
        _object_1;
        _object_2;
        _object_3;
        (* _object_4; *)
    |] in

    let bvh = BVH_Node.create objects in

    let aspect = 16. /. 9. in

    let image_width = 400 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 20. in

    let camera_pos = Vec3.create 26. ~-.3. ~-.6. in
    let camera_look_at = Vec3.create 0. ~-.2. 0. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;
    camera.focus_dist <- 3.4;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    scene.background <- Vec3.create 0. 0. 0.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let _cornell_box = fun () ->
    let mat_red   = Material.create_lambertian_colour (Vec3.create 0.65 0.05 0.05) in
    let mat_white = Material.create_lambertian_colour (Vec3.create 0.73 0.73 0.73) in
    let mat_green = Material.create_lambertian_colour (Vec3.create 0.12 0.45 0.15) in
    let mat_light = Material.create_diffuse_colour (Vec3.create 15. 15. 15.) in

    let quad_1 = Shape.create_quad (Vec3.create      0.      0.      0.)
                                   (Vec3.create      0.      0.    555.)
                                   (Vec3.create      0. ~-.555.      0.) in
    let quad_2 = Shape.create_quad (Vec3.create    555.      0.      0.)
                                   (Vec3.create      0. ~-.555.      0.)
                                   (Vec3.create      0.      0.    555.) in
    let quad_3 = Shape.create_quad (Vec3.create      0.      0.      0.)
                                   (Vec3.create    555.      0.      0.)
                                   (Vec3.create      0.      0.    555.) in
    let quad_4 = Shape.create_quad (Vec3.create    555. ~-.555.    555.)
                                   (Vec3.create ~-.555.      0.      0.)
                                   (Vec3.create      0.      0. ~-.555.) in
    let quad_5 = Shape.create_quad (Vec3.create      0.      0.    555.)
                                   (Vec3.create    555.      0.      0.)
                                   (Vec3.create      0. ~-.555.      0.) in

    let quad_light = Shape.create_quad (Vec3.create    343.~-.554.    332.)
                                       (Vec3.create ~-.130.     0.      0.)
                                       (Vec3.create      0.     0. ~-.105.) in

    let _object_1 = Object.create quad_1 mat_green in
    let _object_2 = Object.create quad_2 mat_red   in
    let _object_3 = Object.create quad_3 mat_white in
    let _object_4 = Object.create quad_4 mat_white in
    let _object_5 = Object.create quad_5 mat_white in
    let _object_6 = Object.create quad_light mat_light in


    let objects = [|
        _object_1;
        _object_2;
        _object_3;
        _object_4;
        _object_5;
        _object_6
    |] in

    let bvh = BVH_Node.create objects in

    let aspect = 1. in

    let image_width = 300 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let vfov = 40. in

    let camera_pos = Vec3.create 278. ~-.278. ~-.800. in
    let camera_look_at = Vec3.create 278. ~-.278. 0. in
    let camera = Camera.create camera_pos camera_look_at in
    camera.defocus_angle <- 0.;

    let viewport = Viewport.create_vfov_aspect_camera vfov aspect camera in

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 400;
    scene.background <- Vec3.create 0. 0. 0.;
    Scene.render_scene scene;

    IntersectionCount.print_intersections ()

let () =
    Random.self_init ();
    (* three_spheres () *)
    (* _checkered_spheres () *)
    (* _perlin_spheres () *)
    (* _quads () *)
    _simple_lights ()
    (* _cornell_box () *)
