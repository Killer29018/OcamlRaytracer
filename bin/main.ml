open Camera
open Scene
open Shape
open Object
open Material
open Vec3
open Viewport

(*
+x : right
+y : down
+z : in
*)

let () =
    let material_ground = Material.create_lambertian (Vec3.create 0.8 0.8 0. )     in
    let material_left   = Material.create_metal      (Vec3.create 0.8 0.8 0.8) 0.3 in
    let material_centre = Material.create_lambertian (Vec3.create 0.1 0.2 0.5)     in
    let material_right  = Material.create_metal      (Vec3.create 0.8 0.6 0.2) 1.  in

    let sphere_ground = Shape.create_sphere (Vec3.create    0. 100.5 1. ) 100.  in
    let sphere_left   = Shape.create_sphere (Vec3.create ~-.1.   0.  1.2)   0.5 in
    let sphere_centre = Shape.create_sphere (Vec3.create    0.   0.  1.2)   0.5 in
    let sphere_right  = Shape.create_sphere (Vec3.create    1.   0.  1.2)   0.5 in

    let object_ground = Object.create sphere_ground material_ground in
    let _object_left   = Object.create sphere_left   material_left in
    let object_centre = Object.create sphere_centre material_centre in
    let _object_right  = Object.create sphere_right  material_right in

    let aspect = 16. /. 9. in
    let image_width = 350 in
    let image_height = int_of_float ((float_of_int image_width) /. aspect) in

    let viewport = Viewport.create_vfov_aspect 2. aspect (Float.pi /. 1.5) in

    let camera = Camera.create (Vec3.create 0. 0. 2.5) (Vec3.create 0. 0. 1.2) in
    Printf.printf "%s\n" (Camera.string_of_camera camera);

    let scene = Scene.create_null_definition_with_objects [ object_ground; (* object_left; *) object_centre; (* object_right *) ] in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 5;
    scene.sample_count <- 100;
    Scene.render_scene scene
