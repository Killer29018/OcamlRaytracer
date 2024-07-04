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
    (* let mat1 = Material.create_lambertian (Vec3.create 1. 0.1 0.1) in *)
    (* let mat2 = Material.create_lambertian (Vec3.create 0.1 0.1 0.9) in *)

    (* let ground_material = Material.create_lambertian (Vec3.create 0.3 0.8 0.1) in *)
    let mat = Material.create_lambertian (Vec3.create 0.8 0.8 0.8) in

    let sphere1 = Shape.create_sphere (Vec3.create 0. ~-.0.5 2.) 1. in
    (* let sphere2 = Shape.create_sphere (Vec3.create 1.5 ~-.0.5 2.) 1. in *)
    let sphere_ground = Shape.create_sphere (Vec3.create 0. 100.5 2.) 100. in

    let obj1 = Object.create sphere1 mat in
    (* let obj2 = Object.create sphere2 mat2 in *)
    let obj3 = Object.create sphere_ground mat in

    let viewport = Viewport.create_vfov_aspect 2. 1. (Float.pi /. 2.) in

    let camera = Camera.create (Vec3.create 0. 0. 0.5) (Vec3.create 0. 0. 2.) in
    Printf.printf "%s\n" (Camera.string_of_camera camera);

    let scene = Scene.create_null_definition_with_objects [ obj1; (* obj2; *) obj3 ] in
    scene.name <- "output";
    scene.image_width <- 200;
    scene.image_height <- 200;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 2;
    scene.sample_count <- 10;
    Scene.render_scene scene
