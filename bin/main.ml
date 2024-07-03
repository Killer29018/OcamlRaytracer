open Camera
open Scene
open Shape
open Object
open Material
open Vec3

(*
+x : right
+y : down
+z : in
*)

let () =
    let mat1 = Material.create_lambertian (Vec3.newV 1. 0.1 0.1) in
    let mat2 = Material.create_lambertian (Vec3.newV 0.1 1. 0.1) in

    let ground_material = Material.create_lambertian (Vec3.newV 0.3 0.8 0.1) in

    let sphere1 = Shape.create_sphere (Vec3.newV ~-.1.5 ~-.0.5 2.) 1. in
    let sphere2 = Shape.create_sphere (Vec3.newV 1.5 ~-.0.5 2.) 1. in
    let sphere_ground = Shape.create_sphere (Vec3.newV 0. 100.5 0.) 100. in

    let obj1 = Object.create sphere1 mat1 in
    let obj2 = Object.create sphere2 mat2 in
    let obj3 = Object.create sphere_ground ground_material in

    let camera = Camera.create (Vec3.newV 0. 0. ~-.1.) (Vec3.newV 0. 0. 2.) in
    Printf.printf "%s\n" (Camera.string_of_camera camera);

    let scene = Scene.create_null_definition_with_objects [ obj1; obj2; obj3 ] in
    scene.name <- "output";
    scene.image_width <- 200;
    scene.image_height <- 200;
    scene.viewport_width <- 2.;
    scene.viewport_height <- 2.;
    scene.viewport_depth <- 1.;
    scene.camera <- camera;
    scene.max_depth <- 200;
    Scene.render_scene scene
