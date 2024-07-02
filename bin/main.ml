open Scene
open Shape
open Object
open Material
open Vec3

(*
    Shapes
        Collision
        HitRecord

    Materials
        newRay
        hitColour


    Objects <- Shapes <- Materials
    Object manager
        getRayColour
*)

(*
+x : right
+y : down
+z : in
*)

(*
let i = Image.generate_vec3_image 20 20;;

let checkerboard _p x y =
    if (x + y) mod 2 == 0 then
        Vec3.newV 1. 1. 1.
    else
        Vec3.newV 0. 1. 1.
*)

let () =
    let mat1 = Material.create_lambertian (Vec3.newV 1. 0.1 0.1) in
    let mat2 = Material.create_lambertian (Vec3.newV 0.1 1. 0.1) in

    let ground_material = Material.create_lambertian (Vec3.newV 0.3 0.8 0.1) in

    let sphere1 = Shape.create_sphere (Vec3.newV ~-.1.5 0. 2.) 1. in
    let sphere2 = Shape.create_sphere (Vec3.newV 1.5 0. 2.) 1. in
    let sphere_ground = Shape.create_sphere (Vec3.newV 0. 100.5 0.) 100. in

    let obj1 = Object.create sphere1 mat1 in
    let obj2 = Object.create sphere2 mat2 in
    let obj3 = Object.create sphere_ground ground_material in

    let scene = Scene.create_null_definition_with_objects [ obj1; obj2; obj3 ] in
    scene.name <- "output";
    scene.image_width <- 200;
    scene.image_height <- 200;
    scene.viewport_width <- 2.;
    scene.viewport_height <- 2.;
    scene.viewport_depth <- 0.4;
    scene.max_depth <- 200;
    Scene.render_scene scene
