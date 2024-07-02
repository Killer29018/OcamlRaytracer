open Scene
open Shape
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
    let sphere = Shape.create_sphere (Vec3.newV 0. 0. 2.) 1. in

    let scene = Scene.create_null_definition () in
    let scene = Scene.add_object scene sphere in
    scene.name <- "output";
    scene.image_width <- 200;
    scene.image_height <- 200;
    scene.viewport_width <- 2.;
    scene.viewport_height <- 2.;
    scene.viewport_depth <- 1.;
    Scene.render_scene scene
