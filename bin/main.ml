open Scene
open Shape
open Object
open Viewport
open Vec3
open Material
let () =
    let material_left = Material.create_lambertian (Vec3.create 1. 0. 0.) in
    let material_center = Material.create_lambertian (Vec3.create 1. 0. 1.) in
    let material_right = Material.create_lambertian (Vec3.create 0. 0. 1.) in
    let material_ground = Material.create_lambertian (Vec3.create 0.1 0.9 0.1) in

    let left_sphere = Shape.create_sphere (Vec3.create ~-.1.2 0. 2.) 0.5 in
    let center_sphere = Shape.create_sphere (Vec3.create 0. 0. 2.2) 0.5 in
    let right_sphere = Shape.create_sphere (Vec3.create 1.2 0. 2.) 0.5 in
    let ground_sphere = Shape.create_sphere (Vec3.create 0. 100.5 2.) 100. in

    let left_obj = Object.create left_sphere material_left in
    let center_obj = Object.create center_sphere material_center in
    let right_obj = Object.create right_sphere material_right in
    let ground_obj = Object.create ground_sphere material_ground in

    let viewport = Viewport.create 2. 2. 1. in

    let scene_def = Scene.create_null () in
    let scene_def = Scene.add_object scene_def left_obj in
    let scene_def = Scene.add_object scene_def center_obj in
    let scene_def = Scene.add_object scene_def right_obj in
    let scene_def = Scene.add_object scene_def ground_obj in
    scene_def.image_width <- 200;
    scene_def.image_height <- 200;
    scene_def.viewport <- viewport;
    Scene.render_scene scene_def
