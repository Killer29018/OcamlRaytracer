open Camera
open Scene
open Shape
open Object
open Material
open Vec3
open Viewport
open BVH_Node

(* open Ray *)
(* open Interval *)
(* open HitRecord *)

(*
+x : right
+y : down
+z : in
*)

let () =
    let material_ground = Material.create_lambertian (Vec3.create 0.8 0.8 0. )     in
    let material_left   = Material.create_dielectric (1.5) in
    let material_bubble = Material.create_dielectric (1. /. 1.5) in
    let material_centre = Material.create_lambertian (Vec3.create 0.1 0.2 0.5)     in
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

    let vfov = 20. in

    let camera_pos = Vec3.create ~-.2. ~-.2. ~-.1. in
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

    (* Printf.printf "%s\n" (BVH_Node.to_string bvh); *)
    (* let r_c = Vec3.zero in *)
    (* let r_d = Vec3.p_z in *)
    (* let r = Ray.create r_c r_d in *)
    (* let hit = BVH_Node.check_collision r bvh (Interval.zero_infinite) in *)
    (* Printf.printf "%s\n" (Ray.string_of_ray r); *)
    (* Printf.printf "%s\n" (HitRecord.string_of_hit hit); *)

    let scene = Scene.create_null_definition_with_bvh bvh objects in
    scene.name <- "output";
    scene.image_width <- image_width;
    scene.image_height <- image_height;
    scene.viewport <- viewport;
    scene.camera <- camera;
    scene.max_depth <- 50;
    scene.sample_count <- 100;
    Scene.render_scene scene
