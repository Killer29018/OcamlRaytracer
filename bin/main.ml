open Vec3
open Pixels
open Ray
open Shape

let viewportWidth = 2.
let aspectRatio = 1.
(* let aspectRatio = 16. /. 9. *)
let viewportHeight = viewportWidth *. (1. /. aspectRatio)

let left = ~-.viewportWidth /. 2.
let right = viewportWidth /. 2.
let top = viewportHeight /. 2.
let bottom = ~-.viewportHeight /. 2.

let imageWidth = 500
let imageHeight = int_of_float (((float_of_int imageWidth) /. viewportWidth) *. viewportHeight)

let ppmHeader =
    Printf.sprintf "P3\n%d %d\n255" imageWidth imageHeight

let () =
    let origin = newVec3 0. 0. 0. in
    let forward = norm (newVec3 0. 0. 1.) in
    (* let ray = newRay origin forward in *)
    let sphereOrigin = newVec3 0. 0. 10. in
    let sphere = Sphere (sphereOrigin, 1.) in

    let pixels = createPixelArray imageWidth imageHeight () in
    let getColour r c _ =
        let percentX = (float_of_int c) /. (float_of_int imageWidth) in
        let percentY = (float_of_int r) /. (float_of_int imageHeight) in
        let x = lerp (newVec3 left 0. 0.) (newVec3 right 0. 0.) percentX in
        let y = lerp (newVec3 0. top 0.) (newVec3 0. bottom 0.) percentY in
        let ray = newRay (add (add origin x) y) forward in
        let record = sphereCollision ray sphere in
        match record with
        | Miss -> newVec3 0. 0. 0.
        | Hit h -> vecCompMax h.normal (newVec3 0. 0. 0.)
    in
    let checkerboard = mapPixels getColour pixels in
    Printf.printf "%s \n" (ppmHeader);
    printPixels checkerboard
