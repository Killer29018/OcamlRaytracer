open Vec3
open Pixels
open Ray
open Shape
open Material

let viewportWidth = 2.
let aspectRatio = 16. /. 9.
let viewportHeight = viewportWidth *. (1. /. aspectRatio)
let viewportDistance = 1.8

let left = (~-.viewportWidth) /. 2.
let right = viewportWidth /. 2.
let top = viewportHeight /. 2.
let bottom = (~-.viewportHeight) /. 2.

let imageWidth = 1080
let imageHeight = int_of_float (ceil ((float_of_int imageWidth) *. (1. /. aspectRatio)))

let origin = Vec3.newVec3 0. 0. ~-.3.8
let forward = Vec3.norm (Vec3.newVec3 0. ~-.0. 1.)

let maxDepth = 100
let samples = 10

let material_ground =
    Shape.{ mat = Material.Lambertian (Material.{ albedo = Vec3.newVec3 0.8 0.8 0. }) }

let material_centre =
    Shape.{ mat = Material.Lambertian (Material.{ albedo = Vec3.newVec3 0.1 0.2 0.5 }) }

let material_left =
    Shape.{ mat = Material.Metal (Material.{ albedo = Vec3.newVec3 0.8 0.8 0.8 }, Material.{ fuzz = 0.3 }) }

let material_right =
    Shape.{ mat = Material.Metal (Material.{ albedo = Vec3.newVec3 0.8 0.6 0.2 }, Material.{ fuzz = 1. }) }

let shapes = [
    ( material_ground, Shape.Sphere { centre = Vec3.newVec3 0. ~-.100.5 ~-.1.; radius = 100.});
    ( material_centre, Shape.Sphere { centre = Vec3.newVec3 0. 0. ~-.1.2; radius = 0.5});
    ( material_left, Shape.Sphere { centre = Vec3.newVec3 ~-.1. 0. ~-.1.; radius = 0.5});
    ( material_right, Shape.Sphere { centre = Vec3.newVec3 1. 0. ~-.1.; radius = 0.5})
]

let generateRay origin xPercent yPercent =
    let hPixelSizeX = 0.5 *. viewportWidth /. (float_of_int imageWidth) in
    let hPixelSizeY = 0.5 *. viewportHeight /. (float_of_int imageHeight) in

    let x = Vec3.lerp (Vec3.newVec3 left 0. 0.) (Vec3.newVec3 right 0. 0.) xPercent in
    let y = Vec3.lerp (Vec3.newVec3 0. top 0.) (Vec3.newVec3 0. bottom 0.) yPercent in
    let z = Vec3.scalar forward viewportDistance in

    let rX = (~-.hPixelSizeX) +. Random.float (2. *. hPixelSizeX) in
    let rY = (~-.hPixelSizeY) +. Random.float (2. *. hPixelSizeY) in

    let offset = Vec3.newVec3 rX rY 0. in
    let dir = (Vec3.addM [x; y; z; offset]) in

    Ray.newRay origin (Vec3.norm dir)

let ppmHeader =
    Printf.sprintf "P3\n%d %d\n255" imageWidth imageHeight

let rec getRayColour ray currentDepth =
    if currentDepth >= maxDepth then Vec3.vec3Zero
    else
        let (hitRecord, hitShape) = Shape.shapeCollisions ray shapes in
        match hitRecord with
        | Miss ->
            let a = 0.5 *. ray.direction.y +. 1. in
            Vec3.add (Vec3.scalar (Vec3.newVec3 1. 1. 1.) (1. -. a)) (Vec3.scalar (Vec3.newVec3 0.5 0.7 1.) a)
        | Hit h ->
            let (g, _) = hitShape in
            let mat = g.mat in
            let values = Material.scatter mat ray h in
            match values with
            | Some (a, r) ->
                Vec3.compMul a (getRayColour r (currentDepth + 1))
            | None ->
                Vec3.newVec3 0. 0. 0.

let perPixel row col =
    let percentX = (float_of_int col) /. (float_of_int imageWidth) in
    let percentY = (float_of_int row) /. (float_of_int imageHeight) in
    let rays = List.init samples (fun _ -> generateRay origin percentX percentY) in
    let colours = List.map (fun x -> getRayColour x 0) rays in
    let total = List.fold_left (fun x y -> Vec3.add x y) Vec3.vec3Zero colours in
    Vec3.scalar total (1. /. (float_of_int samples))

let () =
    let pixels = Pixels.createPixelArray imageWidth imageHeight () in
    let getColour r c _ =
        perPixel r c
    in
    let checkerboard = Pixels.mapPixels getColour pixels in
    Printf.printf "%s \n" (ppmHeader);
    Pixels.printPixels checkerboard
