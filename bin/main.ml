open Vec3
open Pixels
open Ray
open Shape
open Material

open Obj

(* Parameters *)
let viewportWidth = 2.
let aspectRatio = 16. /. 9.
let viewportHeight = viewportWidth *. (1. /. aspectRatio)
let viewportDistance = 1.2

let imageWidth = 1080
let imageHeight = int_of_float (ceil ((float_of_int imageWidth) *. (1. /. aspectRatio)))

let cameraPosition = Vec3.newVec3 0. 1. ~-.8.

let lookAt = Vec3.newVec3 0. 0. 0.
let worldUp = Vec3.newVec3 0. 1. 0.

let maxDepth = 40
let samples = 40

(* Calculated *)
let windowLeft = (~-.viewportWidth) /. 2.
let windowRight = viewportWidth /. 2.
let windowTop = viewportHeight /. 2.
let windowBottom = (~-.viewportHeight) /. 2.

let cameraDirection = Vec3.sub lookAt cameraPosition

let forward = Vec3.norm cameraDirection
let right = Vec3.norm (Vec3.cross worldUp forward)
let up = Vec3.norm (Vec3.cross forward right)

let leftBound = Vec3.scalar right windowLeft
let rightBound = Vec3.scalar right windowRight
let upperBound = Vec3.scalar up windowTop
let lowerBound = Vec3.scalar up windowBottom

let material_ground =
    Material.Lambertian (Material.{ albedo = Vec3.newVec3 0.8 0.8 0. })

let material_centre =
    Material.Lambertian (Material.{ albedo = Vec3.newVec3 0.1 0.2 0.5 })

let material_left =
    Material.Dielectric (Material.nullMaterialGeneral (), Material.{ refraction = 1.5 })

let material_right =
    Material.Metal (Material.{ albedo = Vec3.newVec3 0.8 0.6 0.2 }, Material.{ fuzz = 1. })

let material_triangle =
    Material.Lambertian (Material.{ albedo = Vec3.newVec3 1. 0. 0. })

let material_ball =
    Material.Metal (Material.{ albedo = Vec3.newVec3 0. 0.8 0.8 }, Material.{ fuzz = 0.7 })

let material_monkey =
    Material.Metal (Material.{ albedo = Vec3.newVec3 0.8 0. 0.8 }, Material.{ fuzz = 0.3 })

let ball = Obj.getTriangles "Ball.obj" (Vec3.newVec3 ~-.3. 1. ~-.1.2) material_ball
let monkey = Obj.getTriangles "Monkey.obj" (Vec3.newVec3 3. 1. ~-.1.2) material_monkey

let shapes = [
    Shape.createSphere (Vec3.newVec3 0. ~-.100.5 ~-.1.) 100. material_ground;
    Shape.createSphere (Vec3.newVec3 0. 0. ~-.1.2) 0.5 material_centre;
    Shape.createSphere (Vec3.newVec3 ~-.1. 0. ~-.1.) 0.5 material_left;
    Shape.createSphere (Vec3.newVec3 1. 0. ~-.1.) 0.5 material_right;
    Shape.createTriangle (Vec3.newVec3 0. 2. 1.) (Vec3.newVec3 1. 0. 1.) (Vec3.newVec3 ~-.1. 0. 1.) (Vec3.newVec3 0. 0. ~-.1.) material_triangle
] @ ball @ monkey

let generateRay xPercent yPercent =
    let hPixelSizeX = 0.5 *. viewportWidth /. (float_of_int imageWidth) in
    let hPixelSizeY = 0.5 *. viewportHeight /. (float_of_int imageHeight) in

    let xOffset = Vec3.lerp leftBound rightBound xPercent in
    let yOffset = Vec3.lerp upperBound lowerBound yPercent in
    let depth = Vec3.scalar forward viewportDistance in

    let rX = (~-.hPixelSizeX) +. Random.float (2. *. hPixelSizeX) in
    let rY = (~-.hPixelSizeY) +. Random.float (2. *. hPixelSizeY) in

    let offset = Vec3.newVec3 rX rY 0. in
    let dir = (Vec3.addM [xOffset; yOffset; depth; offset]) in

    Ray.newRay cameraPosition (Vec3.norm dir)

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
    let rays = List.init samples (fun _ -> generateRay percentX percentY) in
    let colours = List.map (fun x -> getRayColour x 0) rays in
    let total = List.fold_left (fun x y -> Vec3.add x y) Vec3.vec3Zero colours in
    Vec3.scalar total (1. /. (float_of_int samples))

let () =
    let pixels = Pixels.createPixelArray imageWidth imageHeight () in
    let getColour r c _ =
        perPixel r c
    in
    let image = Pixels.mapPixels getColour pixels in
    Printf.printf "%s \n" (ppmHeader);
    Pixels.printPixels image
