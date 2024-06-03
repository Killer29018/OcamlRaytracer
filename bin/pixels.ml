open Vec3

let mapPixels f pixels =
    Array.mapi (fun r rows -> Array.mapi (fun c p -> f r c p) rows) pixels

let createPixelArraySpecific w h c =
    fun () -> Array.make_matrix w h c

let createPixelArray w h =
    createPixelArraySpecific w h (newVec3 0. 0. 0.)

let printPixels x =
    let stringArray = Array.map (fun r -> Array.map (fun p -> Printf.sprintf "%s\n" (string_of_vec3 p)) r) x in
    for row = 0 to (Array.length stringArray) - 1 do
        let rowPixels = Array.get stringArray row in
        for col = 0 to (Array.length rowPixels) - 1 do
            let col = Array.get rowPixels col in
            Printf.printf "%s" col; ()
        done
    done
