open Vec3

module Pixels = struct
    (* let mapPixels f pixels = *)
    (*     Array.mapi (fun r rows -> Array.mapi (fun c p -> f r c p) rows) pixels *)

    let mapPixels f pixels =
        let nc = 12 in
        Parmap.array_parmapi ~ncores: 3 ~keeporder: true (fun r rows -> Parmap.array_parmapi ~ncores: nc ~keeporder: true (fun c p -> f r c p) rows) pixels

    let createPixelArraySpecific w h c =
        fun () -> Array.make_matrix h w c

    let createPixelArray w h =
        createPixelArraySpecific w h (Vec3.newVec3 0. 0. 0.)

    let printPixels x =
        let stringArray = Array.map (fun r -> Array.map (fun p -> Printf.sprintf "%s\n" (Vec3.string_of_vec3 p)) r) x in
        for row = 0 to (Array.length stringArray) - 1 do
            let rowPixels = Array.get stringArray row in
            for col = 0 to (Array.length rowPixels) - 1 do
                let col = Array.get rowPixels col in
                Printf.printf "%s" col; ()
            done
        done
end
