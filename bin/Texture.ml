open Vec3
open Vec2

open Perlin

module Texture = struct
    type solid_texture_data = {
        colour : Vec3.vec3;
    }

    type noise_texture_data = {
        noise : Perlin.perlin_T
    }

    type checker_texture_data = {
        inv_scale : float;
        even : texture_T;
        odd : texture_T;
    }
    and texture_T = None
                   | Solid of solid_texture_data
                   | Noise of noise_texture_data
                   | Checker of checker_texture_data

    let create_null =
        fun () -> None

    let create_solid c =
        Solid { colour = c }

    let create_noise = fun () ->
        Noise { noise = Perlin.create () }

    let create_checker scale even odd =
        Checker { inv_scale = 1. /. scale; even = even; odd = odd }

    let create_checker_colour scale even odd =
        Checker { inv_scale = 1. /. scale; even = create_solid even; odd = create_solid odd }

    let get_colour_solid data =
        data.colour

    let get_colour_noise data point =
        Vec3.scalar Vec3.one (Perlin.noise data.noise point)

    let rec get_colour_checker data (uv : Vec2.vec2) (point : Vec3.vec3) =
        let xInt = int_of_float (Float.floor (data.inv_scale *. point.x)) in
        let yInt = int_of_float (Float.floor (data.inv_scale *. point.y)) in
        let zInt = int_of_float (Float.floor (data.inv_scale *. point.z)) in

        let isEven = (xInt + yInt + zInt) mod 2 = 0 in

        if isEven then
            get_colour data.even uv point
        else
            get_colour data.odd uv point

    and get_colour texture uv point =
        match texture with
        | None -> Vec3.zero
        | Solid s -> get_colour_solid s
        | Noise n -> get_colour_noise n point
        | Checker c -> get_colour_checker c uv point
end
