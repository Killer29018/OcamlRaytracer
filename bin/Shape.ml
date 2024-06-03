open Vec3
open Ray
open HitRecord

type sphereSize = float

exception ShapeError

type shape = None | Sphere of vec3 * sphereSize;;

let sphereNormal pos = function
    | Sphere (center, _) ->
            norm (sub center pos)
    | _ -> raise ShapeError

let sphereCollision ray = function
    | Sphere (center, radius) ->
            let o = ray.origin in
            let d = ray.direction in
            let diff = sub o center in
            let a = dot d d in
            let b = dot d diff in
            let c = (mag_squared diff) -. (radius *. radius) in
            let disc = sqrt (b *. b -. a *. c) in
            let t =
                if (~-.b -. disc) > 0. then
                    (~-.b -. disc) /. a
                else if (~-.b +. disc) > 0. then
                    (~-.b +. disc) /. a
                else
                    ~-.1.
            in
            if t > 0. then
                let hitPosition = add o (scalar d t) in
                newHitRecord t (sphereNormal hitPosition (Sphere (center, radius))) hitPosition
            else
                Miss
    | _ -> raise ShapeError
