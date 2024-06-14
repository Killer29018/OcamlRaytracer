open Vec3
open Ray
open HitRecord
open Material
open Box

module Shape = struct
    type shapeGeneral = {
        mat : Material.materialType;
        box: Box.box
    }

    type sphere = {
        centre: Vec3.vec3;
        radius: float
    }

    type triangle = {
        a: Vec3.vec3;
        b: Vec3.vec3;
        c: Vec3.vec3;
        normal: Vec3.vec3
    }

    type shapeT = None
                | Sphere of sphere
                | Triangle of triangle

    type shape = shapeGeneral * shapeT

    let createSphere c r mat =
        let general = {
            mat = mat;
            box = {
                min = Vec3.newVec3 (c.Vec3.x -. r) (c.Vec3.y -. r) (c.Vec3.z -. r);
                max = Vec3.newVec3 (c.x +. r) (c.y +. r) (c.z +. r)
            }
        } in
        (general, Sphere { centre = c; radius = r })

    let createTriangle a b c n mat =
        let general = {
            mat = mat;
            box = {
                min = Vec3.minComp3 a b c;
                max = Vec3.maxComp3 a b c
            }
        } in
        (general, Triangle { a = a; b = b; c = c; normal = n })

    exception ShapeError

    let nullShapeGeneral =
        fun () -> { mat = Material.None; box = Box.emptyBox () }

    let shapeNormal pos = function
        | Sphere sphere ->
                Vec3.norm (Vec3.sub pos sphere.centre)
        | Triangle t ->
                t.normal
        | _ -> raise ShapeError

    let shapeCollision ray s =
        let (g, shape) = s in
        let hit = HitRecord.nullHitRecord () in
        if not (Box.intersection g.box ray) then
            HitRecord.Miss
        else
            match shape with
                | Sphere sphere ->
                    let o = ray.Ray.origin in
                    let d = ray.Ray.direction in
                    let diff = Vec3.sub o sphere.centre in
                    let a = Vec3.dot d d in
                    let b = Vec3.dot d diff in
                    let c = (Vec3.mag_squared diff) -. (sphere.radius *. sphere.radius) in
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
                        let hitPosition = Vec3.add o (Vec3.scalar d t) in
                        let normal = shapeNormal hitPosition (Sphere sphere) in
                        begin
                            hit.t <- t;
                            hit.frontFace <- (Vec3.dot normal ray.direction) < 0.;
                            hit.normal <- if hit.frontFace then
                                              normal
                                          else
                                              Vec3.negate normal
                            ;
                            hit.pos <- hitPosition;
                        end;
                        HitRecord.Hit hit
                    else
                        HitRecord.Miss
                | Triangle tr ->
                    let dot = Vec3.dot tr.normal ray.direction in
                    if dot > ~-.Float.epsilon  && dot < Float.epsilon then
                        HitRecord.Miss
                    else
                        let edge1 = Vec3.sub tr.b tr.a in
                        let edge2 = Vec3.sub tr.c tr.a in
                        let rayCrossE2 = Vec3.cross ray.direction edge2 in
                        let det = Vec3.dot edge1 rayCrossE2 in

                        let invDet = 1. /. det in
                        let s = Vec3.sub ray.origin tr.a in
                        let u = invDet *. (Vec3.dot s rayCrossE2) in

                        let sCrossE1 = Vec3.cross s edge1 in
                        let v = invDet *. (Vec3.dot ray.direction sCrossE1) in
                        let t = invDet *. (Vec3.dot edge2 sCrossE1) in

                        if det > ~-.Float.epsilon && det < Float.epsilon then
                            HitRecord.Miss
                        else if u < 0. || u > 1. then
                            HitRecord.Miss
                        else if v < 0. then
                            HitRecord.Miss
                        else if v < 0. || (u +. v) > 1. then
                            HitRecord.Miss
                        else if t > Float.epsilon then
                            begin
                                hit.t <- t;
                                hit.frontFace <- (Vec3.dot tr.normal ray.direction) < 0.;
                                hit.normal <- tr.normal;
                                hit.pos <- Vec3.add ray.origin (Vec3.scalar ray.direction t);
                                HitRecord.Hit hit
                            end
                        else
                            HitRecord.Miss
                | _ -> raise ShapeError

    let shapeCollisions ray shapes =
        let hits = List.map (shapeCollision ray) shapes in
        let comb = List.map2 (fun x y -> (x, y)) hits shapes in
        let minHit (current,s1) (next,s2) =
            match current, next with
            | _, HitRecord.Miss -> (current, s1)
            | HitRecord.Miss, _ -> (next, s2)
            | HitRecord.Hit a, HitRecord.Hit b ->
                if a.t < b.t then
                    (HitRecord.Hit a, s1)
                else
                    (HitRecord.Hit b, s2)
        in
        List.fold_left minHit (Miss, (nullShapeGeneral (), None)) comb
end
