open AABB
open Object
open HitRecord
open Interval
open Shape

module BVH_Node = struct
    type bvh_node = {
        aabb: AABB.aabb_T;
        left: node;
        right: node
    }
    and node = Object of Object.object_T
             | Node of bvh_node
             | None

    let create_null =
        fun () -> {
            aabb = AABB.create_null ();
            left = None;
            right = None;
        }

    let rec create_span (xs : Object.object_T array) start stop =
        let comparator =
            fun a b ->
                let aa = Object.get_bounding_box a in
                let ab = Object.get_bounding_box b in
                AABB.compare_x aa ab
        in
        let span = stop - start in

        let (left, right) =
            if span = 1 then
                (Object (Array.get xs 0), Object (Array.get xs 0))
            else if span == 2 then
                (Object (Array.get xs 0), Object (Array.get xs 1))
            else
                let new_array = Array.sub xs start span in
                Array.sort comparator new_array;
                let mid = start + span / 2 in
                let left = create_span new_array start mid in
                let right = create_span new_array mid stop in
                (Node left, Node right)
        in
        let a1 =
            match left with
            | Object o -> Object.get_bounding_box o
            | Node n -> n.aabb
            | None -> AABB.create_null ()
        in
        let a2 =
            match right with
            | Object o -> Object.get_bounding_box o
            | Node n -> n.aabb
            | None -> AABB.create_null ()
        in
        { aabb = AABB.create_aabb a1 a2; left = left; right = right }

    let create xs =
        create_span xs 0 (Array.length xs)

    let rec check_collision r bvh (interval : Interval.interval_T) =
        (* let _ = 1 in Printf.printf "%s\n" (AABB.to_string bvh.aabb); *)
        if not (AABB.hit bvh.aabb r interval) then
            (* let _ = 1 in Printf.printf "Miss AABB\n"; *)
            HitRecord.Miss
        else
            (* let _ = 1 in Printf.printf "Hit AABB\n"; *)
            let left =
                match bvh.left with
                | Object o -> Object.check_collision o r interval
                | Node n -> check_collision r n interval
                | None -> HitRecord.Miss
            in
            let new_interval =
                match left with
                | HitRecord.Miss -> interval
                | HitRecord.Hit h1 -> Interval.create interval.min h1.t
            in
            let right =
                match bvh.right with
                | Object o -> Object.check_collision o r new_interval
                | Node n -> check_collision r n new_interval
                | None -> HitRecord.Miss
            in
            HitRecord.closest_hit left right

    let rec to_string b =
        let left = Printf.sprintf "LEFT  [%s]"
            (match b.left with
             | None -> ""
             | Node n -> to_string n
             | Object o -> Shape.string_of_shape o.shape
            ) in

        let right = Printf.sprintf "RIGHT [%s]"
            (match b.right with
             | None -> ""
             | Node n -> to_string n
             | Object o -> Shape.string_of_shape o.shape
            ) in
        Printf.sprintf "NODE {[%s] [%s]}" left right
end
