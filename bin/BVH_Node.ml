open AABB
open Object
open HitRecord
open Interval

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
            aabb = AABB.empty;
            left = None;
            right = None;
        }

    let rec create (xs : Object.object_T array) =
        let comparator =
            fun a b ->
                let aa = Object.get_bounding_box a in
                let ab = Object.get_bounding_box b in

                AABB.compare_x aa ab
        in
        let span = Array.length xs in

        let (left, right) =
            if span = 1 then
                (Object (Array.get xs 0), None)
            else if span = 2 then
                (Object (Array.get xs 0), Object (Array.get xs 1))
            else
                let mid = span / 2 in
                Array.fast_sort comparator xs;
                let left = create (Array.sub xs 0 mid) in
                let right = create (Array.sub xs mid (span - mid)) in
                (Node left, Node right)
        in
        let a1 =
            match left with
            | Object o -> Object.get_bounding_box o
            | Node n -> n.aabb
            | None -> AABB.empty
        in
        let a2 =
            match right with
            | Object o -> Object.get_bounding_box o
            | Node n -> n.aabb
            | None -> AABB.empty
        in
        { aabb = AABB.create_aabb a1 a2; left = left; right = right }

    let rec check_collision r bvh (interval : Interval.interval_T) =
        if not (AABB.hit bvh.aabb r interval) then
            HitRecord.Miss
        else
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
             | Object o -> Object.to_string o
            ) in

        let right = Printf.sprintf "RIGHT [%s]"
            (match b.right with
             | None -> ""
             | Node n -> to_string n
             | Object o -> Object.to_string o
            ) in
        Printf.sprintf "NODE {[%s] [%s]}" left right
end
