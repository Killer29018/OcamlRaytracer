open Vec3
open Interval
open Ray

module AABB = struct
    type aabb_T = {
        x : Interval.interval_T;
        y : Interval.interval_T;
        z : Interval.interval_T
    }

    let empty = {
        x = Interval.empty;
        y = Interval.empty;
        z = Interval.empty
    }

    let universe = {
        x = Interval.universe;
        y = Interval.universe;
        z = Interval.universe
    }

    let create_values min_x max_x min_y max_y min_z max_z = {
            x = Interval.create min_x max_x;
            y = Interval.create min_y max_y;
            z = Interval.create min_z max_z
        }

    let create_intervals x y z = {
            x = x;
            y = y;
            z = z
        }

    let create_points a b =
        let min_point = Vec3.min_comp a b in
        let max_point = Vec3.max_comp a b in
        {
            x = Interval.create min_point.x max_point.x;
            y = Interval.create min_point.y max_point.y;
            z = Interval.create min_point.z max_point.z
        }

    let create_aabb a b =
        {
            x = Interval.expand a.x b.x;
            y = Interval.expand a.y b.y;
            z = Interval.expand a.z b.z
        }

    let hit aabb (ray : Ray.ray) (interval : Interval.interval_T) =
        let ray_origin = ray.origin in
        let ray_direction = ray.direction in

        let update_interval (ax : Interval.interval_T) origin direction (interval : Interval.interval_T) =
            let dir_inv = 1. /. direction in
            let t0 = (ax.min -. origin) *. dir_inv in
            let t1 = (ax.max -. origin) *. dir_inv in

            let new_interval =
                if t0 < t1 then
                    Interval.create
                        (if t0 > interval.min then t0 else interval.min)
                        (if t1 < interval.max then t1 else interval.max)
                else
                    Interval.create
                        (if t1 > interval.min then t1 else interval.min)
                        (if t0 < interval.max then t0 else interval.max)
            in
            (new_interval.max >= new_interval.min, new_interval)
        in

        let (h1, int) = update_interval aabb.x ray_origin.x ray_direction.x interval in
        let (h2, int) = update_interval aabb.y ray_origin.y ray_direction.y int in
        let (h3, _)   = update_interval aabb.z ray_origin.z ray_direction.z int in

        h1 && h2 && h3

        let to_string aabb =
            Printf.sprintf "X: %s | Y: %s | Z: %s"
                (Interval.to_string aabb.x)
                (Interval.to_string aabb.y)
                (Interval.to_string aabb.z)

    let compare (int1 : Interval.interval_T) (int2 : Interval.interval_T) =
        if int1.min < int2.min then ~-1
        else if int1.min = int2.min then
            if int1.max > int2.max then
                1
            else
                0
        else
            1

    let compare_x a b = compare a.x b.x

    let compare_y a b = compare a.y b.y

    let compare_z a b = compare a.z b.z

end
