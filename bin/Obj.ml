open Vec3
open Vec2

open Shape

module Obj = struct
    type vertex = {
        pos: Vec3.vec3;
        normal: Vec3.vec3;
        uv: Vec2.vec2
    }

    type face = {
        vertices: vertex list;
        minBound: Vec3.vec3;
        maxBound: Vec3.vec3
    }

    let objectString = "o"
    let vertexString = "v"
    let vertexNormalString = "vn"
    let vertexUVString = "vt"
    let faceString = "f"

    exception LineLengthErr
    exception VertexParsingErr
    exception FaceParsingErr
    exception VertexCountErr

    type lineType = None
                  | Object
                  | Vertex
                  | Normal
                  | TexCoord
                  | Face


    type splitLine = {
        lineType: lineType;
        values: string list
    }

    let type_of_string x =
        if String.equal objectString x then Object
            else if String.equal vertexString x then Vertex
        else if String.equal vertexNormalString x then Normal
            else if String.equal vertexUVString x then TexCoord
        else if String.equal faceString x then Face
            else None


    let readFile filename =
        let lines = ref [] in
        let channel = open_in filename in
        try
            while true; do
                lines := (input_line channel) :: !lines
            done; !lines
        with End_of_file ->
            close_in channel;
            List.rev !lines

    let readObjIntoSplit filename =
        let lines = readFile filename in
        let splitLines = List.map (fun x -> String.split_on_char ' ' x) lines in
        let filteredLines = List.filter (fun x -> List.length x > 0) splitLines in

    let lineToSplit x =
        let filtered = List.filter (fun x -> String.length x > 0) x in
        match filtered with
        | [] -> raise LineLengthErr
        | x::xs -> { lineType = type_of_string x; values = xs }
        in
        List.map lineToSplit filteredLines


    let splitSplit splits lineType parseFun =
        let strings = List.filter (fun x -> x.lineType = lineType) splits in
        let labeled = List.mapi (fun i x -> (i + 1, x)) strings in
        List.map
            (fun (i, x) ->
                let v = parseFun x in
                    (i, v)
            )
        labeled

    let split3ArrayToVec3 x =
        match x.values with
        | x::y::z::[] -> Vec3.{ x = float_of_string x; y = float_of_string y; z = float_of_string z }
        | _ -> raise VertexParsingErr

    let split2ArrayToVec2 x =
        match x.values with
        | x::y::[] -> Vec2.{ x = float_of_string x; y = float_of_string y }
        | _ -> raise VertexParsingErr

    let parseFace x =
        let indices = String.split_on_char '/' x in
        match indices with
        | v::[] -> (int_of_string v, 0, 0)
        | v::t::[] -> (int_of_string v, int_of_string t, 0)
        | v::t::n::[] -> (int_of_string v, int_of_string t, int_of_string n)
        | _ -> raise FaceParsingErr

    let getVertices splits =
        splitSplit splits Vertex split3ArrayToVec3

    let getNormals splits =
        splitSplit splits Normal split3ArrayToVec3

    let getTexcoords splits =
        splitSplit splits TexCoord split2ArrayToVec2

    let rec getValue i = function
    | (j, x)::xs ->
        if i = j then
            x
        else getValue i xs
    | [] -> raise FaceParsingErr

    let minVertex v1 v2 v3 =
        Vec3.minComp3 v1.pos v2.pos v3.pos

    let maxVertex v1 v2 v3 =
        Vec3.maxComp3 v1.pos v2.pos v3.pos

    let createVertex (v, t, n) vertices texCoords normals =
        { pos = getValue v vertices; normal = getValue n normals; uv = getValue t texCoords }

    let getFaces splits =
        let vertices = getVertices splits in
        let texCoords = getTexcoords splits in
        let normals = getNormals splits in

        let indices = splitSplit splits Face (fun x ->
            match x.values with
            | a::b::c::[] -> (parseFace a, parseFace b, parseFace c)
            | _ -> raise FaceParsingErr
            ) in
        List.map (fun (_i, (i1, i2, i3)) ->
            let v1 = createVertex i1 vertices texCoords normals in
            let v2 = createVertex i2 vertices texCoords normals in
            let v3 = createVertex i3 vertices texCoords normals in

            { vertices = [ v1; v2; v3 ]; minBound = minVertex v1 v2 v3; maxBound = maxVertex v1 v2 v3 }
        )
        indices

    let getFaceFromObj name =
        let splits = readObjIntoSplit name in
        getFaces splits

    let getTriangles name position material =
        let faces = getFaceFromObj name in
        List.map
            (fun x ->
                match x.vertices with
                | a::b::c::[] ->
                    let general = Shape.{
                        mat = material;
                        box = {
                            min = Vec3.add x.minBound position;
                            max = Vec3.add x.maxBound position;
                        }
                    } in
                    (general, Shape.Triangle {
                        a = Vec3.add a.pos position;
                        b = Vec3.add b.pos position;
                        c = Vec3.add c.pos position;
                        normal = a.normal
                        })
                | _ -> raise VertexCountErr
            )
            faces
end
