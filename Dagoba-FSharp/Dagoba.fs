module Dagoba_FSharp.Library

//https://github.com/aosabook/500lines/blob/master/dagoba/code/dagoba.js

type InternalVertex = {
    id: int;
    name: string;
}

type Vertex = {
    name: string;
}

type Edge = {
    label: string;
    _in: int;
    _out: int;
}

type Graph = {
    edges: Edge list;
    vertices: InternalVertex list;
    autoId: int;
    vertexMap: Map<int, InternalVertex>;
    outMap: Map<int, int list>;
    inMap: Map<int, int list>;
}

type GremlinState = {
    id: int;
}


type Gremlin = {
    vertex: InternalVertex;
    state: GremlinState;
}

type QueryStateResponse =
    | Gremlin of Gremlin
    | Done
    | Pull
    | Started

type QueryState = {
    vertices: InternalVertex list;
    response: QueryStateResponse 
}


type QueryStep = {
    fn: (Graph -> obj -> QueryStateResponse -> QueryState -> QueryState);
    state: QueryState;
    args: obj;
}

type Query = {
    graph: Graph;
    program: QueryStep list;
}


type QueryRunHelper = {
    query: Query;
    idx: int;
    lastResult: QueryStateResponse;
    results: QueryStateResponse list;
}


//http://stackoverflow.com/a/2890789
let rec insert v i l =
    match i, l with
    | 0, xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | i, [] -> failwith "index out of range"

let rec remove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

let rec replace v i l =
    match i, l with
    | 0, x::xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | i, [] -> failwith "index out of range"


let emptyGraph = { edges = []; vertices = []; autoId = 1; vertexMap = Map.empty; inMap = Map.empty; outMap = Map.empty}

let addVertex (vertex: Vertex) (graph: Graph) =
    let internalVertex = {id = graph.autoId; name = vertex.name;}

    {graph with 
        autoId = graph.autoId + 1;
        vertices = internalVertex :: graph.vertices;
        vertexMap = Map.add graph.autoId internalVertex graph.vertexMap;
        outMap = Map.add graph.autoId [] graph.outMap;
        inMap = Map.add graph.autoId [] graph.outMap; }

let addEdge (edge: Edge) (graph: Graph) =
    //TODO: If not there a error is thrown
    //Or do tryFind
    let _in = Map.find edge._in graph.vertexMap
    let _out = Map.find edge._out graph.vertexMap

    let listOfIn = Map.find edge._in graph.inMap
    let updatedInMap = Map.add edge._in (edge._out :: listOfIn) graph.inMap
    let listOfOut = Map.find edge._out graph.outMap
    let updatedOutMap = Map.add edge._out (edge._in :: listOfOut) graph.outMap

    {graph with 
        edges = edge :: graph.edges;
        inMap = updatedInMap
        outMap = updatedOutMap}

let addVertices (vertices: Vertex list) (graph: Graph) =
    List.fold (fun state v -> addVertex v state) graph vertices

let addEdges (edges: Edge list) (graph: Graph) =
    List.fold (fun state v -> addEdge v state) graph edges

let findVertex (name: string) =
    ""    

let findVertexById (id: int) (graph: Graph) =
    Map.find id graph.vertexMap

let findVertexByIds (ids: int list) (graph: Graph) =
    List.map (fun value -> Map.find value graph.vertexMap) ids

// ---
// Query functions
// ---

let add (fn) (args:obj) (q: Query)=
    let step = {
    // (fun graph args state gremlin -> {vertex = {name = "A"}; state =  {id = 1};}
        fn = fn;
        args = args;
        state = {
                vertices = [];
                response = Started; 
                }
    }
    {q with program = step :: q.program}

let vertex graph (args: obj) gremlin (state: QueryState) =
    match state.response with
    | Started ->
        let data = findVertexById (args :?> int) graph
        let list = [data]
        if (List.length list) = 0 then
            {state with response = Done}
        else
            match list with
            | vertex::xs ->
                let vertices' = xs
                let gremlin = {vertex = vertex; state = {id=0}}
                {state with vertices = vertices'; response = Gremlin gremlin}
            | _ ->
                state //TODO: Pop vertex off
    | _ ->
        let list = state.vertices
        if (List.length list) = 0 then
            {state with response = Done}
        else 
            match list with
            | vertex::xs ->
                let vertices' = xs
                let gremlin = {vertex = vertex; state = {id=0}}
                {state with vertices = vertices'; response = Gremlin gremlin}
            | _ ->
                state //TODO: Pop vertex off

let v (args:obj) (graph: Graph) =
    let q = {
        graph = graph;
        program = []; // TODO: Add vertex
    }
    q |> add vertex args

let isQueryDone (q: Query) =
    q.program
    |> List.map (fun p -> p.state.response)
    |> List.map (fun r ->
        match r with
        | Done -> true
        | _ -> false)
    |> List.fold (fun a r -> a && r) true 

//TODO: Check if this is call tail optimized
let rec runQuery (q: QueryRunHelper) =
    if isQueryDone q.query then
        q.results
    else
        let graph = q.query.graph
        let step = List.item q.idx q.query.program
        let stepState' = step.fn graph step.args q.lastResult step.state
        let step' = {step with state = stepState'}
        let lastResult' = stepState'.response;
        let idx' = q.idx + 1;
        let program' = replace step' q.idx q.query.program
        let query' = {q.query with program = program'}
        if idx' >= program'.Length then
            runQuery {q with query = query'; results = lastResult' :: q.results}
        else
            runQuery {q with query = query'; idx = idx'}
        

let run (q: Query) =
    let queryRunHelper = {
        query = q;
        idx = 0;
        lastResult = Started;
        results = [];
    }

    let results = runQuery queryRunHelper
    results
    |> List.filter (fun i ->
        match i with
           | Gremlin g -> true
           | _ -> false)
    |> List.map (fun i -> 
        match i with
        | Gremlin g -> g.vertex)