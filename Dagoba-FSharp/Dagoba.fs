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
    inId: int;
    outId: int;
}

type Graph = {
    edges: Edge list;
    vertices: InternalVertex list;
    autoId: int;
    vertexMap: Map<int, InternalVertex>;
    outMap: Map<int, Edge list>;
    inMap: Map<int, Edge list>;
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
    edges: Edge list;
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
let rec replace v i l =
    match i, l with
    | 0, x::xs -> v::xs
    | i, x::xs -> x::replace v (i - 1) xs
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
    let _in = Map.find edge.inId graph.vertexMap
    let _out = Map.find edge.outId graph.vertexMap

    let listOfIn = Map.find edge.inId graph.inMap
    let updatedInMap = Map.add edge.inId (edge :: listOfIn) graph.inMap
    let listOfOut = Map.find edge.outId graph.outMap
    let updatedOutMap = Map.add edge.outId (edge :: listOfOut) graph.outMap

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

let findOutEdges (vertex: InternalVertex) (graph: Graph) =
    Map.find vertex.id graph.inMap

let findInEdges (vertex: InternalVertex) (graph: Graph) =
    Map.find vertex.id graph.outMap
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
                edges = [];
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
            | _ -> state
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
            | _ -> state

let rec genericEdgePipeline getId (findEdge: InternalVertex -> Graph -> Edge list) graph (args: obj) (gremlin: QueryStateResponse) (state: QueryState) =
    if state.response = Started then
        {state with response = Pull}
    else
        if state.edges.Length = 0  && state.response = Pull then
            match gremlin with
            | Gremlin g ->
                let edges = findEdge g.vertex graph
                if edges.Length = 0 then
                    {state with response = Pull} // 
                else
                    genericEdgePipeline getId findEdge graph args gremlin {state with edges = edges;}
            | _ -> {state with response = Pull}
        elif state.edges.Length = 0 then
            {state with response = Done}
        else
            match state.edges with
            | x::xs ->
                let vertexId = getId x
                let vertex = findVertexById vertexId graph
                match gremlin with
                | Gremlin g ->
                    {state with edges = xs; response = Gremlin {g with vertex = vertex}}
                | _ -> {state with response = Pull}
            | _ ->  {state with response = Pull}

let rec outEdgePipeline graph (args: obj) (gremlin: QueryStateResponse) (state: QueryState) =
    if state.response = Started then
        {state with response = Pull}
    else
        if state.edges.Length = 0  && state.response = Pull then
            match gremlin with
            | Gremlin g ->
                let edges = findOutEdges g.vertex graph
                if edges.Length = 0 then
                    {state with response = Pull} // 
                else
                    outEdgePipeline graph args gremlin {state with edges = edges;}
            | _ -> {state with response = Pull}
        elif state.edges.Length = 0 then
            {state with response = Done}
        else
            match state.edges with
            | x::xs ->
                let vertexId = x.outId
                let vertex = findVertexById vertexId graph
                match gremlin with
                | Gremlin g ->
                    {state with edges = xs; response = Gremlin {g with vertex = vertex}}
                | _ -> {state with response = Pull}
            | _ ->  {state with response = Pull}

let v (args:obj) (graph: Graph) =
    let q = {
        graph = graph;
        program = []; // TODO: Add vertex
    }
    q |> add vertex args

let out (args:obj) (q: Query) =
    q |> add (genericEdgePipeline (fun v -> v.outId ) findOutEdges) args

let inE (args:obj) (q: Query) =
    q |> add (genericEdgePipeline (fun v -> v.inId ) findInEdges) args

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
        let stepStateN = step.fn graph step.args q.lastResult step.state
        let stepN = {step with state = stepStateN}
        //TODO: If there is a reponse(Not pull) move the idx forward?
        let lastResultN = stepStateN.response;
        let idxN = q.idx - 1;
        let programN = replace stepN q.idx q.query.program
        let queryN = {q.query with program = programN}
        match lastResultN with
        | Pull ->
            runQuery {q with query = queryN;  idx = q.idx + 1;}
        | Done ->
            q.results
        | _ ->
            if q.idx <= 0 then
                runQuery {q with query = queryN; results = lastResultN :: q.results; lastResult = lastResultN;}
            else
                runQuery {q with query = queryN; idx = idxN; lastResult = lastResultN;}
        

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