module Dagoba_FSharp.Library

//https://github.com/aosabook/500lines/blob/master/dagoba/code/dagoba.js

type Vertex = {
    name: string;
}

type InternalVertex = {
    id: int;
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
    vertex: Vertex;
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
    fn: (Graph -> obj -> Gremlin -> QueryState -> QueryState);
    state: QueryState;
    args: obj;
}

type Query = {
    graph: Graph;
    state: QueryState;
    program: QueryStep list
}

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

let run (q: Query) =
    q

let progressQuery (q: Query) =
    q

let add (fn) (args:obj) (q: Query)=
    let step = {
    // (fun graph args state gremlin -> {vertex = {name = "A"}; state =  {id = 1};}
        fn = fn;
        args = args;
        state = {
                vertices = [];
                response =  Pull; 
                }
    }
    {q with program = step :: q.program}

let vertex graph (args: obj) gremlin (state: QueryState) =
    match state.response with
    | Started ->
        let data = findVertexById (args :?> int)
        if (List.length state.vertices) = 0 then
            {state with response = Done}
        else 
            state
    | _ -> 
        if (List.length state.vertices) = 0 then
            {state with response = Done}
        else 
            state