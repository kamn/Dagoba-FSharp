﻿open NUnit.Framework
open Dagoba_FSharp.Library;

[<Test>]
let ``empty Map``() = 
    Assert.AreEqual(1, emptyGraph.autoId)

[<Test>]
let ``Add vertex``() = 
    let graph = addVertex {name = "A"} emptyGraph
    Assert.AreEqual(2, graph.autoId)
    Assert.AreEqual(1, List.length graph.vertices)

[<Test>]
let ``Add vertices``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertex {name = "A"}
        |> addVertex {name = "B"}
    Assert.AreEqual(3, vertexGraph.autoId)
    Assert.AreEqual(2, List.length vertexGraph.vertices)

[<Test>]
let ``Add vertices list``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}]
    Assert.AreEqual(3, vertexGraph.autoId)
    Assert.AreEqual(2, List.length vertexGraph.vertices)

[<Test>]
let ``Add edge``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertex {name = "A"}
        |> addVertex {name = "B"}
        |> addEdge {label = "parent"; inId = 2; outId = 1; }
    Assert.AreEqual(3, vertexGraph.autoId)
    Assert.AreEqual(2, List.length vertexGraph.vertices)
    Assert.AreEqual(1, List.length vertexGraph.edges)
    Assert.AreEqual(1, List.length (Map.find 2 vertexGraph.inMap))
    Assert.AreEqual(0, List.length (Map.find 1 vertexGraph.inMap))
    Assert.AreEqual(0, List.length (Map.find 2 vertexGraph.outMap))
    Assert.AreEqual(1, List.length (Map.find 1 vertexGraph.outMap))

[<Test>]
let ``Add edge list``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
        |> addEdges [{label = "parent"; inId = 2; outId = 1; }; {label = "parent"; inId = 3; outId = 2; }]
    Assert.AreEqual(4, vertexGraph.autoId)
    Assert.AreEqual(3, List.length vertexGraph.vertices)
    Assert.AreEqual(2, List.length vertexGraph.edges)
    Assert.AreEqual(1, List.length (Map.find 2 vertexGraph.inMap))
    Assert.AreEqual(0, List.length (Map.find 1 vertexGraph.inMap))
    Assert.AreEqual(1, List.length (Map.find 2 vertexGraph.outMap))
    Assert.AreEqual(1, List.length (Map.find 1 vertexGraph.outMap))


let ``Find out edge list``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
        |> addEdges [{label = "parent"; inId = 2; outId = 1; }; {label = "parent"; inId = 3; outId = 2; }]
    Assert.AreEqual(4, vertexGraph.autoId)
    let outEdges = findInEdges (findVertexById 1 vertexGraph) vertexGraph
    Assert.AreEqual(1, List.length outEdges)
    Assert.AreEqual({label = "parent"; inId = 2; outId = 1; }, outEdges.[0])

let ``Find in edge list``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
        |> addEdges [{label = "parent"; inId = 2; outId = 1; }; {label = "parent"; inId = 3; outId = 2; }]
    Assert.AreEqual(4, vertexGraph.autoId)
    let inEdges = findInEdges (findVertexById 2 vertexGraph) vertexGraph
    Assert.AreEqual(1, List.length inEdges)
    Assert.AreEqual({label = "parent"; inId = 2; outId = 1; }, inEdges.[0])

[<Test>]
let ``Find Vertex by id``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let vertex = findVertexById 1 vertexGraph
    Assert.AreEqual("A", vertex.name)

[<Test>]
let ``Find Vertex by ids``() = 
    let vertexGraph = 
        emptyGraph
        |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let vertices = findVertexByIds [1; 2] vertexGraph
    Assert.AreEqual("A", vertices.[0].name)
    Assert.AreEqual("B", vertices.[1].name)

[<Test>]
let ``Graph to Query``() = 
    let graph = emptyGraph |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let graphQuery = graph |> v 1
    Assert.AreEqual(graph, graphQuery.graph)

[<Test>]
let ``Is empty Query done``() = 
    let graph = emptyGraph |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let emptyQuery =  {
        graph = graph;
        program = []; // TODO: Add vertex
    }
    Assert.AreEqual(true, (isQueryDone emptyQuery))

[<Test>]
let ``Is single program Query done``() = 
    let graph = emptyGraph |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let query =  {
        graph = graph;
        program = [{
            fn = vertex;
            args = 0;
            state = {
                    vertices = [];
                    edges = [];
                    response =  Done; 
                    }
        }]; // TODO: Add vertex
    }
    Assert.AreEqual(true, (isQueryDone query))

[<Test>]
let ``Is single program Query not done``() = 
    let graph = emptyGraph |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
    let query =  {
        graph = graph;
        program = [{
            fn = vertex;
            args = 0;
            state = {
                    vertices = [];
                    edges = [];
                    response =  Pull; 
                    }
        }]; // TODO: Add vertex
    }
    Assert.AreEqual(false, (isQueryDone query))


[<Test>]
let ``Vertex pipeline one step``() = 
    let graph = emptyGraph |> addVertices [{name = "A"};]
    let query =  v 1 graph
    let queryState = {
        vertices = [];
        edges = [];
        response =  Started; 
    }
    let queryState' = vertex graph 1 Started queryState
    let maybeGremlin = queryState'.response
    let expectedGremlin = {
        vertex = {
            id = 1;
            name = "A"; 
        };
        state = {
            id=0
        };
    }
    Assert.AreEqual(Gremlin expectedGremlin, maybeGremlin)

[<Test>]
let ``Vertex pipeline two step``() = 
    let graph = emptyGraph |> addVertices [{name = "A"};]
    let query =  v 1 graph
    let queryState = {
        vertices = [];
        edges = [];
        response =  Started; 
    }
    let queryState' = vertex graph 1 Started queryState
    let queryState'' = vertex graph 1 queryState'.response queryState'
    let maybeGremlin = queryState''.response
    Assert.AreEqual(Done, maybeGremlin)


[<Test>]
let ``Basic run program``() = 
    let results =
         emptyGraph 
         |> addVertices [{name = "A"};]
         |> v 1
         |> run
    Assert.AreEqual("A", results.[0].name)

[<Test>]
let ``Basic run with out program``() = 
    let results =
         emptyGraph 
         |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
         |> addEdges [{label = "parent"; inId = 1; outId = 2; }; {label = "parent"; inId = 2; outId = 1; }]
         |> v 1
    let outResult =
        results
        |> out 1
        |> run
    Assert.AreEqual("B", outResult.[0].name)

[<Test>]
let ``Run with out program and two edges``() = 
    let results =
         emptyGraph 
         |> addVertices [{name = "A"}; {name = "B"}; {name = "C"}]
         |> addEdges [{label = "parent"; inId = 1; outId = 2; }; {label = "parent"; inId = 1; outId = 3; }]
         |> v 1
    let outResult =
        results
        |> out 1
        |> run
    Assert.AreEqual("B", outResult.[0].name)
    Assert.AreEqual("C", outResult.[1].name)

[<Test>]
let ``Basic run with inE program``() = 
    let results =
         emptyGraph 
         |> addVertices [{name = "A"}; {name = "B"};]
         |> addEdges [{label = "parent"; inId = 2; outId = 1 };]
         |> v 1
    let outResult =
        results
        |> inE 1
        |> run
    Assert.AreEqual("B", outResult.[0].name)