open NUnit.Framework
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
        |> addEdge {label = "parent"; _in = 2; _out = 1; }
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
        |> addEdges [{label = "parent"; _in = 2; _out = 1; }; {label = "parent"; _in = 3; _out = 2; }]
    Assert.AreEqual(4, vertexGraph.autoId)
    Assert.AreEqual(3, List.length vertexGraph.vertices)
    Assert.AreEqual(2, List.length vertexGraph.edges)
    Assert.AreEqual(1, List.length (Map.find 2 vertexGraph.inMap))
    Assert.AreEqual(0, List.length (Map.find 1 vertexGraph.inMap))
    Assert.AreEqual(1, List.length (Map.find 2 vertexGraph.outMap))
    Assert.AreEqual(1, List.length (Map.find 1 vertexGraph.outMap))

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