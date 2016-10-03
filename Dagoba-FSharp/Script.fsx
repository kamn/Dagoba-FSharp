// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

// Define your library scripting code here

let map1 = Map.ofList [ (1, "one"); (2, "two") ]
let map2 = map1.Add(0, "zero")
let map3 =  map2.Add(0, "zero!")