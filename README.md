# Dagoba-FSharp

[EXPERIMENTAL]
[INCOMPLETE]

A basic and changed port of [Dagoba](https://github.com/aosabook/500lines/tree/master/dagoba) from [500 Lines or Less](http://aosabook.org/en/500L/dagoba-an-in-memory-graph-database.html).

## Purpose
The main purpose of this project was to help be gain more understanding of the 500 Lines graph database and insight into F#.
I have stopped(or at least slowed down) work on this repo since I feel I have gain some insight and benefit from porting the code.

## Differences

The differences between the original Dagoba and Dagoba F# mirror the difference between JavaScript and F#.
The code is written in a strongly-typed and immutable way because of this there are some major differences.

- Unable to add arbitrary node or edge objects
- Adding a node or edge creates a new graph DB
- Unable to dynamic add pipeline

There are two things I viewed as out of scope for this project: Serialization and Persistence.
I was interested in the implementation of the graph database only.
I might add those features in the future.

## Examples

```fsharp
let result = emptyGraph
         |> addVertices [{name = "A"};]
         |> v 1
         |> run
```
will output the first node in the DB

```fsharp
let outResult =
    graph
    |> v 1
    |> out 1
    |> run
```
will output all connected that is connected out from the first node

## Comparisons

Some comparisons to the original Dagoba and other graph DBs for fun.

```js
g.v(1).run() //Gets the first node
g.v(1).out('knows').run() // Get people who node 1 knows
```
