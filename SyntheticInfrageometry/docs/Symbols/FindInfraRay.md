---
Template: Symbol
Name: FindInfraRay
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraRay
Keywords: [ray, half-line, direction, pool, distance matrix, Euclid Postulate 2]
SeeAlso: [InfraRay, InfraRayQ, PencilDirections, PencilCardinality, FindInfraLine, GeodesicExtensionGraph, FindInfraSegment, SameDirectionQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraRay]()[*g*, *O*, *v*]</code> gives one ray from *O* through *v* in *g* — a geodesic from *O* containing *v* that cannot be prolonged past its last vertex — as an [InfraRay]() wrapper.

<code>[FindInfraRay]()[*g*, *O*, *v*, *n*]</code> gives exactly *n* rays or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class as a pool.

## Details & Options

A ray from *O* through *v* is a geodesic *O … v … e* with *d(O, e) = d(O, v) + d(v, e)* such that no neighbour of *e* lies one step farther from *O*. The first vertex is the origin, which is what makes it a ray rather than a line: inextensibility is required at the far end only. [InfraRayQ]() is the predicate.

Rays are how direction is expressed without a vector space. There is no tangent space on a graph, so "the direction from *O* towards *v*" is not a vector but the *family* of rays from *O* containing *v* — and like every other family here it is large. [PencilDirections]() and [PencilCardinality]() count the rays leaving a vertex, the graph's stand-in for the sphere of directions; [SameDirectionQ]() compares directions.

The pool is a single DAG with source *O*: the geodesic bundle from *O* to *v* glued at *v* to <code>[GeodesicExtensionGraph]()[*g*, {*O*, *v*}]</code>. The extension set is closed under a farther step, so the sinks of that DAG are exactly the inextensible ends and every path from *O* to a sink is a ray. There is no compatibility condition on one side, so the finder agrees with [InfraRayQ]() by construction. `All` returns <code>[InfraRay]()[{*dag*}]</code>; its `["Multiplicity"]` is the path count and `["Length"]` lists one length per ray, since rays to different sinks differ in length. A bounded count streams rays off the DAG.

<code>[FindInfraRay]()[*g*, *O*, *O*, All]</code> is every ray from *O* — the pencil. Wrapper anchors such as [InfraSet]() spread to one DAG per pair.

The longest rays are a selection on the result, `SelectInfraWalk[g, rays, All, "From" -> "MaxLength"]`.

| Option | Values | Meaning |
|---|---|---|
| `Method` | `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"`, `"RandomGreedy"` | `Automatic` resolves by the count: `All` to `"Exhaustive"`, the pool; a bounded or absent count to `"Greedy"`, which takes the branches of the DAG in candidate order, so the count-less call is deterministic. `"RandomGreedy"` takes them in random order, seeded by an ambient `SeedRandom`. The class is the same under every value; `"Pruning"` is accepted and inert, the pool having no frontier to cap. |

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 2 | To produce a finite straight-line continuously; the ray is the one-sided production. |
| Hilbert | Group II, order | Rays are defined from betweenness on a line. |
| Tarski | A4, segment construction | Laying off a segment from a point in a given direction. |
| Birkhoff | Ruler postulate | A half-open coordinate interval on a line. |

## Basic Examples

How many rays leave the centre of each substrate through a vertex at distance 5.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     FindInfraRay[g, a, b, All]["Multiplicity"]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

Three rays from the centre of each substrate. Each starts at the origin and runs to the edge of the patch; they are drawn off the pool in random order, `"RandomGreedy"` being the explicit random call, with the seed in front of it.

```wl
SeedRandom[1]; Row[Table[
   With[
     {g = InfraSubstrate[name, "Medium", "Gray", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraRay[g, a, b, UpTo[3], Method -> "RandomGreedy"] -> $InfraRayColor,
          InfraPoint[a] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]]
```

## Scope

Five rays leave vertex 6 of the grid through its neighbour 7; two stop at the corner 4 after three steps, three reach the corner 16 after four. The pool is one DAG.

```wl
With[
  {rays = FindInfraRay[GridGraph[{4, 4}], 6, 7, All]},
  {rays["Multiplicity"], rays["Length"], rays["Realizations"]}]
```

The count-less call is one ray, the same one every time.

```wl
FindInfraRay[GridGraph[{4, 4}], 6, 7]
```

Both geodesics from 1 to its antipode on the 6-cycle are rays: nothing lies farther from 1 than 4.

```wl
FindInfraRay[CycleGraph[6], 1, 4, All]["Realizations"]
```

A multi-vertex anchor spreads over the origins.

```wl
FindInfraRay[CycleGraph[6], InfraSet[{1, 2}], 4, All]["Realizations"]
```

## Properties and Relations

Every ray satisfies [InfraRayQ](); the predicate accepts the pool.

```wl
InfraRayQ[GridGraph[{4, 4}], FindInfraRay[GridGraph[{4, 4}], 6, 7, All]]
```

Every ray begins at its origin, and the distance from the origin grows by one at each step.

```wl
With[
  {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {ray = FindInfraRay[g, a, b, All]["First"]},
  {First[ray] === a, GraphDistance[g, a, #] & /@ ray === Range[0, Length[ray] - 1]}]
```

With the origin as its own direction the pool is the whole spray, and its rays are the pencil.

```wl
Sort @ FindInfraRay[CycleGraph[6], 1, 1, All]["Realizations"] === Sort @ PencilDirections[CycleGraph[6], 1]
```

The class is the same under every `Method`.

```wl
With[
  {g = GridGraph[{4, 4}]},
  SameQ @@ (Sort @ FindInfraRay[g, 6, 7, All, Method -> #]["Realizations"] & /@
     {"Exhaustive", "Greedy", "RandomGreedy"})]
```
