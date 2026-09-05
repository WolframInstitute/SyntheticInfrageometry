---
Template: Symbol
Name: GeodesicExtensionGraph
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/GeodesicExtensionGraph
Keywords: [geodesic extension, DAG, spray, line pool, ray pool, distance matrix]
SeeAlso: [GeodesicSprayGraph, FindInfraLine, FindInfraRay, ExtendInfraSegment, FindInfraSegment, InfraSet]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[GeodesicExtensionGraph]()[*g*, {*p1*, *p2*}]</code> gives the DAG of all geodesic extensions of the segment *p1* → *p2* beyond *p2*.

## Details & Options

The vertex set is {*e* : *d(p1, e) = d(p1, p2) + d(p2, e)*} — the vertices that some geodesic from *p1* through *p2* reaches beyond *p2* — and the edges are the edges of *g* along increasing distance from *p1*. The source is *p2*.

The set is closed under such a step: if *u* is in it and *v* is a neighbour of *u* one step farther from *p1*, then *v* is in it too. So the directed paths from *p2* are exactly the geodesics from *p2* that stay geodesic behind any *p1* → *p2* geodesic, and the **sinks** are the vertices past which no such geodesic can be prolonged — the ends of the rays from *p1* through *p2*.

This is the shared engine of the distance-matrix family. The ray pool of [FindInfraRay]() is the geodesic bundle from *p1* to *p2* glued at *p2* to this DAG. The two sides of a line through *p1* and *p2* are <code>[GeodesicExtensionGraph]()[*g*, {*p2*, *p1*}]</code> and <code>[GeodesicExtensionGraph]()[*g*, {*p1*, *p2*}]</code>, and [FindInfraLine]() and [ExtendInfraSegment]() pick their ends from these two, subject to joint geodesicity.

With *p1* = *p2* the condition is empty and the result is the whole spray, <code>[GeodesicSprayGraph]()[*g*, *p1*]</code>.

Anchors may be wrappers — [InfraSet](), [InfraPoint](), a bundle — and spread to one DAG per pair of anchor vertices, returned as a list.

The graph keeps the embedding coordinates of *g*, so it draws in place.

## Basic Examples

Beyond the corner edge 1 → 2 of a 3 × 3 grid: every vertex not in the first column, with edges away from the corner.

```wl
GeodesicExtensionGraph[GridGraph[{3, 3}], {1, 2}]
```

Its vertex set is the set of vertices reached by a geodesic from 1 through 2.

```wl
Sort @ VertexList @ GeodesicExtensionGraph[GridGraph[{3, 3}], {1, 2}]
```

Beyond the diagonal from the corner 1 through the centre 5, only the far quadrant remains, and the single sink is the opposite corner.

```wl
With[
  {h = GeodesicExtensionGraph[GridGraph[{3, 3}], {1, 5}]},
  {Sort @ VertexList[h], Select[VertexList[h], VertexOutDegree[h, #] == 0 &]}]
```

## Scope

Wrapper anchors give one DAG per pair.

```wl
Length @ GeodesicExtensionGraph[GridGraph[{3, 3}], {InfraSet[{1, 3}], 5}]
```

The two sides of a line through the edge 1–2 of the 6-cycle.

```wl
{Sort @ VertexList @ GeodesicExtensionGraph[CycleGraph[6], {2, 1}],
 Sort @ VertexList @ GeodesicExtensionGraph[CycleGraph[6], {1, 2}]}
```

## Properties and Relations

With the anchor as its own direction the extension graph is the spray.

```wl
With[
  {g = GridGraph[{3, 3}]},
  Sort @ EdgeList @ GeodesicExtensionGraph[g, {5, 5}] === Sort @ EdgeList @ GeodesicSprayGraph[g, 5]]
```

The vertex set is closed under a farther step: every neighbour of a vertex in the graph that lies one step farther from *p1* is in the graph.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {h = GeodesicExtensionGraph[g, {1, 6}]},
  AllTrue[VertexList[h],
    v |-> AllTrue[
      Select[AdjacencyList[g, v], GraphDistance[g, 1, #] == GraphDistance[g, 1, v] + 1 &],
      MemberQ[VertexList[h], #] &]]]
```

The sinks are the ends of the rays from *p1* through *p2*.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {h = GeodesicExtensionGraph[g, {6, 7}]},
  Sort @ Select[VertexList[h], VertexOutDegree[h, #] == 0 &] ===
    Sort @ DeleteDuplicates[Last /@ FindInfraRay[g, 6, 7, All]["Realizations"]]]
```
