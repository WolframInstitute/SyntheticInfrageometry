---
Template: Symbol
Name: InfraCircle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraCircle
Keywords: [circle, cycle, level surface, wrapper, closure]
SeeAlso: [FindInfraCircle, InfraShell, InfraEllipse, InfraPolygon, InfraString]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraCircle]()[{*cycle*}]</code> is a circle given by one cycle.

<code>[InfraCircle]()[{*cycle1*, …, *cyclek*}]</code> is a circle given by several.

## Details & Options

Definition: an infra-circle is a simple cycle lying in a level surface of the metric.

A realisation is an **open** vertex sequence. The edge from the last vertex back to the first is implicit.

So a circle of *k* vertices has *k* edges. `["Length"]` gives the edge count per realisation, as a list, and it equals the vertex count. For a path wrapper such as [InfraSegment]() the edge count is one less than the vertex count. That is the difference between a cycle wrapper and a path wrapper.

A cycle is always connected. This is why `"Connected"` is not an admissible property on [FindInfraCircle]().

[InfraShell]() is the set-shaped companion. The shell is the level surface itself. The circle is a cycle inside it. On a lattice the shell has no cycle at all, so the circle is empty until the radius is widened to a band.

[InfraSceneHighlight]() draws the multi form with sequential edges and closes it automatically. Passing an already-closed sequence is harmless: the closure is not doubled.

For homotopy the wrapper coerces to [InfraString](), a closed walk taken modulo rotation, since a circle has no base point.

Inside an [InfraScene](), `InfraCircle[center, radius]` names a circle to be solved for.

## Basic Examples

The circle of a square grid at band `{4, 5}`. Its length equals its vertex count, because the closure is implicit.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {c = First @ GraphCenter[g]},
  {circ = FindInfraCircle[g, c, {4, 5}]},
  <|"realisations" -> Length @ First @ circ,
    "length (edges)" -> circ["Length"],
    "vertices in first" -> Length @ First @ First @ circ|>]
```

Drawn on the substrate, with the centre marked.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {c = First @ GraphCenter[g]},
  InfraSceneHighlight[g,
    {FindInfraCircle[g, c, {4, 5}] -> $InfraCircleColor, InfraPoint[{c}] -> $InfraPointColor},
    "PointSizeRange" -> 15,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

## Properties and Relations

A circle lies inside its shell.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {c = First @ GraphCenter[g]},
  SubsetQ[First @ First @ FindInfraShell[g, c, {4, 5}], First @ First @ FindInfraCircle[g, c, {4, 5}]]]
```

Consecutive vertices are adjacent, and so are the last and the first.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {c = First @ GraphCenter[g]},
  {cyc = First @ First @ FindInfraCircle[g, c, {4, 5}]},
  AllTrue[Partition[Append[cyc, First[cyc]], 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]]
```
