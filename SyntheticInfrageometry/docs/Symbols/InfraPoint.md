---
Template: Symbol
Name: InfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPoint
Keywords: [point, superposition, measure, multiplicity, wrapper]
SeeAlso: [FindInfraPoint, InfraMeasure, FindInfraMidpoint, InfraSegment, InfraSet]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraPoint]()[{*v1*, …, *vk*}]</code> is a point given by a set of candidate vertices.

<code>[InfraPoint]()[<|*v* -> *m*, …|>]</code> is a measured point: each candidate carries a weight.

<code>[InfraPoint]()[{*v1*, …}, {*m1*, …}]</code> is sugar for the association form.

## Details & Options

Definition: an infra-point is a set of vertices, optionally with a weight on each.

A construction on a graph rarely has one answer. It has a family of admissible answers. `InfraPoint` holds that family as one object.

`InfraPoint` is the only wrapper that carries a measure. Its realisations are vertices, so a weight per realisation is a weight per vertex. Every other `Infra*` head is a plain set of realisations. For those, a vertex measure is a lossy projection, computed on demand by [InfraMeasure]().

Weights count. They are not probabilities put in by hand. A weight of 6 means six geodesics chose that vertex.

Weights are created at a projection. `seg[[i]]` is one. [FindInfraMidpoint]() is another. Weights on an input point do not flow through a construction: a construction reads the support and ignores the weights.

Accessors:

| Accessor | Gives |
|---|---|
| `["Support"]` | the vertices |
| `["Weights"]` | the raw counts |
| `["Mass"]` | the total of the weights |
| `["Measure"]` | the weights normalised to sum to 1 |
| `["Realizations"]` | the vertices, as for any wrapper |
| `["First"]` | the first vertex |

Graph-keyed invariants such as `["BallVolumes", g]` and `["Dimension", g]` read the support only.

Repeated vertices sum. An all-ones measure collapses to the bare support.

Inside an [InfraScene](), `InfraPoint[]`, `InfraPoint["Center"]` and `InfraPoint[origin, dist]` name a point to be solved for.

## Basic Examples

Two vertices at odd distance have no exact midpoint. The result is a measured point with six candidates.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  FindInfraMidpoint[g, a, b]]
```

The accessors read that object apart. The measure is the weights divided by the mass, so it sums to 1.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {m = FindInfraMidpoint[g, a, b]},
  <|"support" -> m["Support"], "weights" -> m["Weights"], "mass" -> m["Mass"],
    "measure total" -> Total @ Values @ m["Measure"]|>]
```

The weights are what the picture shows. Heavier candidates are drawn larger.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  InfraSceneHighlight[g,
    {FindInfraMidpoint[g, a, b] -> $InfraCircleColor, InfraPoint[{a, b}] -> $InfraPointColor},
    "PointSizeRange" -> 22,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

## Properties and Relations

An all-ones measure is the same object as the bare support.

```wl
InfraPoint[{4, 9, 16}, {1, 1, 1}] === InfraPoint[{4, 9, 16}]
```

Repeated vertices sum into the weights.

```wl
InfraPoint[{4, 9, 4, 4}]["Weights"]
```
