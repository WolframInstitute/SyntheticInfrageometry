---
Template: Symbol
Name: InfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSegment
Keywords: [segment, geodesic bundle, interval DAG, multiplicity, wrapper]
SeeAlso: [FindInfraSegment, InfraPoint, InfraLine, InfraMeasure, MetricInterval]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraSegment]()[{*path1*, …, *pathk*}]</code> is a segment given by a set of geodesics.

<code>[InfraSegment]()[*dag*]</code> is the same segment given by its geodesic DAG.

## Details & Options

Definition: an infra-segment between *a* and *b* is the set of all geodesics from *a* to *b*.

It carries no weights. The geodesics are equally admissible, so nothing distinguishes them.

There are two forms of the same object.

- The **enumerated** form is a list of paths. `All` produces it.
- The **DAG** form is a graph: the geodesic interval. It is the default return of [FindInfraSegment]().

The DAG is preferred because the number of geodesics grows fast while the DAG stays small. On a square grid, two vertices at distance 5 have 10 geodesics and a 12-vertex DAG.

A measure appears when you project. `seg[[i]]` is the *i*-th position across all realisations, returned as a measured [InfraPoint](). Its weight at a vertex is the number of geodesics passing through that vertex at that position. Position 1 is the start, so it carries the full multiplicity. This is how [FindInfraMidpoint]() gets its weights.

DAG accessors:

| Accessor | Gives |
|---|---|
| `["Graph"]` | the interval DAG itself |
| `["Vertices"]` | the vertices it covers |
| `["Length"]` | the edge count |
| `["Multiplicity"]` | how many geodesics it represents |
| `["Measure"]` | the per-vertex occupation |
| `["Start"]`, `["End"]` | the endpoints |
| `["Realizations", n]` | enumerate on demand; `UpTo[n]` and `All` also work |

Endpoints are deduplicated. Every geodesic of a family shares them. [InfraPath]() keeps endpoint multiplicity instead, because walks can end anywhere.

Inside an [InfraScene](), `InfraSegment[p, q]` names a segment to be solved for.

## Basic Examples

The default form is the interval DAG. Every geodesic is a directed path through it.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  FindInfraSegment[g, a, b]["Graph"]
]
```

The DAG is small where the enumeration is large.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {seg = FindInfraSegment[g, a, b]},
  <|"DAG vertices" -> Length @ seg["Vertices"], "length" -> seg["Length"],
    "multiplicity" -> seg["Multiplicity"], "start" -> seg["Start"], "end" -> seg["End"]|>]
```

Indexing by position gives a measured point. Position 1 is the start and carries every geodesic.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {seg = FindInfraSegment[g, a, b, All]},
  {seg[[1]], seg[[3]]}]
```

## Properties and Relations

The vertices of the DAG are the metric interval between the endpoints.

```wl
With[
  {g = InfraSubstrate["Hexagonal", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Sort @ FindInfraSegment[g, a, b]["Vertices"] === Sort @ MetricInterval[g, a, b]]
```

Every realisation has the same length: the graph distance.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Union @ FindInfraSegment[g, a, b, All]["Length"] === {GraphDistance[g, a, b]}]
```
