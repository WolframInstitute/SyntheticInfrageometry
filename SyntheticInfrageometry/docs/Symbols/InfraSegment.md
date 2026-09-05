---
Template: Symbol
Name: InfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSegment
Keywords: [segment, geodesic bundle, interval DAG, multiplicity, wrapper]
SeeAlso: [FindInfraSegment, ExtendInfraSegment, InfraPoint, InfraLine, InfraMeasure, MetricInterval]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraSegment]()[{*path1*, …, *pathk*}]</code> is a segment given by a set of geodesics.

<code>[InfraSegment]()[*dag*]</code> is the same segment given by its geodesic DAG.

<code>[InfraSegment]()[{*dag1*, …, *dagk*}]</code> is the pool form: one geodesic DAG per pair of ends, as returned by [ExtendInfraSegment]() and by [FindInfraSegment]() over several anchors.

## Details & Options

Definition: an infra-segment between *a* and *b* is the set of all geodesics from *a* to *b*.

It carries no weights. The geodesics are equally admissible, so nothing distinguishes them.

There are three forms of the same object.

- The **enumerated** form is a list of paths, what a bounded count produces.
- The **DAG** form is a graph: the geodesic interval. `All` produces it for one pair of endpoints.
- The **pool** form is a list of DAGs, one per pair of ends — the return of [ExtendInfraSegment]()[*g*, *seed*, *kspec*, All] and of [FindInfraSegment]() spread over wrapper anchors. A lone atom collapses to the DAG form.

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

On the pool the accessors are sized by the atoms, never by the family: `["Length"]` is one number per DAG, `["Start"]` and `["End"]` are the [InfraSet]() of sources and of sinks, `["Multiplicity"]` and `["Measure"]` sum the per-atom counts, `["Realizations", n]` enumerates lazily atom by atom, and `seg[[i]]` is the *i*-th layer of every atom weighted by occupation. A 20 × 20 grid edge has about 9 × 10⁹ lines through it in two atoms, so nothing here enumerates by default.

Endpoints are deduplicated. Every geodesic of a family shares them. [InfraPath]() keeps endpoint multiplicity instead, because walks can end anywhere.

Inside an [InfraScene](), `InfraSegment[p, q]` names a segment to be solved for.

## Basic Examples

`All` gives the interval DAG. Every geodesic is a directed path through it.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  FindInfraSegment[g, a, b, All]["Graph"]
]
```

The DAG is small where the enumeration is large.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {seg = FindInfraSegment[g, a, b, All]},
  <|"DAG vertices" -> Length @ seg["Vertices"], "length" -> seg["Length"],
    "multiplicity" -> seg["Multiplicity"], "start" -> seg["Start"], "end" -> seg["End"]|>]
```

Indexing by position gives a measured point. Position 1 is the start and carries every geodesic.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {seg = FindInfraSegment[g, a, b, All]},
  {seg[[1]], seg[[3]]}]
```

The pool form: one DAG per pair of ends. Extending an edge of the grid by one step on each side gives seven ends pairs, each carrying one geodesic here.

```wl
With[
  {pool = ExtendInfraSegment[GridGraph[{4, 4}], {6, 7}, 1, All]},
  {Length @ pool["Graph"], pool["Multiplicity"], pool["Start"], pool["End"]}]
```

## Properties and Relations

The vertices of the DAG are the metric interval between the endpoints.

```wl
With[
  {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Sort @ FindInfraSegment[g, a, b, All]["Vertices"] === Sort @ MetricInterval[g, a, b]]
```

The length of the DAG is the graph distance, and every realisation has it.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {seg = FindInfraSegment[g, a, b, All]},
  {seg["Length"] === GraphDistance[g, a, b], Union[Length[#] - 1 & /@ seg["Realizations"]] === {seg["Length"]}}]
```
