---
Template: Symbol
Name: InfraLine
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraLine
Keywords: [line, inextensible geodesic, pool, wrapper, geodesic DAG]
SeeAlso: [FindInfraLine, InfraLineQ, InfraSegment, InfraRay, InfraEffectivePoint, InfraMeasure]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraLine]()[{*line1*, …, *linek*}]</code> is a bundle of lines, each an inextensible geodesic given as a vertex sequence.

<code>[InfraLine]()[{*dag1*, …, *dagk*}]</code> is the pool form: one geodesic DAG per pair of ends, whose source-to-sink paths are the lines with those ends.

## Details & Options

[FindInfraLine]() returns the enumerated form for a bounded count and the pool form for `All`. The two describe the same object; the pool stays small where the enumeration is astronomical — 5 242 880 lines through two vertices of a 313-vertex square-tiling patch fit in 104 DAGs.

Accessors on either form:

| Accessor | Gives |
|---|---|
| `["Realizations"]` | the lines as vertex sequences; on the pool `["Realizations", n]`, `UpTo[n]` or `All` enumerates lazily, atom by atom |
| `["Length"]` | edge counts — one per line on the enumerated form, one per DAG on the pool |
| `["Multiplicity"]` | how many lines the object holds, by the path-count DP on the pool |
| `["Measure"]` | the per-vertex occupation, the fraction of lines through each vertex |
| `["OccupationCount"]` | the same as a count |
| `["Graph"]` | the DAGs of the pool |
| `["Vertices"]` | the vertices covered |
| `["First"]` | one line |

`line[[i]]` is the *i*-th position across all lines as a measured [InfraEffectivePoint](): its weight at a vertex is the number of lines passing through that vertex at that position. On the pool this is the *i*-th layer of every DAG weighted by geodesic occupation, computed without enumeration; *i* may be negative.

Rendered like [InfraSegment]() by [InfraSceneHighlight]() — sequential-edge semantics.

Inside an [InfraScene](), <code>[InfraLine]()[*p*, *q*]</code> names a line to be solved for.

## Basic Examples

`All` gives the pool. Two DAGs carry the twelve lines through an interior edge of the grid.

```wl
With[
  {lines = FindInfraLine[GridGraph[{4, 4}], 6, 7, All]},
  {Length @ lines["Graph"], lines["Multiplicity"], lines["Length"]}]
```

A bounded count gives the enumerated form.

```wl
FindInfraLine[CycleGraph[6], 1, 2, 2, Method -> "Greedy"]
```

Enumerate the pool on demand.

```wl
FindInfraLine[GridGraph[{4, 4}], 6, 7, All]["Realizations", UpTo[2]]
```

## Properties and Relations

The first position across the pool is a measured point: every line starts at one of the two ends 1 and 13, six at each.

```wl
FindInfraLine[GridGraph[{4, 4}], 6, 7, All][[1]]
```

The measure is the fraction of lines through each vertex; the anchors carry all of them.

```wl
With[
  {measure = FindInfraLine[GridGraph[{4, 4}], 6, 7, All]["Measure"]},
  {measure[6], measure[7], measure[1]}]
```
