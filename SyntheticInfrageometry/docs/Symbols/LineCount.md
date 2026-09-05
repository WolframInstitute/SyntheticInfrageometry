---
Template: Symbol
Name: LineCount
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/LineCount
Keywords: [line, count, inextensible geodesic]
SeeAlso: [FindInfraLine, InfraLineQ, PencilCardinality, UniversalLineQ, FindLineStructure]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[LineCount]()[*g*]</code> gives the number of distinct lines in *g* — inextensible geodesics, counted up to reversal.

## Details & Options

A line and its reverse are one line. The count enumerates <code>[FindInfraLine]()[*g*, *u*, *v*, All]</code> over every pair of vertices and deduplicates, so it is exponential on lattices; it is meant for small graphs, where it is the coarsest invariant of the incidence structure.

## Basic Examples

The 6-cycle has six lines, one per antipodal pair of ends; a path has one.

```wl
{LineCount[CycleGraph[6]], LineCount[PathGraph[Range[5]]]}
```

The 3 × 3 grid has twelve.

```wl
LineCount[GridGraph[{3, 3}]]
```
