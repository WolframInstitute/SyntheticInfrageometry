---
Template: Symbol
Name: InfraLineQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraLineQ
Keywords: [line, inextensible geodesic, predicate]
SeeAlso: [FindInfraLine, InfraLine, InfraSegmentQ, InfraRayQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraLineQ]()[*g*, *walk*]</code> tests whether *walk* is a line in *g*: a geodesic that no neighbour of either endpoint prolongs.

<code>[InfraLineQ]()[*g*, *lines*]</code> tests every line of an [InfraLine]() bundle or pool.

## Details & Options

Two conditions: the sequence is a geodesic ([InfraSegmentQ]()), and stepping off either end to a neighbour gives a walk that is no longer a shortest path — no neighbour of the first vertex lies one step farther from the last, and no neighbour of the last lies one step farther from the first. Both tests are single distance lookups.

Sequences shorter than two vertices are `False`.

The predicate is the companion of [FindInfraLine](), and every line that finder returns satisfies it under every `Method`.

## Basic Examples

On a path the whole graph is a line; any proper stretch is not, since it can still be prolonged.

```wl
{InfraLineQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}], InfraLineQ[PathGraph[Range[5]], {2, 3, 4}]}
```

On the 6-cycle a geodesic of length 3 reaches the antipode and is a line; one of length 2 is not.

```wl
{InfraLineQ[CycleGraph[6], {1, 2, 3, 4}], InfraLineQ[CycleGraph[6], {1, 2, 3}]}
```

## Properties and Relations

Every line [FindInfraLine]() produces satisfies the predicate; the pool is accepted as a whole.

```wl
InfraLineQ[GridGraph[{4, 4}], FindInfraLine[GridGraph[{4, 4}], 6, 7, All]]
```

A line on a grid runs from boundary to boundary; the grid row {1, 2, 3, 4} is a line, the stretch {2, 3} is not.

```wl
{InfraLineQ[GridGraph[{4, 4}], {1, 2, 3, 4}], InfraLineQ[GridGraph[{4, 4}], {2, 3}]}
```
