---
Template: Symbol
Name: FindInfraParallel
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraParallel
Keywords: [parallel, parallel postulate, Euclid Postulate 5, level set]
SeeAlso: [InfraParallelQ, FindInfraLine, FindInfraPerpendicular, InfraLineQ, FindInfraBisectingHyperplane]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraParallel]()[*g*, *line*, *p*]</code> gives the lines through *p* parallel to *line*, as unary [InfraLine]() wrappers.

A count *n* gives exactly *n* or `$Failed`; `UpTo[n]` up to *n*; `All` all of them.

## Details & Options

Definition: a parallel to *line* through *p* is a **maximal geodesic** through *p* every vertex of which is at the same distance from *line*.

This is Euclid's fifth postulate read intrinsically, and it is the strictest of the Euclidean constructions here. Two conditions must hold at once. The candidate must be a maximal geodesic, and it must lie in a single level set of the distance-to-*line* function.

On a lattice that is hard to satisfy, and the examples below find **none**. The reason is the interaction of the two conditions. A maximal geodesic on a square grid is generally a staircase, and the distance from a staircase to a fixed line varies along it. A curve that does keep constant distance is usually not a geodesic. So the two requirements pull against each other.

This is worth stating carefully. Euclid's fifth postulate fails here, but not in the way it fails in hyperbolic geometry, where a point off a line has *many* parallels. On these substrates it has **none**. Non-existence, not multiplicity, is the failure mode.

The count is zero on a bounded patch and also on a boundary-free flat torus, so it is not an artefact of the patch having a rim.

[InfraParallelQ]() asks a different question, and the two do not match. The predicate tests that two vertex sets are disjoint and at constant distance, and does not require either to be a maximal geodesic. So it accepts pairs this function would never return — concentric shells, for instance, are parallel by that test. A parallel can exist as a *set* while no parallel exists as a *line*.

Option `Properties` takes `{}` only. Option `Method` takes `"Exhaustive"` (default), `{"Exhaustive", "Pruning" -> spec}`, or `"Greedy"`.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 5 | The parallel postulate: two lines cut by a transversal making internal angles less than two right angles meet on that side. |
| Euclid | Proposition I.31 | To draw a line through a given point parallel to a given line. |
| Hilbert | Group IV | Playfair's axiom: through a point not on a line there is exactly one parallel. |
| Tarski | A10, Euclid's axiom | The Euclidean axiom, stated with betweenness and equidistance. |

## Basic Examples

Through a point off a line, on three substrates.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Small", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
     {line = First @ First @ FindInfraLine[g, c, far, 1]},
     {p = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 2 && ! MemberQ[line, #] &]},
     Length @ First @ FindInfraParallel[g, line, p, All]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

The count does not change when the boundary is removed. On a flat torus, which has no rim at all, it is the same.

```wl
With[
  {t = IndexGraph @ TessellationGraph[{4, 4}, {12, 12}]},
  {far = SelectFirst[VertexList[t], GraphDistance[t, 1, #] == 4 &]},
  {line = First @ First @ FindInfraLine[t, 1, far, 1]},
  {p = SelectFirst[VertexList[t], GraphDistance[t, 1, #] == 2 && ! MemberQ[line, #] &]},
  <|"torus vertices" -> VertexCount[t], "parallels" -> Length @ First @ FindInfraParallel[t, line, p, All]|>]
```

## Properties and Relations

The distance from a maximal geodesic to a fixed line varies along it, which is what blocks the construction. Here is that variation for one line and one candidate.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Small", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {line = First @ First @ FindInfraLine[g, c, far, 1]},
  {p = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 2 && ! MemberQ[line, #] &]},
  {other = First @ First @ FindInfraLine[g, p, SelectFirst[VertexList[g], GraphDistance[g, p, #] == 4 &], 1]},
  Union @ Table[Min[GraphDistance[g, v, #] & /@ line], {v, other}]]
```
