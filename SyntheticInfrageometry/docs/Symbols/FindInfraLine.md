---
Template: Symbol
Name: FindInfraLine
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraLine
Keywords: [line, maximal geodesic, Euclid Postulate 2, parallel postulate]
SeeAlso: [InfraLine, FindInfraSegment, FindInfraRay, FindInfraParallel, InfraLineQ, LineCount]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraLine]()[*g*, *a*, *b*]</code> gives the maximal geodesic lines through *a* and *b* in *g*, as an [InfraLine]() wrapper.

<code>[FindInfraLine]()[*g*, *segment*]</code> gives the maximal lines containing *segment* as a contiguous subsequence.

A count *n* gives exactly *n* realisations or `$Failed`, `UpTo[n]` up to *n*, `All` (the default) all of them.

## Details & Options

A line is an **inclusion-maximal geodesic**: a shortest path that cannot be extended at either end and still be a shortest path.

That is the intrinsic reading of Euclid's second postulate — produce a finite straight line continuously — and it is where the analogy with the plane breaks hardest. In the plane two points determine one line. On a lattice they determine an enormous family: below, two vertices at distance 5 on a 177-vertex square grid lie on **81600** maximal geodesics, against 192 on the hexagonal tiling and 16 on the irregular mesh.

So `All` is combinatorially dangerous on a lattice, and `UpTo[n]` is the honest default for a picture. The count itself is the interesting invariant — see [LineCount]() and [PencilCardinality]() for the summaries built on it.

Option `"Maximality"` takes `"Extension"` (default; inextensible) or `"Diameter"`. Option `"Direction"` takes `"BothSides"` (default), `"Forward"` or `"Backward"`. Option `Method` takes `"Exhaustive"` (default) or `"Greedy"`, which returns one realisation without backtracking.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 2 | To produce a finite straight-line continuously in a straight-line. |
| Euclid | Postulate 5 | The parallel postulate, which quantifies over lines and so is a statement about this family. |
| Hilbert | I.1, I.2 | Two distinct points determine one and only one line. |
| Tarski | (derived) | Lines are not primitive; collinearity is defined from betweenness. |
| Birkhoff | Ruler postulate | A line is a set of points in bijection with the reals. |

## Basic Examples

How many maximal geodesics pass through two vertices at distance 5. Uniqueness does not merely fail — on the square grid the family is enormous.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Length @ First @ FindInfraLine[g, a, b, All]],
   {name, {"Plane", "Square", "Hexagonal"}}]
```

Three of them on each substrate. Each line runs clear across the patch, since a geodesic stops only where it can no longer be extended.

```wl
Row[Table[
   With[
     {g = InfraSubstrate[name, "Medium", "Gray", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraLine[g, a, b, UpTo[3]] -> $InfraLineColor,
          InfraSet[{a, b}] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name]]],
   {name, {"Plane", "Square", "Hexagonal"}}]]
```

## Properties and Relations

A line through two points contains a geodesic between them, so its vertex sequence includes them both and the segment is a subsequence.

```wl
With[
  {g = InfraSubstrate["Hexagonal", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {line = First @ First @ FindInfraLine[g, a, b, 1]},
  {SubsetQ[line, {a, b}], InfraLineQ[g, line]}]
```
