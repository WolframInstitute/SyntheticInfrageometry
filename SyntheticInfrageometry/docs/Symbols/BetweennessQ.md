---
Template: Symbol
Name: BetweennessQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/BetweennessQ
Keywords: [betweenness, Tarski, geodesic, metric interval]
SeeAlso: [EquidistanceQ, MetricInterval, FindInfraSegment, FindInfraMidpoint, TarskiAxiomQ]
RelatedGuides: [TarskiGeometryGuide]
---

## Usage

<code>[BetweennessQ]()[*g*, *u*, *w*, *v*]</code> tests Tarski's betweenness $B(u,w,v)$: whether *w* lies on a geodesic from *u* to *v* in *g*.

## Details & Options

$B(u,w,v)$ holds exactly when $d(u,w) + d(w,v) = d(u,v)$.

Betweenness is one of the two primitives of Tarski's axiomatization — the other is equidistance, tested by [EquidistanceQ]() — so this predicate is where the classical axiom system meets the graph. `TarskiAxiomQ` and the eleven individual axiom predicates are all built from these two.

Note the argument order: the **middle** point is second, matching Tarski's $B(u,w,v)$, not last.

Unlike the plane, a graph has no order-completeness. Betweenness holds for a whole *set* of middle vertices, which is exactly the metric interval, and that set can be large: on a square grid the vertices between two points fill a whole diamond, not a line.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Tarski | B(u, w, v), primitive | One of the two primitive relations, together with equidistance. |
| Hilbert | Group II, order | Betweenness axioms II.1 to II.4, taken as primitive on a line. |
| Euclid | (not stated) | Betweenness is used throughout Book I but never postulated. |
| Birkhoff | Ruler postulate | $w$ is between $u$ and $v$ iff its ruler coordinate lies between theirs. |

## Basic Examples

Betweenness holds for every midpoint of two vertices, on each substrate.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     AllTrue[FindInfraMidpoint[g, a, b]["Realizations"], BetweennessQ[g, a, #, b] &]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

The set of vertices satisfying betweenness is the metric interval, and on a lattice it is broad rather than thin.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Length @ Select[VertexList[g], BetweennessQ[g, a, #, b] &]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

That interval, drawn on the square grid: every vertex between the two endpoints, not merely those on one geodesic.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "Gray", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  InfraSceneHighlight[g,
    {InfraSet[Select[VertexList[g], BetweennessQ[g, a, #, b] &]] -> $InfraShellColor,
     InfraSet[{a, b}] -> $InfraPointColor},
    "PointSizeRange" -> 15,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 320]]
```

## Properties and Relations

The vertices between two points are exactly [MetricInterval]().

```wl
With[
  {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Select[VertexList[g], BetweennessQ[g, a, #, b] &] === Sort @ MetricInterval[g, a, b]]
```
