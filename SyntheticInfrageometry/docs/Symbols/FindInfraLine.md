---
Template: Symbol
Name: FindInfraLine
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraLine
Keywords: [line, inextensible geodesic, distance matrix, pool, Euclid Postulate 2, parallel postulate]
SeeAlso: [InfraLine, InfraLineQ, ExtendInfraSegment, GeodesicExtensionGraph, FindInfraSegment, FindInfraRay, FindInfraParallel, LineCount]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraLine]()[*g*, *a*, *b*]</code> gives one line through *a* and *b* in *g* — an inextensible geodesic containing both — as an [InfraLine]() wrapper.

<code>[FindInfraLine]()[*g*, *segment*]</code> gives one line containing *segment* as a contiguous subsequence; *segment* is a vertex sequence or an [InfraSegment]() bundle, which extends as a whole.

<code>[FindInfraLine]()[*g*, *a*, *b*, *n*]</code> gives exactly *n* lines or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class as a pool.

## Details & Options

A line is an **inextensible geodesic**: a shortest path that no neighbour of either endpoint prolongs. [InfraLineQ]() is the predicate, and every line returned satisfies it under every `Method`.

That is the intrinsic reading of Euclid's second postulate — produce a finite straight line continuously — and it is where the analogy with the plane breaks hardest. In the plane two points determine one line. On a lattice they determine an enormous family: below, two vertices at distance 5 on a 313-vertex square-tiling patch lie on 5 242 880 lines, against 6144 on the hexagonal tiling and 1386 on the irregular mesh.

Everything is read off the distance matrix. Write *d* for *d(a, b)*. A pair of ends (*s*, *e*) is **admissible** when *d(s, e) = d(s, a) + d + d(b, e)* — the concatenation of any geodesics *s…a*, *a…b*, *b…e* is then a geodesic, whichever geodesics are used — and no neighbour of *s* or *e* prolongs *d(s, e)*. The candidate ends are the vertices of the two extension graphs <code>[GeodesicExtensionGraph]()[*g*, {*b*, *a*}]</code> and <code>[GeodesicExtensionGraph]()[*g*, {*a*, *b*}]</code>. Admissibility on each side alone is not enough: on the 6-cycle through the edge 1–2 the ends 5 and 4 are each admissible, but *d(5, 4) = 1*, so the pool is two DAGs *plus* a compatibility relation, and there are three lines through that edge, not four.

`All` returns the **pool** <code>[InfraLine]()[{*dag1*, …}]</code>, one geodesic DAG per admissible pair of ends, whose source-to-sink paths are exactly the lines with those ends. `["Multiplicity"]`, `["Length"]` (one number per DAG) and `["Measure"]` are read off the DAGs by dynamic programming, without enumeration; `["Realizations"]` enumerates on demand. A bounded count streams lines off the admissible pairs.

Because admissibility reads only the ends, a whole geodesic bundle extends as one object: <code>[FindInfraLine]()[*g*, *seg*, All]</code> is <code>[ExtendInfraSegment]()[*g*, *seg*, Infinity, All]</code>, the extension pool with no budget.

The longest lines are a selection on the result, `SelectInfraWalk[g, lines, All, "From" -> "MaxLength"]`, which reads the lengths off the pool and keeps the pool form.

| Option | Values | Meaning |
|---|---|---|
| `Method` | `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"`, `"RandomGreedy"` | `Automatic` resolves by the count: `All` to `"Exhaustive"`, the pool; a bounded or absent count to `"Greedy"`, which takes the admissible pairs and the branches of each DAG in candidate order, so the count-less call is deterministic. `"RandomGreedy"` takes them in random order, seeded by an ambient `SeedRandom`. The class is the same under every value; `"Pruning"` is accepted and inert, the pool having no frontier to cap. |
| `"Direction"` | `"BothSides"` (default), `"Forward"`, `"Backward"` | which ends may move: `"Forward"` keeps *a* and extends past *b* only, `"Backward"` keeps *b* and extends before *a* only. |
| `Properties` | `{}` | only the empty list; a rule on the extension is a local law and lives on `ExtendInfraGeodesic`. |

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 2 | To produce a finite straight-line continuously in a straight-line. |
| Euclid | Postulate 5 | The parallel postulate, which quantifies over lines and so is a statement about this family. |
| Hilbert | I.1, I.2 | Two distinct points determine one and only one line. |
| Tarski | (derived) | Lines are not primitive; collinearity is defined from betweenness. |
| Birkhoff | Ruler postulate | A line is a set of points in bijection with the reals. |

## Basic Examples

How many lines pass through two vertices at distance 5. Uniqueness does not merely fail — on the square tiling the family is enormous, and the pool counts it without enumerating a single line.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     FindInfraLine[g, a, b, All]["Multiplicity"]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

Three of them on each substrate, drawn off the pool in random order: `"RandomGreedy"` is the explicit random call, and the seed goes in front of it.

```wl
SeedRandom[1]; Row[Table[
   With[
     {g = InfraSubstrate[name, "Medium", "Gray", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraLine[g, a, b, UpTo[3], Method -> "RandomGreedy"] -> $InfraLineColor,
          InfraSet[{a, b}] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]]
```

## Scope

Through an edge of the 6-cycle there are three lines, not four: the ends 5 and 4 are each admissible on their own side, but the pair is not.

```wl
FindInfraLine[CycleGraph[6], 1, 2, All]["Realizations"]
```

`All` is the pool: one geodesic DAG per admissible pair of ends, and the counts come from the DAGs.

```wl
With[
  {lines = FindInfraLine[GridGraph[{4, 4}], 6, 7, All]},
  {lines["Multiplicity"], lines["Length"], Length @ lines["Graph"]}]
```

A bounded count streams lines off the pool; a strict count that cannot be met is `$Failed`.

```wl
{FindInfraLine[CycleGraph[6], 1, 2, 2, Method -> "Greedy"],
 FindInfraLine[CycleGraph[6], 1, 2, 5]}
```

A geodesic bundle extends as one object. Six geodesics from a corner of the grid, twelve lines through all of them at once.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {seg = FindInfraSegment[g, 1, 11, All]},
  {seg["Multiplicity"], FindInfraLine[g, seg, All]["Multiplicity"]}]
```

## Options

### "Direction"

`"Forward"` keeps the first anchor fixed and extends past the second; `"Backward"` the reverse.

```wl
{FindInfraLine[PathGraph[Range[5]], 2, 3, All, "Direction" -> "Forward"]["Realizations"],
 FindInfraLine[PathGraph[Range[5]], 2, 3, All, "Direction" -> "Backward"]["Realizations"]}
```

### Method

The class is the same under every `Method`; only the order in which lines come off the pool differs.

```wl
With[
  {g = GridGraph[{4, 4}]},
  SameQ @@ (Sort @ FindInfraLine[g, 6, 7, All, Method -> #]["Realizations"] & /@
     {"Exhaustive", "Greedy", "RandomGreedy"})]
```

## Properties and Relations

Every line satisfies [InfraLineQ](); the predicate accepts the pool.

```wl
InfraLineQ[GridGraph[{4, 4}], FindInfraLine[GridGraph[{4, 4}], 6, 7, All]]
```

A line through a segment is that segment extended with no budget.

```wl
With[
  {g = GridGraph[{4, 4}]},
  Sort @ FindInfraLine[g, {6, 7}, All]["Realizations"] ===
    Sort @ ExtendInfraSegment[g, {6, 7}, Infinity, All]["Realizations"]]
```

The longest lines are a selection on the pool. On the irregular mesh the four lines through two vertices have three different lengths.

```wl
With[
  {g = InfraSubstrate["SquareMeshGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {lines = FindInfraLine[g, a, b, All]},
  {lines["Length"], SelectInfraWalk[g, lines, All, "From" -> "MaxLength"]["Length"]}]
```
