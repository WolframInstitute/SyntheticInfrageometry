---
Template: Symbol
Name: FindInfraTriangle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraTriangle
Keywords: [triangle, corners, geodesic sides, product class, Euclid I.22]
SeeAlso: [InfraTriangle, FindInfraPolygon, CompleteInfraEquilateralTriangle, FindInfraSegment, InfraTriangleQ, ComparisonTriangle]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraTriangle]()[*g*, {*a*, *b*, *c*}]</code> gives one triangle with corners *a*, *b*, *c* in *g* — a geodesic on each side — as an [InfraTriangle]() wrapper.

<code>[FindInfraTriangle]()[*g*, {*a*, *b*, *c*}, *n*]</code> gives exactly *n* triangles or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class.

## Details & Options

A triangle with corners *a*, *b*, *c* is three geodesics, *a … b*, *b … c* and *c … a*. It is the three-corner case of [FindInfraPolygon]() and shares its engine, options and defaults: corners are vertices or [InfraPoint]() atoms, each realisation is a list of three unary [InfraSegment]() sides, and `["Sides"]`, `["Length"]` (the perimeter, in edges) and `["Vertices"]` read them.

The class is the product of the three geodesic classes, so the count multiplies. Below, the triangle on the centre of each substrate and two vertices at distance 4 from it and from each other has 18 realisations on the irregular mesh, 36 on the square tiling and 8 on the hexagonal — all of perimeter 12.

Nothing in the definition keeps the sides apart. On the 3×3 grid with corners 1, 3, 9 the side from 9 to 1 may retrace the two others, and the class admits it, since each side is a geodesic on its own.

`All` forms the product. A bounded count never does: it streams that many geodesics per side and reads the first members of their product, so the count-less call — one triangle, the first geodesic of every side — is cheap and deterministic, and a strict count is exact.

| Option | Values | Meaning |
|---|---|---|
| `Method` | `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"`, `"RandomGreedy"` | forwarded to the side geodesics. `Automatic` resolves by the count: `All` to `"Exhaustive"`, a bounded or absent count to `"Greedy"`. The class is the same under every value; `"Greedy"` and `"Exhaustive"` take each side's geodesics in candidate order, `"RandomGreedy"` in random order, seeded by an ambient `SeedRandom`. `"Pruning"` is accepted and inert: the sides come off geodesic pools, which have no frontier to cap. |

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 19 | A trilateral figure is one contained by three straight lines. |
| Euclid | Proposition I.22 | To construct a triangle out of three straight lines equal to three given straight lines. |
| Hilbert | (defined) | A triangle is three points not on a line together with the segments joining them. |

## Basic Examples

How many triangles have the centre of each substrate and two vertices at distance 4 from it and from each other as corners.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 &]},
     {d = Last @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 && GraphDistance[g, b, #] == 4 &]},
     Length @ FindInfraTriangle[g, {c, b, d}, All]["Realizations"]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

All of them at once, drawn diffusely: a side lying on many triangles is drawn more strongly than one lying on few.

```wl
Row[Table[
   With[
     {g = InfraSubstrate[name, "Medium", "Gray", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 &]},
     {d = Last @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 && GraphDistance[g, b, #] == 4 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraTriangle[g, {c, b, d}, All] -> $InfraSegmentColor,
          InfraSet[{c, b, d}] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]]
```

## Scope

On the 3×3 grid the corners 1, 3, 9 carry six triangles. The count-less call is one of them — its third side retraces the other two — and a strict count of seven is `$Failed`.

```wl
With[
  {g = GridGraph[{3, 3}]},
  {FindInfraTriangle[g, {1, 3, 9}]["Realizations"],
   Length @ FindInfraTriangle[g, {1, 3, 9}, All]["Realizations"],
   FindInfraTriangle[g, {1, 3, 9}, 7]}]
```

## Options

### Method

The class is the same under every `Method`; only the order in which triangles come off it differs, and `"RandomGreedy"` draws the witness in random order, so the seed goes in front.

```wl
SeedRandom[1]; With[
  {g = GridGraph[{3, 3}]},
  {SameQ @@ (Sort @ FindInfraTriangle[g, {1, 3, 9}, All, Method -> #]["Realizations"] & /@
      {"Exhaustive", "Greedy", "RandomGreedy"}),
   FindInfraTriangle[g, {1, 3, 9}, Method -> "RandomGreedy"]["Realizations"]}]
```

## Properties and Relations

Every triangle satisfies [InfraTriangleQ](), and the class is the polygon class on the same three corners.

```wl
With[
  {g = GridGraph[{3, 3}]},
  {InfraTriangleQ[g, FindInfraTriangle[g, {1, 3, 9}, All]],
   FindInfraTriangle[g, {1, 3, 9}, All]["Realizations"] === FindInfraPolygon[g, {1, 3, 9}, All]["Realizations"]}]
```

The perimeter is the sum of the three corner distances.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {d = Last @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 && GraphDistance[g, b, #] == 4 &]},
  First @ FindInfraTriangle[g, {c, b, d}]["Length"] === GraphDistance[g, c, b] + GraphDistance[g, b, d] + GraphDistance[g, d, c]]
```
