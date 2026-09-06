---
Template: Symbol
Name: FindInfraPolygon
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPolygon
Keywords: [polygon, corners, geodesic sides, product class, Euclid Definition 19]
SeeAlso: [InfraPolygon, FindInfraTriangle, FindInfraRegularPolygon, FindInfraSegment, InfraPolygonQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraPolygon]()[*g*, {*p1*, …, *pn*}]</code> gives one polygon with corners *p1*, …, *pn* in *g* — a geodesic between each pair of consecutive corners, the last side returning to *p1* — as an [InfraPolygon]() wrapper.

<code>[FindInfraPolygon]()[*g*, {*p1*, …, *pn*}, *m*]</code> gives exactly *m* polygons or `$Failed`; `UpTo[m]` gives up to *m*; `All` gives the whole class.

## Details & Options

A polygon with corners *p1*, …, *pn* (*n* ≥ 3) is a cyclic sequence of *n* geodesics, the *i*-th from *pi* to *p(i+1)* and the last from *pn* back to *p1*. Corners are vertices or [InfraPoint]() atoms; each realisation is a list of *n* unary [InfraSegment]() sides, and `["Sides"]`, `["Length"]` (the perimeter, in edges) and `["Vertices"]` read them.

The class is the Cartesian **product** of the per-side geodesic classes, so the count multiplies: a side with *k* geodesics contributes a factor *k*. On a 5×5 grid the square on the four corners has one realisation, while the diamond on the four side midpoints has 6^4 = 1296, every side being a diagonal with six geodesics.

Nothing in the definition keeps the sides apart. A side may run back over another: on the 3×3 grid with corners 1, 3, 9 the side from 9 to 1 may retrace the two others, and the class admits it, since each side is a geodesic on its own.

`All` forms the product. A bounded count never does: it streams that many geodesics per side and reads the first members of their product off a mixed-radix index, so the count-less call — one polygon, the first geodesic of every side — is cheap and deterministic, and a strict count is exact.

| Option | Values | Meaning |
|---|---|---|
| `Method` | `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"`, `"RandomGreedy"` | forwarded to the side geodesics. `Automatic` resolves by the count: `All` to `"Exhaustive"`, a bounded or absent count to `"Greedy"`. The class is the same under every value; `"Greedy"` and `"Exhaustive"` take each side's geodesics in candidate order, `"RandomGreedy"` in random order, seeded by an ambient `SeedRandom`. `"Pruning"` is accepted and inert: the sides come off geodesic pools, which have no frontier to cap. |

Sides chosen by a local rule — straightest, curvature-minimising — are not a polygon option: build them with `FindInfraGeodesic` and assemble the [InfraPolygon]().

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 19 | Rectilinear figures are those contained by straight lines: trilateral, quadrilateral, multilateral. |
| Hilbert | (defined) | A polygon is a closed broken line of segments, defined from betweenness. |

## Basic Examples

The square on the four corners of a 5×5 grid has one realisation; the diamond on the four side midpoints has 1296.

```wl
With[
  {g = GridGraph[{5, 5}]},
  {Length @ FindInfraPolygon[g, {1, 5, 25, 21}, All]["Realizations"],
   Length @ FindInfraPolygon[g, {3, 15, 23, 11}, All]["Realizations"]}]
```

Three of the diamonds, streamed off the class in random order.

```wl
SeedRandom[1]; With[
  {g = GridGraph[{5, 5}]},
  InfraSceneHighlight[g,
    {FindInfraPolygon[g, {3, 15, 23, 11}, UpTo[3], Method -> "RandomGreedy"] -> $InfraSegmentColor,
     InfraSet[{3, 15, 23, 11}] -> $InfraPointColor},
    ImageSize -> 300]]
```

## Scope

The count-less call is one polygon, deterministic; a strict count is exact, and `$Failed` when the class is smaller.

```wl
With[
  {g = GridGraph[{5, 5}]},
  {FindInfraPolygon[g, {3, 15, 23, 11}]["Length"],
   Length @ FindInfraPolygon[g, {3, 15, 23, 11}, UpTo[2000]]["Realizations"],
   FindInfraPolygon[g, {3, 15, 23, 11}, 2000]}]
```

A side may retrace the others.

```wl
FindInfraPolygon[GridGraph[{3, 3}], {1, 3, 9}]["Realizations"]
```

Corners may be [InfraPoint]() atoms.

```wl
FindInfraPolygon[GridGraph[{5, 5}], InfraPoint /@ {1, 5, 25, 21}]["Vertices"]
```

## Options

### Method

The class is the same under every `Method`; only the order in which polygons come off it differs.

```wl
With[
  {g = GridGraph[{5, 5}]},
  SameQ @@ (Sort @ FindInfraPolygon[g, {3, 15, 23, 11}, All, Method -> #]["Realizations"] & /@
     {"Exhaustive", "Greedy", "RandomGreedy"})]
```

## Properties and Relations

Every polygon satisfies [InfraPolygonQ](), and a three-corner polygon is a [FindInfraTriangle]().

```wl
With[
  {g = GridGraph[{3, 3}]},
  {InfraPolygonQ[g, FindInfraPolygon[g, {1, 3, 9}, All]],
   FindInfraPolygon[g, {1, 3, 9}, All]["Realizations"] === FindInfraTriangle[g, {1, 3, 9}, All]["Realizations"]}]
```

The perimeter is the sum of the corner distances around the cycle.

```wl
With[
  {g = GridGraph[{5, 5}]},
  {corners = {3, 15, 23, 11}},
  First @ FindInfraPolygon[g, corners]["Length"] ===
    Total[GraphDistance[g, ##] & @@@ Partition[Append[corners, First @ corners], 2, 1]]]
```
