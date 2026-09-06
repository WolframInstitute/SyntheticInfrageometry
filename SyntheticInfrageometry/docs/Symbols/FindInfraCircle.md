---
Template: Symbol
Name: FindInfraCircle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCircle
Keywords: [circle, level surface, separating cycle, circle pool, girth, Euclid Postulate 3]
SeeAlso: [InfraCircle, FindInfraShell, FindInfraBall, InfraCircleQ, FindInfraEllipse]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraCircle]()[*g*, *c*, *r*]</code> gives one infra-circle around *c* at radius *r* in *g*: a shortest cycle in the level surface at radius *r* that separates *c* from what lies beyond, as an [InfraCircle]() wrapper. *r* is a scalar or a band `{rmin, rmax}`.

<code>[FindInfraCircle]()[*g*, *c*, *r*, *n*]</code> gives exactly *n* circles or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class, as the circle pool where one exists.

## Details & Options

The level surface at radius *r* around *c* is $\{v : d(c,v) = r\}$, or $\{v : r_{\min} \le d(c,v) \le r_{\max}\}$ for a band. A circle is a simple cycle inside that level surface which separates *c* from everything beyond it; the canonical ones are the shortest, and the count-less call returns one of them.

**A circle need not exist.** On a lattice a single distance shell contains no two adjacent vertices, so it spans no cycle at all and the result is empty. This is not a defect of the definition — it is what a sphere of one-vertex thickness is on a lattice.

Thickening the radius to a band fixes it, and **the band thickness that suffices tracks the girth of the tiling**. On the square grid, girth 4, a two-thick band `{r, r+1}` already carries a cycle. On the hexagonal tiling, girth 6, it does not: `{r, r+1}` is still empty and the band has to reach `{r, r+2}`. On an irregular mesh a single shell usually works, since its vertices are adjacent by accident of the triangulation.

The class is carried two ways. Under the default `Properties`, a single anchor and an integer band, the circles are the source-to-sink paths of a **pool** of geodesic DAGs — the level surface cut open along a radial seam: `All` returns <code>[InfraCircle]()[{*dag*, …}]</code>, whose `["Multiplicity"]` counts the circles without enumerating one, and a bounded count streams circles off the atoms. Below, the band `{4, 5}` on the irregular mesh holds 75 circles in one pool. Off that class — other `Properties`, several anchors, or a band the seam cannot cut open, reported by `::uncertified` — the family is enumerated by a cycle sweep over the level surface, length by length, stopping at the first non-empty length when `"Shortest"` is present.

Option `Properties` defaults to `{"Separating", "Shortest"}`, the canonical infra-circle. Dropping `"Shortest"` gives all separating cycles in order of length; dropping `"Separating"` allows any simple cycle in the level surface. `"Connected"` is not a valid property — a cycle is connected.

| Option | Values | Meaning |
|---|---|---|
| `Properties` | `{"Separating", "Shortest"}` (default), `{"Separating"}`, `{"Shortest"}`, `{}` | which cycles of the level surface count, conjoined. |
| `Method` | `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"`, `"RandomGreedy"` | `Automatic` resolves by the count: `All` to `"Exhaustive"`, a bounded or absent count to `"Greedy"`. The class is the same under every value; `"Greedy"` and `"Exhaustive"` take the atoms and their branches in candidate order, `"RandomGreedy"` in random order, seeded by an ambient `SeedRandom`. `"Pruning" -> n | p | Infinity` caps the cycles the sweep keeps per length — the result is then the shortest among the survivors — and is inert on the pool, which has no frontier. |

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 3 | To draw a circle with any center and radius. |
| Hilbert | (not primitive) | Circles are defined from congruence, not postulated. |
| Tarski | Equidistance | The locus of points equidistant from a centre, from the four-place congruence relation. |
| Birkhoff | Ruler postulate | The locus at fixed ruler distance from a point. |

## Basic Examples

At a single radius the circle exists on the irregular mesh and is empty on both lattices — the shell has no two adjacent vertices to make a cycle from.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     FindInfraCircle[g, c, 4, All]["Multiplicity"]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

Thickening the radius to a band produces a genuine circle, and the thickness needed follows the girth: `{4, 5}` suffices on the square grid, while the hexagonal tiling needs `{4, 6}`.

```wl
Association @ Table[
   band -> With[
     {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     FindInfraCircle[g, c, band, All]["Multiplicity"]],
   {band, {4, {4, 5}, {4, 6}}}]
```

The circle around the centre of each lattice, at the band each one needs; on both it is unique.

```wl
Row[Table[
   With[
     {g = InfraSubstrate[First[spec], "Medium", "Gray", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraCircle[g, c, Last[spec]] -> $InfraCircleColor,
          InfraPoint[c] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[First[spec] <> ", band " <> ToString[Last[spec]]]]],
   {spec, {{"SquareTilingGraph", {4, 5}}, {"HexagonalTilingGraph", {4, 6}}}}]]
```

## Scope

On the irregular mesh the band `{4, 5}` holds 75 circles. `All` returns them as one pool without enumerating them; the count-less call is one of them, 28 edges long.

```wl
With[
  {g = InfraSubstrate["SquareMeshGraph", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {FindInfraCircle[g, c, {4, 5}, All]["Multiplicity"], FindInfraCircle[g, c, {4, 5}]["Length"]}]
```

A bounded count streams circles off the pool; a strict count that cannot be met is `$Failed`. Sixteen circles lie in the band `{2, 4}` around the centre of a 9×9 grid.

```wl
With[
  {g = GridGraph[{9, 9}]},
  {FindInfraCircle[g, 41, {2, 4}, All]["Multiplicity"],
   Length @ FindInfraCircle[g, 41, {2, 4}, 3]["Realizations"],
   FindInfraCircle[g, 41, {2, 4}, 20]}]
```

## Options

### Properties

Without `"Shortest"` every separating cycle in the band is returned, by length: 256 of them in the band `{1, 3}` around the centre of a 7×7 grid, from one of length 8 to 81 of length 16.

```wl
Tally[Length /@ FindInfraCircle[GridGraph[{7, 7}], 25, {1, 3}, All, Properties -> {"Separating"}]["Realizations"]]
```

### Method

The class is the same under every `Method`; only the order in which circles come off the pool differs.

```wl
With[
  {g = GridGraph[{9, 9}]},
  SameQ @@ (Sort @ FindInfraCircle[g, 41, {2, 4}, All, Method -> #]["Realizations"] & /@
     {"Exhaustive", "Greedy", "RandomGreedy"})]
```

The count-less call is deterministic; `"RandomGreedy"` takes the pool in random order, so the seed goes in front.

```wl
SeedRandom[1]; With[
  {g = GridGraph[{9, 9}]},
  {FindInfraCircle[g, 41, {2, 4}] === FindInfraCircle[g, 41, {2, 4}],
   FindInfraCircle[g, 41, {2, 4}, Method -> "RandomGreedy"]["Realizations"]}]
```

Off the pool the family comes from the cycle sweep, and `"Pruning"` caps the cycles kept per length: the 256 separating cycles above become one per length.

```wl
Length @ FindInfraCircle[GridGraph[{7, 7}], 25, {1, 3}, All,
   Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}]["Realizations"]
```

## Properties and Relations

A circle lies in the level surface: every one of its vertices belongs to the shell at the same band.

```wl
With[
  {g = GridGraph[{9, 9}]},
  {circle = First @ FindInfraCircle[g, 41, {2, 4}]["Realizations"]},
  SubsetQ[First @ First @ FindInfraShell[g, 41, {2, 4}], circle]]
```
