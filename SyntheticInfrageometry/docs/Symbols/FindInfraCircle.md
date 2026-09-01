---
Template: Symbol
Name: FindInfraCircle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCircle
Keywords: [circle, level surface, separating cycle, girth, Euclid Postulate 3]
SeeAlso: [InfraCircle, FindInfraShell, FindInfraBall, InfraCircleQ, FindInfraEllipse]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraCircle]()[*g*, *c*, *r*]</code> gives the canonical infra-circle around *c* at radius *r*: the shortest separating cycle in the level surface, together with its ties. *r* is a scalar or a band `{rmin, rmax}`.

<code>[FindInfraCircle]()[*g*, *c*, *r*, *n*]</code> gives exactly *n* realisations or `$Failed`; `UpTo[n]` up to *n*; `All` all of them.

## Details & Options

The level surface at radius *r* around *c* is $\{v : d(c,v) = r\}$, or $\{v : r_{\min} \le d(c,v) \le r_{\max}\}$ for a band. A circle is a simple cycle inside that level surface which separates *c* from everything beyond it; the canonical one is shortest, and its ties are returned with it.

**A circle need not exist.** On a lattice a single distance shell contains no two adjacent vertices, so it spans no cycle at all and the result is empty. This is not a defect of the definition — it is what a sphere of one-vertex thickness is on a lattice.

Thickening the radius to a band fixes it, and **the band thickness that suffices tracks the girth of the tiling**. On the square grid, girth 4, a two-thick band `{r, r+1}` already carries a cycle. On the hexagonal tiling, girth 6, it does not: `{r, r+1}` is still empty and the band has to reach `{r, r+2}`. On an irregular mesh a single shell usually works, since its vertices are adjacent by accident of the triangulation.

Option `Properties` defaults to `{"Separating", "Shortest"}`, the canonical infra-circle. Dropping `"Shortest"` gives all separating cycles ordered by length; dropping `"Separating"` allows any simple cycle in the level surface. `"Connected"` is not a valid property — a cycle is connected.

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
     Length @ First @ FindInfraCircle[g, c, 4]],
   {name, {"PlanePatch", "SquarePatch", "HexagonalPatch"}}]
```

Thickening the radius to a band produces a genuine circle, and the thickness needed follows the girth: `{4, 5}` suffices on the square grid, while the hexagonal tiling needs `{4, 6}`.

```wl
Association @ Table[
   band -> With[
     {g = InfraSubstrate["HexagonalPatch", "Medium", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     Length @ First @ FindInfraCircle[g, c, band]],
   {band, {4, {4, 5}, {4, 6}}}]
```

The circle around the centre of each lattice, at the band each one needs.

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
   {spec, {{"SquarePatch", {4, 5}}, {"HexagonalPatch", {4, 6}}}}]]
```
