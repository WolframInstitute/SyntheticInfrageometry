---
Template: Symbol
Name: FindInfraShell
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraShell
Keywords: [shell, sphere, level surface, volume growth, dimension]
SeeAlso: [InfraShell, FindInfraBall, FindInfraCircle, InfraShellQ, SeparatesQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraShell]()[*g*, *c*, *r*]</code> gives the metric shell $\{v : d(c,v) = r\}$ around *c* as an [InfraShell]() wrapper. *r* may be a band `{rmin, rmax}`.

<code>[FindInfraShell]()[*g*, *c*, *r*, *n*]</code> gives exactly *n* realisations or `$Failed`; `UpTo[n]` up to *n*; `All` (the default) all of them.

## Details & Options

The shell of radius *r* about *c* is $\{v : d(c,v) = r\}$ — the sphere of the graph metric, as a vertex **set**.

It is the discrete analogue of a sphere, not of a circle: it is codimension-1 as a set, but on a lattice its vertices are pairwise non-adjacent, so it carries no cycle. The cyclic object is [FindInfraCircle](), which needs a thickened band for exactly that reason.

The shell is the substrate of the volume-growth invariants. Its cardinality as a function of *r* is the surface-area profile, and on a flat lattice it grows **linearly**, which is the statement that the dimension is 2. The slope is a property of the tiling: 4 per step on the square grid, 3 on the hexagonal.

Option `Properties` takes `{}` (default; the whole level set as one realisation), `{"Separating"}` (inclusion-minimal subsets separating the centre from beyond), or `{"Separating", "Connected"}`. Option `Method` takes `"Exhaustive"` (default) or `"Greedy"`. The `["Volume"]` accessor gives the per-realisation vertex count as a list.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 3 | To draw a circle with any center and radius; the shell is its locus of points. |
| Tarski | Equidistance | The set equidistant from a centre, from the four-place congruence relation. |
| Hilbert | (defined) | Not primitive; defined through segment congruence. |

## Basic Examples

Shell cardinality against radius. On both lattices the growth is exactly linear — 4r on the square grid, 3r on the hexagonal — which is the intrinsic statement that these substrates are two-dimensional.

```wl
Association @ Table[
   name -> With[
     {g = ExampleGraphData[name, "Medium"]},
     {c = First @ GraphCenter[g]},
     Table[First @ FindInfraShell[g, c, r]["Volume"], {r, 0, 5}]],
   {name, {"Plane", "Square", "Hexagonal"}}]
```

The successive shells around the centre of the square grid: nested rings, each of 4r vertices, and none of them carrying a cycle.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {c = First @ GraphCenter[g]},
  InfraSceneHighlight[g,
    Table[FindInfraShell[g, c, r] -> $InfraShellColor, {r, 1, 5}],
    "PointSizeRange" -> 13,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

## Properties and Relations

The ball is the union of the shells up to its radius, so the volumes are the partial sums of the areas.

```wl
With[
  {g = ExampleGraphData["Hexagonal", "Medium"]},
  {c = First @ GraphCenter[g]},
  {areas = Table[First @ FindInfraShell[g, c, r]["Volume"], {r, 0, 5}]},
  {volumes = Table[First @ FindInfraBall[g, c, r]["Volume"], {r, 0, 5}]},
  Accumulate[areas] === volumes]
```
