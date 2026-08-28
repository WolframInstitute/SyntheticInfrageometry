---
Template: Symbol
Name: FindInfraBisectingHyperplane
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraBisectingHyperplane
Keywords: [perpendicular bisector, hyperplane, bisector, separating set, Euclid I.10]
SeeAlso: [InfraPlane, FindInfraMidpoint, EquidistanceQ, SeparatesQ, FindInfraEquidistantSet]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraBisectingHyperplane]()[*g*, *p1*, *p2*]</code> gives the perpendicular bisector $\{v : d(p_1,v) = d(p_2,v)\}$ as an [InfraPlane]() wrapper. A positional `{lo, hi}` widens it to the slab $lo \le d(p_1,v) - d(p_2,v) \le hi$.

<code>[FindInfraBisectingHyperplane]()[*g*, *p1*, *p2*, *n*]</code> gives exactly *n* realisations or `$Failed`; `UpTo[n]` up to *n*; `All` all of them.

## Details & Options

The bisector of *p1* and *p2* is the set of vertices equidistant from both. It is the codimension-1 object of the metric: in the plane a line, here a vertex set.

**It is empty at odd distance.** If $d(p_1,p_2)$ is odd then no vertex can be equidistant from both, since the two distances would have to be equal halves of an odd number. Below, two vertices at distance 6 on a square grid have a 15-vertex bisector, while at distance 5 the bisector is empty. This is the same parity obstruction that makes [FindInfraMidpoint]() return a two-vertex effective point at odd distance, seen one dimension up.

The `{lo, hi}` slab is the repair: widening to $-1 \le d(p_1,v) - d(p_2,v) \le 1$ catches the vertices that straddle the bisector and is non-empty at either parity.

Option `Properties` takes `{}` (default; the slab itself as one realisation), `{"Separating"}` (inclusion-minimal subsets that disconnect *p1* from *p2*), or `{"Separating", "Connected"}`. Option `Method` takes `"Exhaustive"` (default) or `"Greedy"`. The `["Volume"]` accessor gives the per-realisation vertex count.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Proposition I.10, I.11 | Bisecting a segment, and erecting a perpendicular at a point on it. |
| Hilbert | Group III, congruence | The locus equidistant from two points, from segment congruence. |
| Tarski | Equidistance | The set of *v* with *v p1* congruent to *v p2*, directly in the primitive relation. |
| Birkhoff | Ruler postulate | The locus at equal ruler distance from two points. |

## Basic Examples

The bisector exists at even distance and is empty at odd — a parity obstruction, not a failure of the construction.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {a = First @ GraphCenter[g]},
  {even = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  {odd = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  <|"distance 6" -> First @ FindInfraBisectingHyperplane[g, a, even]["Volume"],
    "distance 5" -> First @ FindInfraBisectingHyperplane[g, a, odd]["Volume"]|>]
```

The bisector of two vertices at distance 6, drawn with its endpoints. It runs clear across the patch, separating one from the other.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  InfraSceneHighlight[g,
    {FindInfraBisectingHyperplane[g, a, b] -> $InfraPlaneColor,
     InfraSet[{a, b}] -> $InfraPointColor},
    "PointSizeRange" -> 15,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

## Properties and Relations

Every vertex of the bisector is equidistant from the two points, which is what [EquidistanceQ]() tests.

```wl
With[
  {g = ExampleGraphData["Hexagonal", "Medium"]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  AllTrue[First @ First @ FindInfraBisectingHyperplane[g, a, b], EquidistanceQ[g, a, #, b, #] &]]
```
