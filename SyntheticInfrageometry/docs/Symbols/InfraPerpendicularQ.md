---
Template: Symbol
Name: InfraPerpendicularQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPerpendicularQ
Keywords: [perpendicular, right angle, projection, foot, tolerance]
SeeAlso: [FindInfraPerpendicular, InfraParallelQ, InfraAngle, InfraEqualQ, InfraLineQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraPerpendicularQ]()[*g*, *l1*, *l2*]</code> tests whether the two lines are perpendicular.

## Details & Options

Definition, with the default `Method -> "Projection"`: at each common vertex, the foot of the perpendicular from one line onto the other must lie in the intersection of the two lines.

The test is a conjunction over all common vertices. It is `False` when the lines do not meet.

There is no single right definition of a right angle on a graph, so there are several methods and they are **not** equivalent.

| Method | Tests |
|---|---|
| `"Projection"` (default) | the foot of the perpendicular lies in the intersection |
| `"Coordinate"` | signed projection coordinates of the feet are centred on the meeting point |
| `"Arclength"` | the four corner wedge angles at the meeting point are equal within a tolerance |
| `"Alexandrov"` | the same, with comparison angles in the model plane of curvature *k* |

Sub-options live inside the method spec, because their natural units differ per family. `"Projection"` takes `"Equality"`, forwarded to [InfraEqualQ](): `"Subset"` is the default, and `"Set"`, `"Multiset"`, `"Diffuse"` and `"Overlap"` are also accepted. `"Coordinate"` takes `"ZeroTest"`. `"Arclength"` and `"Alexandrov"` take `"Tolerance"` in radians, and `"Alexandrov"` takes `"Curvature"`. Use `"Curvature" -> 0` for the synthetic right-angle test.

The top-level option `"Radius" -> r` localises the test to the *r*-neighbourhood of the meeting point.

**The default is strict.** A line produced by [FindInfraPerpendicular]() with its default Euclid I.12 construction is generally rejected by this predicate under `"Equality" -> "Subset"`, and accepted under `"Equality" -> "Overlap"`. The construction and the test are asking different questions: one lays off an isosceles base and takes its midpoint, the other demands that projections land exactly in the intersection. On a lattice those come apart. If you want the two to agree, pass `"Overlap"`.

Two lines on a lattice can also meet in more than one vertex, which has no analogue in the plane, and the conjunction then has to hold at each of them.

## Basic Examples

A line and a perpendicular to it, produced by the construction. The default test rejects it; the `"Overlap"` equality accepts it.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {l1 = First @ First @ FindInfraLine[g, c, far, 1]},
  {p = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 2 && ! MemberQ[l1, #] &]},
  {l2 = First @ First @ FindInfraPerpendicular[g, l1, p, 1, "Radius" -> 3]},
  <|"default (Subset)" -> InfraPerpendicularQ[g, l1, l2],
    "Overlap" -> InfraPerpendicularQ[g, l1, l2, Method -> {"Projection", "Equality" -> "Overlap"}],
    "common vertices" -> Intersection[l1, l2]|>]
```

The four methods on the same pair of lines. They do not agree, and are not meant to.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {l1 = First @ First @ FindInfraLine[g, c, far, 1]},
  {p = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 2 && ! MemberQ[l1, #] &]},
  {l2 = First @ First @ FindInfraPerpendicular[g, l1, p, 1, "Radius" -> 3]},
  Association @ Table[
    ToString[m] -> InfraPerpendicularQ[g, l1, l2, Method -> m],
    {m, {"Projection", "Coordinate", "Arclength", {"Alexandrov", "Curvature" -> 0}}}]]
```

## Properties and Relations

Lines that do not meet are never perpendicular, whatever the method.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {l1 = First @ First @ FindInfraLine[g, c, far, 1]},
  InfraPerpendicularQ[g, l1, Complement[VertexList[g], l1][[1 ;; 3]]]]
```
