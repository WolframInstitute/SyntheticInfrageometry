---
Template: Symbol
Name: InfraAngle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraAngle
Keywords: [angle, comparison triangle, Alexandrov, arclength, radian]
SeeAlso: [InfraScalarProduct, ComparisonTriangle, CATInequalityQ, InfraPerpendicularQ, InfraCurvature]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraAngle]()[*g*, {*q1*, *p*, *q2*}]</code> gives the angle at *p* between the directions to *q1* and *q2*, in radians.

## Details & Options

Definition, with `Method -> "Alexandrov"`: the angle at *p* is the angle at the corresponding corner of the Euclidean triangle with the same three side lengths.

Writing *d1 = d(p,q1)*, *d2 = d(p,q2)* and *c = d(q1,q2)*, that is

*angle = ArcCos[(d1² + d2² − c²) / (2 d1 d2)]*

This is the comparison angle, and it is a closed form in three distances. Nothing else about the graph enters. It always lies in *[0, π]*.

Two values come out exactly right. If *q1* and *q2* lie on opposite arms of one geodesic through *p*, then *c = d1 + d2* and the angle is exactly π. If *q2* lies on a geodesic from *p* to *q1*, then *c = |d1 − d2|* and the angle is exactly 0.

Because only three distances enter, the comparison angle cannot separate configurations that share them. On a square grid the metric is ℓ¹ and the chord saturates early: two vertices on the same shell of radius 4 can be at distance 8 from each other without lying on a common line through the centre, and the angle then reads π just as a straight line does. This is a property of the substrate, not an error.

`Method -> {"Alexandrov", "Curvature" -> k}` compares in the model plane of curvature *k* instead of the Euclidean one.

`Method -> "Arclength"` is the default and computes something different: it takes the arclength between the two directions on the boundary around *p*, and divides by the radius. It is a synthetic radian measure, not a comparison angle, and the two methods do not agree. On a square grid the straight-line case gives 3 rather than π. It also needs the two arms at equal radius to be meaningful, since the radius appears in the denominator.

The identity worth knowing is with [InfraScalarProduct](): the Alexandrov angle at curvature *k* is `ArcCos` of the scalar product divided by the two lengths, for every *k*.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 8, Postulate 4 | A plane angle is the inclination of two lines. All right angles are equal. |
| Hilbert | Group III | Angle congruence is primitive, alongside segment congruence. |
| Tarski | (derived) | No angle primitive. Angles are defined from equidistance. |
| Birkhoff | Protractor postulate | Angles are coordinatised by the reals modulo 2π. |

## Basic Examples

Three configurations at the centre of a square grid. Opposite arms of a line give exactly π. The same direction gives exactly 0.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {far = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 5 &]},
  {line = First @ First @ FindInfraLine[g, c, far, 1]},
  {i = First @ FirstPosition[line, c]},
  <|"opposite arms" -> N @ InfraAngle[g, {line[[i - 4]], c, line[[i + 4]]}, Method -> "Alexandrov"],
    "same direction" -> N @ InfraAngle[g, {line[[i + 4]], c, line[[i + 2]]}, Method -> "Alexandrov"]|>]
```

The Alexandrov method is the closed form in three distances, and agrees with it exactly.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {q1 = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {q2 = Last @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {d1 = GraphDistance[g, c, q1], d2 = GraphDistance[g, c, q2], ch = GraphDistance[g, q1, q2]},
  {N @ InfraAngle[g, {q1, c, q2}, Method -> "Alexandrov"],
   ArcCos[(d1^2 + d2^2 - ch^2)/(2. d1 d2)]}]
```

The two methods answer different questions. On the straight-line case, arclength gives 3 where the comparison angle gives π.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {far = First @ Sort @ Select[VertexList[g], GraphDistance[g, c, #] == 5 &]},
  {line = First @ First @ FindInfraLine[g, c, far, 1]},
  {i = First @ FirstPosition[line, c]},
  {arms = {line[[i - 4]], c, line[[i + 4]]}},
  <|"Arclength" -> N @ InfraAngle[g, arms], "Alexandrov" -> N @ InfraAngle[g, arms, Method -> "Alexandrov"]|>]
```
