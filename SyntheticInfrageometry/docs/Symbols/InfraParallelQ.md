---
Template: Symbol
Name: InfraParallelQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraParallelQ
Keywords: [parallel, constant distance, level set, Euclid Definition 23]
SeeAlso: [FindInfraParallel, InfraPerpendicularQ, InfraLineQ, FindInfraShell, FindInfraLine]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraParallelQ]()[*g*, *l1*, *l2*]</code> tests whether the two vertex sets are parallel: disjoint, and at constant distance from each other.

## Details & Options

Definition: *l1* and *l2* are parallel when they are disjoint and every vertex of *l2* is at the same distance from the set *l1*.

Both conditions matter, and each corresponds to something classical. Disjointness is Euclid's Definition 23, that parallel lines do not meet. Constant distance is the level-set reading, which is the only thing a metric can say without a notion of direction.

Two consequences that catch people out.

**A line is not parallel to itself.** It meets itself everywhere, so disjointness fails and the answer is `False`. Parallelism here is not reflexive.

**Sharing a single vertex is enough to fail.** Two sets at otherwise constant distance return `False` as soon as they touch.

The predicate does not require its arguments to be lines. It tests two vertex sets, so it accepts things that are not paths at all. Concentric shells are the clearest case: the shell at radius 3 is parallel to the shell at radius 2, at constant distance 1. If you need the arguments to be maximal geodesics, test that separately with [InfraLineQ]().

That is the difference from [FindInfraParallel](), and it is a real one. The finder returns only *maximal geodesics* at constant distance, and on a lattice it typically finds none. This predicate can still return `True` for sets at constant distance. A parallel can exist as a set while no parallel exists as a line.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 23 | Parallel lines are those in a plane which do not meet, however far produced. |
| Euclid | Postulate 5 | The parallel postulate. |
| Hilbert | Group IV | Playfair's axiom: exactly one parallel through a point off the line. |
| Tarski | A10 | Euclid's axiom, stated with betweenness and equidistance. |

## Basic Examples

A line is not parallel to itself, because parallels must not meet.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {line = First @ First @ FindInfraLine[g, c, far, 1]},
  InfraParallelQ[g, line, line]]
```

Concentric shells are parallel. Each vertex of the outer one is at the same distance from the inner one.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {s2 = First @ First @ FindInfraShell[g, c, 2]},
  {s3 = First @ First @ FindInfraShell[g, c, 3]},
  {s4 = First @ First @ FindInfraShell[g, c, 4]},
  <|"shell 3 to shell 2" -> InfraParallelQ[g, s2, s3],
    "shell 4 to shell 2" -> InfraParallelQ[g, s2, s4],
    "distances 3 to 2" -> Union @ Table[Min[GraphDistance[g, v, #] & /@ s2], {v, s3}],
    "distances 4 to 2" -> Union @ Table[Min[GraphDistance[g, v, #] & /@ s2], {v, s4}]|>]
```

Neither shell is a line, which shows the predicate does not require one.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  InfraLineQ[g, First @ First @ FindInfraShell[g, c, 3]]]
```

Two maximal geodesics chosen independently are generally not parallel, because the distance between them varies along their length.

```wl
With[
  {g = ExampleGraphData["Square", "Small"]},
  {c = First @ GraphCenter[g]},
  {far = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 4 &]},
  {l1 = First @ First @ FindInfraLine[g, c, far, 1]},
  {p = SelectFirst[VertexList[g], GraphDistance[g, c, #] == 2 && ! MemberQ[l1, #] &]},
  {l2 = First @ First @ FindInfraLine[g, p, SelectFirst[VertexList[g], GraphDistance[g, p, #] == 4 &], 1]},
  <|"parallel" -> InfraParallelQ[g, l1, l2],
    "distances from l2 to l1" -> Union @ Table[Min[GraphDistance[g, v, #] & /@ l1], {v, l2}]|>]
```
