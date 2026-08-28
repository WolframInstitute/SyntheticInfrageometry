---
Template: Symbol
Name: InfraEffectivePoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraEffectivePoint
Keywords: [effective point, measure, occupation, multiplicity, wrapper]
SeeAlso: [InfraPoint, InfraSet, InfraMeasure, FindInfraMidpoint, FindInfraShellCenter]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraEffectivePoint]()[<|*v* -> *m*, …|>]</code> is a measure on vertices — a point known only up to a distribution.

<code>[InfraEffectivePoint]()[{*v1*, …}, {*m1*, …}]</code> is sugar for the association form.

<code>[InfraEffectivePoint]()[*obj*]</code> is the occupation measure of any `Infra*` object.

## Details & Options

Definition: a effective point is a finitely supported measure on the vertex set — the **measure** layer of the point ontology, sitting above the [InfraPoint]() atom and the [InfraSet]() region.

The name is the point of it. A construction whose answer is a single point in the continuum often cannot localise to a single vertex on a graph: the honest answer is a blob with weights. That is a point at intermediate resolution, and it is a distinct kind of object from a region that merely happens to be small.

Weights count. They are not probabilities put in by hand — a weight of 6 means six geodesics chose that vertex.

Effective points are **created at a projection**, never propagated. The producers are `seg[[i]]` (column occupation), [FindInfraMidpoint]() and [FindInfraGoldenSection]() (layer-band occupation), [FindInfraShellCenter]() (chord-bisection multiplicity), and [InfraMeasure]() itself. Weights on an *input* do not flow through a construction: a construction reads the support and ignores the measure.

Up-coercions, all canonical:

| From | Gives |
|---|---|
| a list of `InfraPoint` atoms | the counting measure — repetition is mass |
| an `InfraSet` | the all-ones measure, which **stays** a effective point |
| any bundle wrapper (`InfraBall`, `InfraSegment`, …) | its vertex occupation; a geodesic-DAG atom by dynamic programming, without enumerating the family |

A **bare vertex list is refused**. `InfraEffectivePoint[{1, 2}]` stays inert by design: on a graph with list-valued vertex labels a support list and a single label are indistinguishable, and that ambiguity is what the three-head split exists to remove.

Accessors:

| Accessor | Gives |
|---|---|
| `["Support"]` | the vertices, as an `InfraSet` |
| `["Vertices"]` | the vertices, as a plain list |
| `["Weights"]` | the raw counts |
| `["Mass"]` | the total of the weights |
| `["Entropy"]` | Shannon entropy of the normalised measure; `0` iff the effective point is sharp |
| `["Measure"]` | mass over the heaviest mass — membership in [0,1], what the renderer draws |
| `["ProbabilityMeasure"]` | mass over total mass — the distribution summing to 1 |
| `["OccupationCount"]` | the raw masses |

In [InfraSceneHighlight]() a effective point renders diffusely by **relative** mass: each vertex is drawn at its mass over the heaviest mass, so the modal vertex is full and lighter ones fade. A uniform effective point — a ball, for instance — is therefore uniformly bright; its diffuseness is its extent, not a per-vertex fade.

## Basic Examples

Two vertices at odd distance have no exact midpoint. The answer is a effective point.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  FindInfraMidpoint[g, a, b]]
```

The accessors read it apart. The support is a region; the entropy says how far from sharp it is.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {m = FindInfraMidpoint[g, a, b]},
  <|"support" -> m["Support"], "weights" -> m["Weights"], "mass" -> m["Mass"],
    "entropy" -> m["Entropy"]|>]
```

The weights are what the picture shows.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  InfraSceneHighlight[g,
    {FindInfraMidpoint[g, a, b] -> $InfraCircleColor, InfraSet[{a, b}] -> $InfraPointColor},
    "PointSizeRange" -> 22,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

## Properties and Relations

The two lossy read-outs of a bundle: `InfraSet` takes the support, `InfraEffectivePoint` the occupation.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {seg = FindInfraSegment[g, 1, 16]},
  {InfraSet[seg]["Length"], InfraEffectivePoint[seg]["Mass"]}]
```

Repetition in a list of atoms is mass; a set, by contrast, deduplicates.

```wl
{InfraEffectivePoint[{InfraPoint[4], InfraPoint[4], InfraPoint[9]}], InfraSet[{4, 4, 9}]}
```

The all-ones measure stays a measure — the layers never cross silently.

```wl
Head @ InfraEffectivePoint[InfraSet[{4, 9}]]
```
