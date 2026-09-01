---
Template: Symbol
Name: InfraSceneViewer
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSceneViewer
Keywords: [viewer, interactive, construction, step, branch]
SeeAlso: [InfraScene, FindInfraScene, InfraSceneHighlight, InfraInstance, InfraGeometricStep]
RelatedGuides: [VisualizationGuide]
---

## Usage

<code>[InfraSceneViewer]()[*scene*, *graph*]</code> is an interactive view of an [InfraScene]() on a graph, stepped one construction step at a time.

<code>[InfraSceneViewer]()[*scene*, *graph*, *bindings*]</code> starts from an association of pre-fixed bindings.

## Details & Options

Definition: the viewer solves the scene one step at a time and draws the objects bound so far, through [InfraSceneHighlight]().

It exists because a construction is a sequence, not a picture. Reading Euclid I.10 as a single figure hides the order; stepping it shows which object each later one depends on.

Controls, in two rows.

| Control | Does |
|---|---|
| step arrows, label menu | move between construction steps |
| eye | hide or show the current step's object |
| Branch / Diffuse | show one realisation, or all of them overlaid |
| branch arrows | page through realisations of the current step |
| Fixed | lock the current branch as the starting point for later steps |

Branch and Diffuse are the two ways to look at a family. Diffuse draws every realisation at once, with intensity following multiplicity, which is the right view for seeing where an object is concentrated. Branch picks one, which is the right view for following a single construction through to the end.

Fixed matters on long constructions. Without it, each step re-solves against every branch of the previous ones and the instance count multiplies. Fixing a branch collapses that.

The rendering options `"OpacityRange"`, `"ThicknessRange"`, `"PointSizeRange"` and `ImageSize` are passed through to [InfraSceneHighlight]().

For a static figure, solve the steps yourself with [FindInfraScene]() and lay them out as a grid. That is what the last example does.

## Basic Examples

Build a scene and open the viewer on it. The result is a `Manipulate`.

```wl
ClearAll[a, b, cA, cB, u];
With[
  {g = InfraSubstrate["PlanePatch", "Medium", "KeepCoordinates" -> True]},
  {p1 = First @ GraphCenter[g]},
  {p2 = SelectFirst[VertexList[g], GraphDistance[g, p1, #] == 8 &]},
  {scene = InfraScene[{a, b, cA, cB, u},
     {InfraGeometricStep[{a == InfraPoint[p1]}, "point a"],
      InfraGeometricStep[{b == InfraPoint[p2]}, "point b"],
      InfraGeometricStep[{cA == InfraCircle[a, {4, 5}]}, "circle around a"],
      InfraGeometricStep[{cB == InfraCircle[b, {4, 5}]}, "circle around b"],
      InfraGeometricStep[{u == InfraIntersection[cA, cB]}, "they meet"]}]},
  Head @ InfraSceneViewer[scene, g]]
```

The first three steps as stills: the two points, then the circle about the first. This is what the viewer shows as you step, drawn without the interface.

```wl
ClearAll[a, b, cA];
With[
  {g = InfraSubstrate["PlanePatch", "Medium", "Gray", "KeepCoordinates" -> True]},
  {p1 = First @ GraphCenter[g]},
  {p2 = SelectFirst[VertexList[g], GraphDistance[g, p1, #] == 8 &]},
  Row[{
    Labeled[
      InfraSceneHighlight[g, {InfraSet[{p1, p2}] -> $InfraPointColor},
        "PointSizeRange" -> 18,
        VertexShapeFunction -> ({AbsolutePointSize[2], Point[#]} &), ImageSize -> 250],
      Text["points a and b"]],
    Labeled[
      InfraSceneHighlight[g,
        {FindInfraCircle[g, p1, {4, 5}] -> $InfraCircleColor,
         InfraSet[{p1, p2}] -> $InfraPointColor},
        "PointSizeRange" -> 18,
        VertexShapeFunction -> ({AbsolutePointSize[2], Point[#]} &), ImageSize -> 250],
      Text["circle around a"]]}]]
```
