---
Template: Symbol
Name: InfraSceneHighlight
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSceneHighlight
Keywords: [visualization, diffuse rendering, multiplicity, highlight, palette]
SeeAlso: [InfraSceneViewer, InfraScene, $InfraPalette, InfraMeasure, InfraPoint]
RelatedGuides: [VisualizationGuide]
---

## Usage

<code>[InfraSceneHighlight]()[*g*, *multiObjects*]</code> draws multi-objects on *g*. Intensity follows multiplicity. Colours blend across objects.

<code>[InfraSceneHighlight]()[*g*, *obj*]</code> draws a single object.

## Details & Options

This is the only rendering primitive in the paclet. Every viewer and every figure calls it.

The principle is diffuse rendering. A construction is a family, not a choice. So the picture shows the whole family, and multiplicity sets the intensity. An edge on many geodesics is drawn strongly. An edge on few is drawn faintly. No single realisation is privileged.

Objects are given as `obj -> style` entries. With no style, the colour comes from the wrapper head via [$InfraPalette](): points red, segments orange, circles teal, and so on.

Options:

| Option | Values | Default |
|---|---|---|
| `"OpacityRange"` | `None`, a scalar, or `{min, max}` | `{0.4, 1.}` |
| `"ThicknessRange"` | `None`, a scalar, or `{min, max}` | base `9.` |
| `"PointSizeRange"` | `None`, a scalar, or `{min, max}` | base `14` |

`None` turns diffusion off and draws every realisation alike. A scalar sets the base measure. A pair sets the envelope.

A per-object override goes on the right of the arrow: a colour, a `Directive`, or a list of options. Bare directives are routed by family. Thickness and dashing go to edges. Point size goes to vertices. Colours and opacity go to both.

Two things to watch.

An explicit appearance directive suppresses the matching diffusion. A `Thickness` directive turns off `"ThicknessRange"`, and the multiplicity information with it.

Ambient styling belongs on the graph, not here. Use `Graph[g, Sequence @@ AmbientGraphStyle["Gray"]]`, and set uniform dot size top-level with `VertexShapeFunction`. A per-entry `AbsolutePointSize` inside the highlight list has no effect.

## Basic Examples

A mixed scene: the geodesic bundle, its endpoints, and its midpoints. Each head takes its palette colour.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  InfraSceneHighlight[g,
    {FindInfraSegment[g, a, b, All] -> $InfraSegmentColor,
     FindInfraMidpoint[g, a, b] -> $InfraCircleColor,
     InfraPoint[{a, b}] -> $InfraPointColor},
    "PointSizeRange" -> 18,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 380]]
```

The same bundle at three opacity settings. `None` draws every geodesic alike. A wider envelope exaggerates the contrast.

```wl
Row[Table[
   With[
     {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraSegment[g, a, b, All] -> $InfraSegmentColor},
         "OpacityRange" -> spec,
         VertexShapeFunction -> ({AbsolutePointSize[2], Point[#]} &),
         ImageSize -> 240],
       Text[ToString[spec]]]],
   {spec, {None, {0.4, 1.}, {0.05, 1.}}}]]
```

## Properties and Relations

The colour of each head is a row of [$InfraPalette]().

```wl
Normal @ $InfraPalette[Select[MemberQ[#Heads, InfraCircle] &], {"Primitive", "Symbol"}]
```
