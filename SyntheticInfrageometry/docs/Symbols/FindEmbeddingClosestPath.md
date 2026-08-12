---
Template: Symbol
Name: FindEmbeddingClosestPath
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindEmbeddingClosestPath
---

## Usage

`FindEmbeddingClosestPath[graph, curve]` snaps an arbitrary embedded curve to a graph walk and returns InfraPath[{walk}] tracing it: each sampled curve point is mapped to its nearest vertex under the graph embedding, consecutive repeats are dropped, and successive anchors are joined by geodesics. curve is a Line / BSplineCurve / BezierCurve or a list of plane points in the embedding's coordinates.

## Details & Options

Unlike EmbeddingClosest (which selects from a supplied bundle), this constructs the path, so it works for shapes with no enumerable bundle (spirals, figure-eights).
