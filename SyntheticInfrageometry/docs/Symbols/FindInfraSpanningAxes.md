---
Template: Symbol
Name: FindInfraSpanningAxes
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraSpanningAxes
---

## Usage

`FindInfraSpanningAxes[graph, n]` returns n mutually well-separated longest geodesics across graph (greedy, no fixed center) or $Failed; UpTo[n] returns up to n; All returns every axis above the separation threshold.

## Details & Options

For axes through a fixed center vertex use FindInfraOrthogonalFrame.

Options: "AxisDistance" ("MinEndpoint" | "Hausdorff" | "Separation"), "MinLength", "MinSeparation", "AxisThickness", "RandomPick".
