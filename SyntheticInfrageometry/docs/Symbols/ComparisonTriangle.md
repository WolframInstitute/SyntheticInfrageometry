---
Template: Symbol
Name: ComparisonTriangle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/ComparisonTriangle
---

## Usage

`ComparisonTriangle[a, b, c]` returns the Wolfram Triangle in R^2 with side lengths {a, b, c} (a opposite p, b opposite q, c opposite r).

`ComparisonTriangle[a, b, c, "Curvature" -> k]` for k != 0 returns InfraComparisonTriangle[<|"Sides" -> ..., "Curvature" -> k, "Angles" -> {alpha_p, alpha_q, alpha_r}|>] in M_k^2.

`ComparisonTriangle[graph, p, q, r, "Curvature" -> k]` reads the side lengths from the graph.
