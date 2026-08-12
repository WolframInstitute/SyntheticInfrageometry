---
Template: Symbol
Name: InfraCurvature
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraCurvature
---

## Usage

`InfraCurvature[graph, v]` returns the local Alexandrov upper-curvature bound at v: L^2 times the supremum of per-triangle CAT bounds over all triangles whose three vertices sit inside B_L(v), with L = GraphDiameter[graph] by default.

`InfraCurvature[graph]` returns an Association[v -> kappa_v] over all vertices.

## Details & Options

Option: "Radius" (Automatic | Integer L) selects the ball radius; reported curvature is rescaled by L^2 to match the (edge / L)^-2 unit convention.
