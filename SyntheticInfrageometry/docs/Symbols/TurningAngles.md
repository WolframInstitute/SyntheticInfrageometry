---
Template: Symbol
Name: TurningAngles
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/TurningAngles
---

## Usage

`TurningAngles[graph, path]` returns the list of discrete exterior angles Pi - InfraAngle[graph, {v_{i-1}, v_i, v_{i+1}}] at each interior vertex of the polygonal curve path = {v_1, ..., v_k}; closed cycles (First[path] == Last[path]) include the wrap-around triple.

`TurningAngles[graph, polyline]` returns the list of turning angles at the knot vertices of an InfraPolyline (one list per realisation); geodesic-interior vertices contribute no turning by construction.
