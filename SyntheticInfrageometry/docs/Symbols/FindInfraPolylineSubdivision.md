---
Template: Symbol
Name: FindInfraPolylineSubdivision
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPolylineSubdivision
---

## Usage

`FindInfraPolylineSubdivision[graph, path]` returns {InfraPolyline[{{seg1, ..., segk}}]} where the legs are the fewest geodesic InfraSegments whose knots are path-vertices and each leg is a shortest path since the previous knot.

## Details & Options

Option: "MaxLength" (Infinity (default) | numeric L) caps every leg's graph-length at L.
