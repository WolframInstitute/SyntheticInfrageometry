---
Template: Symbol
Name: InfraPolylineQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPolylineQ
---

## Usage

`InfraPolylineQ[graph, poly]` tests whether poly = {seg1, ..., segk} of unary InfraSegment wrappers is a valid polyline in graph: every leg is a geodesic and consecutive legs share an endpoint.

## Details & Options

Accepts the wrapped form InfraPolyline[{...}] as well (AllTrue over realisations).
