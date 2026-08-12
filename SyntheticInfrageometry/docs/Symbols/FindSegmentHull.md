---
Template: Symbol
Name: FindSegmentHull
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindSegmentHull
---

## Usage

`FindSegmentHull[graph, S]` returns the smallest superset of S closed under MetricInterval (the segment operator), as an InfraSet.

## Details & Options

Option "LineStructure" (None (default), or an InfraLineStructure / list of lines) closes under the chosen-geodesic stretch on that fixed family instead of all geodesics.

S is any Infra* object, a list of them, or a bare vertex list.
