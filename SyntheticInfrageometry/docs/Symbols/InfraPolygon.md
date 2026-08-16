---
Template: Symbol
Name: InfraPolygon
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPolygon
---

## Usage

`InfraPolygon[{poly}]` is the unary form, where poly = {seg1, ..., segn} is a closed chain of unary InfraSegment sides; InfraPolygon[{poly1, ..., polyk}] is the multi-realisation form.

## Details & Options

Accessors ["Sides"] (the InfraSegment legs per realisation), ["Length"] (perimeter edge count per realisation), ["Vertices"] (corner vertices as InfraPoint atoms).

Find* returns one wrapper carrying the requested realisations.
