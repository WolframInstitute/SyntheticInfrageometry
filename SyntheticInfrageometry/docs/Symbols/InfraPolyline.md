---
Template: Symbol
Name: InfraPolyline
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPolyline
---

## Usage

`InfraPolyline[{poly}]` is the unary form: poly = {seg1, ..., segk} is a list of unary InfraSegment[{path_i}] with consecutive legs sharing their endpoint (Last[path_i] == First[path_{i+1}]).

`InfraPolyline[{poly1, ..., polym}]` is the multi-realisation form.

## Details & Options

Consumed by InfraSceneHighlight (each realisation flattens to one concatenated vertex sequence; sequential-edge semantics).
