---
Template: Symbol
Name: FindInfraRay
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraRay
---

## Usage

`FindInfraRay[graph, O, v]` returns {InfraRay[{ray}]} for one pointed half-line from O in v's direction: the half of a maximal geodesic line through O and v starting at O and ending at the far inextensible endpoint.

`FindInfraRay[graph, O, v, n]` returns a List of n unary InfraRay[{ray}] wrappers or $Failed; UpTo[n] / All controls multiplicity.

## Details & Options

O and v accept InfraPoint[{...}] for multi-anchor spread.
