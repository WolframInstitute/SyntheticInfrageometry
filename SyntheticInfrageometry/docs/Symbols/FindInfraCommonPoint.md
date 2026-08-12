---
Template: Symbol
Name: FindInfraCommonPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCommonPoint
---

## Usage

`FindInfraCommonPoint[graph, lines]` returns InfraPoint[{v1, v2, ...}] of the vertices on every listed line.

## Details & Options

Entries may be bare vertex sequences or InfraSegment / InfraRay wrappers.

The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed.
