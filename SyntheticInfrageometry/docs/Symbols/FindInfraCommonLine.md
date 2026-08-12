---
Template: Symbol
Name: FindInfraCommonLine
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCommonLine
---

## Usage

`FindInfraCommonLine[graph, vertices]` returns InfraLine[{line1, ...}] of the canonical lines containing every listed vertex.

## Details & Options

Entries may be bare vertices or InfraPoint / InfraSegment / InfraLine / InfraRay wrappers.

The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed.
