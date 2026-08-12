---
Template: Symbol
Name: FindInfraCycle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCycle
---

## Usage

`FindInfraCycle[graph, n]` returns n shortest simple cycles as unary InfraCircle[{cycle}] wrappers (sorted by length); UpTo[n] returns up to n; All returns all.

`FindInfraCycle[graph, {k}, n]` and FindInfraCycle[graph, {kMin, kMax}, n] restrict cycle length.
