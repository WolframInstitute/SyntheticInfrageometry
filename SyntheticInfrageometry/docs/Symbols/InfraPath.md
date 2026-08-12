---
Template: Symbol
Name: InfraPath
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPath
---

## Usage

`InfraPath[{walk}]` is the unary form (one walk, possibly non-simple); InfraPath[{walk1, ..., walkk}] is the multi-realisation form.

## Details & Options

FindInfraPath finders return one InfraPath wrapper carrying the requested realisations.

Inside InfraScene, InfraPath[p1, p2, ..., pk] is the step-wise scene-DSL constructor: every length-(k-1) walk {v1, ..., vk} with vi in pi and each consecutive pair (vi, v_{i+1}) a graph edge (non-simple chains kept).

Rendered like InfraSegment (sequential-edge semantics).
