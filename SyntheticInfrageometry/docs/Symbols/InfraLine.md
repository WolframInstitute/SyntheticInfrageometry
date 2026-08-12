---
Template: Symbol
Name: InfraLine
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraLine
---

## Usage

`InfraLine[{line}]` is the unary form (one maximal geodesic); InfraLine[{line1, ..., linek}] is the multi-realisation form.

## Details & Options

FindInfraLine / FindInfraParallel / FindInfraCommonLine return one InfraLine wrapper carrying the requested realisations.

Rendered like InfraSegment (sequential-edge semantics).

Scene-language constructor InfraLine[p, q] is used inside InfraScene.

Single-index line[[i]] returns the weighted InfraPoint of the i-th position across realisations (mass = multiplicity; i may be negative); First[line] and multi-index line[[1,1]] keep the realisation-list / first-line meaning.
