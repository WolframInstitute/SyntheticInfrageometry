---
Template: Symbol
Name: FindLineHull
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindLineHull
---

## Usage

`FindLineHull[graph, S]` returns the smallest superset of S closed under the line operator (the maximal geodesics through each pair), as an InfraSet.

## Details & Options

Option "LineStructure" (None (default), or an InfraLineStructure / list of lines) closes under that fixed line family instead of all maximal geodesics.

S is any Infra* object, a list of them, or a bare vertex list.
