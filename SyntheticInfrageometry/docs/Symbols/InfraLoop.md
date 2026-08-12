---
Template: Symbol
Name: InfraLoop
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraLoop
---

## Usage

`InfraLoop[{walk}]` is the unary form (one closed walk, First === Last); InfraLoop[{walk1, ..., walkk}] is the multi-realisation form.

## Details & Options

Inside InfraScene, InfraLoop[v1, v2, ..., vk] is the scene-DSL constructor (auto-closes if needed).

Open-walk realisations are auto-closed by appending First.

Used by the polymorphic homotopy finders as the base-pointed loop wrapper (homotopy fixes the base vertex unless "FreeHomotopy" -> True).
