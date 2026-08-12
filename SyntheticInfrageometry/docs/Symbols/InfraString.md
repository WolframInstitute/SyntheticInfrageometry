---
Template: Symbol
Name: InfraString
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraString
---

## Usage

`InfraString[{walk}]` is the unary form (one closed walk modulo cyclic rotation); InfraString[{walk1, ..., walkk}] is the multi-realisation form.

## Details & Options

Each realisation is stored as the lex-least cyclic rotation of Most[closeWalk[walk]] (the open canonical form, without the wrap-around vertex repetition); orientation is preserved.

Equality between strings is SameQ on canonical forms.

Inside InfraScene, InfraString[v1, v2, ..., vk] is the scene-DSL constructor (auto-closes and canonicalises).

Used by the polymorphic homotopy finders as the free-loop wrapper (no base point; cyclic rotations are identified).
