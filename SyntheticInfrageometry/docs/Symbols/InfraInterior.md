---
Template: Symbol
Name: InfraInterior
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraInterior
---

## Usage

`InfraInterior[g, s]` is the interior of the vertex set s (a bare vertex list or any Infra* object) in g, returned as an InfraSet.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Method` | "Combinatorial" (default), s minus its inner boundary via GraphInterior; {"Alexandrov", "Radius" -> r}, the topological interior in the closed-r-ball topology, default r = 1 |
