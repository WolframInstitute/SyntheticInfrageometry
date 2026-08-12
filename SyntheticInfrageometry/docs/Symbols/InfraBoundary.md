---
Template: Symbol
Name: InfraBoundary
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraBoundary
---

## Usage

`InfraBoundary[g, s]` is the boundary of the vertex set s (a bare vertex list or any Infra* object) in g, returned as an InfraSet.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Method` | "Combinatorial" (default), the inner vertex boundary via GraphBoundary; {"Alexandrov", "Radius" -> r}, the two-sided cl(s)\int(s) in the closed-r-ball topology, default r = 1 |
