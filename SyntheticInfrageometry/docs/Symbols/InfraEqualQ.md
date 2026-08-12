---
Template: Symbol
Name: InfraEqualQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraEqualQ
---

## Usage

`InfraEqualQ[graph, a, b]` tests equality of two Infra* wrappers via their diffusion diagrams (vertex -> total-occurrence multisets).

## Details & Options

Options:

| Option | Values |
|---|---|
| `Method` | "Diffuse" (default; \|A cap B\| > \|A delta B\|, equivalently weighted Jaccard > 1/2) \| "Overlap" (at least one common vertex) \| "Set" (vertex sets identical) \| "Multiset" (diffusion diagrams identical) |

Returns False on head mismatch.
