---
Template: Symbol
Name: CATInequalityQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/CATInequalityQ
---

## Usage

`CATInequalityQ[graph, {p, q, r}, k : 0]` tests whether the geodesic triangle on {p, q, r} satisfies the CAT(k) thinness inequality.

## Details & Options

Option Method ("ApexSide" (default) -- d(apex, x) <= d_k_bar(apex', x') for every interior vertex x on the side opposite to each apex; "TwoRays" -- d(x, y) <= d_k_bar(x', y') for every cross-ray pair (x, y) on the two rays emanating from each apex).

Returns Indeterminate when k > 0 and the triangle perimeter exceeds 2 Pi / Sqrt[k].
