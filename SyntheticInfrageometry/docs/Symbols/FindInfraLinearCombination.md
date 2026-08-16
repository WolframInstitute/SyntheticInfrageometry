---
Template: Symbol
Name: FindInfraLinearCombination
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraLinearCombination
---

## Usage

`FindInfraLinearCombination[graph, o, {{lambda1, u1}, {lambda2, u2}, ...}]` returns InfraSet[{v1, ...}], the multi-valued vertex realisation of sum_i lambda_i u_i with base point o, computed as scaled-then-pairwise-summed left-to-right.

## Details & Options

Options: "ScaleMethod" (Automatic (default), "Metric", "Line", "Midpoint"); "SumMethod" ("Metric" (default), "Parallel").

The count argument n / UpTo[n] / All (default) picks realisations, exact n failing with $Failed.
