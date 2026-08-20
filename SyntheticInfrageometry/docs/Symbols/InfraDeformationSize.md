---
Template: Symbol
Name: InfraDeformationSize
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraDeformationSize
---

## Usage

`InfraDeformationSize[ref, walk]` returns how many edges of ref the walk replaces: Length[ref] - 1 less the shared prefix and the shared suffix. It is one-sided, measured in ref's edges, so it is not a symmetric walk-space metric. An InfraWalk bundle gives one number per realisation, which is what makes call-site grouping (GroupBy) and extremising (SelectInfraWalk with "From" -> {"Min", scoreFn}) work without a query axis on FindInfraMonotoneDeformation.
