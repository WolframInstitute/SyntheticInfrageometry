---
Template: Symbol
Name: FindInfraMonotoneDeformation
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraMonotoneDeformation
---

## Usage

`FindInfraMonotoneDeformation[graph, ref, deltaSpec]` returns the walks between ref's endpoints along which a potential never decreases, as one flat InfraWalk sorted by (length delta, deformation size). Under the default potential d(First[ref], .) every edge of the augmented spray is radial or transverse, so a deformation is a walk that never steps back toward the base point. deltaSpec bounds the edge count relative to ref (k up to k, {k} exact, {lo, hi} range, Automatic the minimal non-empty class) and is required: transverse edges let a monotone walk shuffle inside one level set forever. Option "Potential" -> Automatic | v | f sets the grading. See also InfraDeformationSize.
