---
Template: Symbol
Name: InfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPoint
---

## Usage

`InfraPoint[{v1, ..., vk}]` is the multi-realisation point -- a candidate set in superposition.

`InfraPoint[<|v -> m, ...|>]` is the measured form: the measure on vertices, stored as the association it is (InfraPoint is the only measured wrapper, since its realisations are vertices and so its vertex marginal is lossless).

`InfraPoint[{v1, ...}, {m1, ...}]` is input sugar for the association; repeated vertices sum and an all-ones measure collapses to the bare support.

## Details & Options

Accessors ["Support"], ["Weights"], ["Mass"], ["First"], plus graph-keyed synthetic invariants (["BallVolumes", g], ["Dimension", g], ...) which read the support only.

Measured points are constructed at projections -- seg[[i]], FindInfraMidpoint, FindInfraShellCenter.

Scene-language constructors InfraPoint[] / InfraPoint["Center"] / InfraPoint[origin, dist] are used inside InfraScene.
