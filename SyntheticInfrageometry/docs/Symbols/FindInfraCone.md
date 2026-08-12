---
Template: Symbol
Name: FindInfraCone
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCone
---

## Usage

`FindInfraCone[graph, axis, slope]` returns InfraObject[set] for the cone of given slope with apex at axis[[1]]: radii are slope * Range[0, Length[axis] - 1].

## Details & Options

Option "Apex" (First (default) | Last) flips the apex end; inherits remaining options from FindInfraRevolution.
