---
Template: Symbol
Name: FindInfraCylinder
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCylinder
---

## Usage

`FindInfraCylinder[graph, axis, r]` returns InfraObject[set] for the constant-radius rotational set around axis.

## Details & Options

Defaults to Method -> "Balls" (the r-neighborhood of the axis, i.e. all v with d(v, c) <= r for some axis vertex c); pass Method -> "Voronoi" for the flat-capped variant.

Inherits remaining FindInfraRevolution options.
