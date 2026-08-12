---
Template: Symbol
Name: FindInfraReflection
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraReflection
---

## Usage

`FindInfraReflection[graph, x, a]` returns InfraPoint[{x1', x2', ...}], the reflections of x through a in superposition, where each x' satisfies B(x, a, x') and ax == ax'.

`FindInfraReflection[graph, x, a, n]` returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all.
