---
Template: Symbol
Name: FindInfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPoint
---

## Usage

`FindInfraPoint[graph]` returns THE canonical point: the whole candidate pool in superposition, InfraPoint[pool].

`FindInfraPoint[graph, n]` returns a tuple of n mutually-constrained points as a List of unary wrappers, exactly n or $Failed; UpTo[n] returns up to n; All returns every vertex.

## Details & Options

Options:

| Option | Values |
|---|---|
| `"From"` | "Random" (default), "Center", "Periphery", anchor -> spec |
| `"Distance"` | d, {dMin, dMax}, "Max", "Spread" |
| `"MaxCliques"` | -- |
