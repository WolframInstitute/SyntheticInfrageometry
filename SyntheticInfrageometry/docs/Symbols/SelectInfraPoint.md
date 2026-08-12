---
Template: Symbol
Name: SelectInfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/SelectInfraPoint
---

## Usage

`SelectInfraPoint[graph, vertices]` draws one vertex from the bundle under graph distance.

`SelectInfraPoint[graph, vertices, n]` returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole filtered pool.

## Details & Options

Operator form SelectInfraPoint[graph, n, opts][vertices].

Options mirror FindInfraPoint ("From", "Distance", "MaxCliques"); the bundle may be a vertex list or any set-like Infra* wrapper.
