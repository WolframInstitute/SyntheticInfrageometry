---
Template: Symbol
Name: InfraWalk
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraWalk
---

## Usage

`InfraWalk[p1, ..., pk]` inside InfraScene is the literal walk through p1, ..., pk.

## Details & Options

A walk itself is a Graph: a directed path on the position pairs {i, v}, a closed walk a directed cycle on them; Last /@ VertexList gives the vertex sequence.
