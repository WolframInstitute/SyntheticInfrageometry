---
Template: Symbol
Name: FindInfraCircle
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraCircle
---

## Usage

`FindInfraCircle[graph, c, r]` returns one InfraCircle wrapper for the canonical infra-circle around c at radius r: the shortest separating cycle in the level surface and its ties (r scalar or {rmin, rmax}).

`FindInfraCircle[graph, c, r, n]` returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all.

## Details & Options

Option Properties (default {"Separating", "Shortest"}).
