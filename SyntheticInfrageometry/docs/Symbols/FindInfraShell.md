---
Template: Symbol
Name: FindInfraShell
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraShell
---

## Usage

`FindInfraShell[graph, c, r]` returns one InfraShell wrapper for the metric shell { v : d(c, v) == r } (r may be {rmin, rmax}).

`FindInfraShell[graph, c, r, n]` returns exactly n realisations or $Failed; UpTo[n] up to n; All (default) all.

## Details & Options

Options Properties ("Separating", "Connected"), Method ("Exhaustive" (default), "Greedy").
