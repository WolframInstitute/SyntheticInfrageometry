---
Template: Symbol
Name: FindInfraPath
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPath
---

## Usage

`FindInfraPath[graph, p1, p2, kspec]` returns one InfraPath wrapper of walks p1 -> p2 (non-simple allowed) with length restricted by kspec (k | {k} | {kmin, kmax} | Infinity); trailing count n returns exactly n realisations or $Failed, UpTo[n] up to n, All (default) all.

## Details & Options

Options Properties ("Simple", "ShortestPath", "LongestPath", {"EdgeMin", f}, {"EdgeMax", f}), Method ("Exhaustive").
