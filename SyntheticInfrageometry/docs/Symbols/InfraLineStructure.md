---
Template: Symbol
Name: InfraLineStructure
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraLineStructure
---

## Usage

`InfraLineStructure[{line1, ..., linek}]` is a consistent geodesic path system, stored as its maximal lines (the structure's realisations).

## Details & Options

Accessors: ["Lines"] / ["Realizations"] the maximal lines, ["First"] / [[i]] one line, ["Length"] per-line edge counts; ["Paths"] the unfolded association <|{u,v} -> P(u,v)|>, ["Incidence"] the transpose <|v -> {line numbers}|>, ["Coordinates"] <|v -> {{line, offset}}|> (offset = edges from the line start), and ["Path", u, v] recovers P(u,v) oriented u -> v.
