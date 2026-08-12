---
Template: Symbol
Name: InfraSceneHighlight
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSceneHighlight
---

## Usage

`InfraSceneHighlight[g, multiObjects]` renders multi-objects diffusely on g: intensity scales with within-object multiplicity, colors blend across objects; InfraSceneHighlight[g, obj] is the single-object shortcut.

## Details & Options

Options "OpacityRange", "ThicknessRange", "PointSizeRange" (each None | scalar base measure | {min, max} envelope).

Per-object style overrides via entry -> color | Directive | {opts}.
