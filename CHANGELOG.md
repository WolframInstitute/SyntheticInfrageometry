# Changelog

## 0.8.2

- New public `InfraMode[graph, infra]` — picks the most-visited realisation(s) from any single-`_List`-arg `Infra*` wrapper (point, segment, line, shell, plane, circle, ray, pencil), the single-realisation readout of the diffuse measure that `InfraSceneHighlight` paints. Same engine exposed bundle-level as a new `"MostVisited"` criterion on `SelectPaths` / `SelectCycles`.

## 0.8.1

- Concise usage-message style: every `::usage` is one sentence per signature, no inline tutorials.
- Retire `Tessellations` from the kernel; the corresponding wiki entry is archived.
- Documentation: `Layer` -> `Geometry` rename across guide notebooks.

## 0.8.0

- Projective layer aligned with the `Find*` -> `Infra*` multi-object pattern used by the Euclidean and Tropical layers.
- New wrapper heads `InfraRay` (multi-realisation) and `InfraPencil` (multi-constituent).
- `FindRay` (formerly the roster's `FindRayClass`); `FindCommonLine` / `FindCommonPoint` accept `InfraPoint` / `InfraSegment` / `InfraRay` / `InfraPencil` anchors.
- New predicate `UniqueConcurrentQ`.

## 0.7.3

- Rename `Aggregation` -> `SelectCoordinate` in `OrthogonalCoordinates`; bare-symbol values (`First`, `Min`, `Median`, ...); add `All` for tied-list preservation.

## 0.7.2

- `Find*` wrapper pass: `FindPoint` / `FindSegment` / `FindLine` / `FindShell` / `FindCircle` return `Infra*` heads with consistent accessors.
- Tropical operations split into a dedicated `TropicalOperations.wl`.
- Option rename pass for consistency with Wolfram conventions.

## 0.6.0

- Euclidean API cleanup: `FindParallel` placeholder allow-list entries `"Spectral"` / `"Resistance"` removed (only `"Metric"` and `"Embedding"` remain).
- `InfraInstance` accessor overloads `InfraInstance[inst, sym]` / `InfraInstance[inst, {sym1, ...}]`.
- `Viewers.wl` split into `Highlights.wl` (diffuse-rendering primitive `InfraSceneHighlight`) + `Viewers.wl` (`Manipulate`-based interactive viewers).

## 0.5.x

See git log for the v0.5 series (curvature engine, Tarski layer, `PathSpace.wl`, `FindShell` / `FindCircle` split, `Curvatures.wl`).
