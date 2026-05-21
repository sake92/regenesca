# AGENTS.md — Regenesca

## Build system: Deder (NOT SBT)

- Install: `brew install sake92/tap/deder`
- Build config: `deder.pkl` (Pkl format)
- Scala version: **2.13.18** (compilation and runtime)

### Commands

| Task | Command |
|------|---------|
| Compile | `deder -t compile` |
| Run tests | `deder -t test` |
| Publish local | `deder -t publishLocal` |
| Run hello example | `deder -t run -m hello` |
| Run migration example | `deder -t run -m migration` |
| Clean | `deder clean` |

## Architecture

- **Package:** `ba.sake.regenesca`
- **Language:** Scala 2.13 (Scala 2 source, not Scala 3)
- **Dependencies:** Scalameta (AST), Scalafix-core 0.14.2 (patch-based rewriting)

### Key types

- `RegenescaGenerator` — top-level orchestrator. Takes `SourceMerger`, calls `generate(Seq[GeneratedFileSource])`
- `SourceMerger` — the merge engine. Accepts `mergeDefBodies: Boolean` (default `true`) and `ForComprehensionMergeStrategy` (default `PreserveUserExpressions`)
- `GeneratedFileSource(path: Path, source: Source)` — pairs a file path with a Scalameta `Source` AST

### How merging works

The merger builds Scalafix `Patch` operations (replace, add, addGlobalImport) and applies them via `PatchInternals.tokenPatchApply` to rewrite the *original* token stream. It is **not** string-based — it's structural/AST-level.

Merge rules:
- `class`/`object`/`trait` — merge member bodies recursively
- `def` — match by full signature; merge bodies (or overwrite if `mergeDefBodies=false`)
- `val`/`var` — match by name; overwrite
- `enum`/`type` — overwrite entirely
- `given` — match by composite key; overwrite
- `import` — add if not already present
- `case` branches — merge by pattern
- Expressions inside method bodies are preserved (not managed)
- User-added members not in generated source are left untouched

## Tests

- **Framework:** MUnit 1.0.1 (`org.scalameta::munit`)
- **Base class:** `BaseSuite` extends `munit.FunSuite`, provides `assertEqStructure(obtained: String, expected: Source)`
- Tests use Scalameta quasiquotes (`source"..."`) inline — no external fixture files
- `assertEqStructure` compares **AST structure**, not string equality (formatting differences are ignored)
- Idempotency is tested: `merge(merge(first, generated), generated) == merge(first, generated)`
- Test dialect is `Scala34`

## Formatting

- **Scalafmt 3.7.15**, `maxColumn = 120`, `runner.dialect = scala3`
- Note: scalafmt dialect is scala3 but the project compiles as Scala 2.13

## Release

```
VERSION="x.y.z"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main $VERSION
```

Pushing a tag triggers `.github/workflows/release.yml` which runs CI tests then publishes to Sonatype (Maven Central).

## CI

- `.github/workflows/ci.yml` — runs on push to `main` and PRs
- Installs deder via Homebrew, then `deder exec -t compile` and `deder exec -t test`
- `continue-on-error: true` is set (noted as temporary)
