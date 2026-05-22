
# Regenesca

Refactoring Generator of Source Code for Scala

"Refactoring" because it **does not blindly overwrite** previously generated files.  
It only *changes what it needs to*.  

This means you can **add your own code into the generated one**, as long as it *does not clash* with it.
The goal is to have a *minimally intrusive* code generator.

## How to use it?

See the [examples](/examples) folder

`RegenescaGenerator` also supports dry-run preview via `generatePreview(...)`, which returns merged source text and change flags without writing files.


## How it works?

Regenesca is based on [Scalameta](https://scalameta.org/).

The principle is simple.  
You tell it what *source code* should be contained in a *particular file*, and then:
- if file doesn't exist -> write that code into it
- if file exists -> merge the existing scala code in a *mostly ad-hoc* way to achieve 99% of what you wanted with your code generator

The merge looks roughly as follows:
- same-named `class` -> merge their contents
- same-named `object` -> merge their contents
- same-named `enum` -> overwrite it completely
- same-named `type` -> overwrite it completely
- same-named `val`/`var` -> overwrite it completely
- same-named `def` -> merge internally OR overwrite it completely (global flag)
- `import` is added if a same one doesn't exist
- `case`s are merged by their patterns
- comments are not preserved
- it ignores expressions when merging

### Supported merge behavior (currently)

- **Definitions merged by key**
  - `class`/`object`/`trait` by name
  - `def` by full signature (supports overload-safe matching)
  - `val`/`var` by extracted variable names from patterns
  - `enum`/`type`/`given`/`given alias` by structural keys
- **Case branches**
  - merged by pattern + guard + body shape key
- **For-comprehensions**
  - configurable strategy:
    - preserve user expressions while merging qualifiers (default)
    - overwrite whole comprehension

### Unsupported / intentionally conservative behavior

- comments/scaladoc are not preserved
- arbitrary expressions are generally preserved from user code (not fully rewritten)
- deletion of stale generated members is not automatic by default

### Merge strategy trade-offs

- `mergeDefBodies = true` (default): minimizes user-code loss, but preserves many user expressions.
- `mergeDefBodies = false`: stronger generator control, but can overwrite user edits inside defs.
- `forComprehensionMergeStrategy = PreserveUserExpressions` (default): safer for manual edits.
- `forComprehensionMergeStrategy = OverwriteComprehensionFully`: strongest regeneration, least preservation.

## Adopters

- [Squery generator](https://github.com/sake92/squery) from version 0.6.0
- [OpenApi4s generator](https://github.com/sake92/openapi4s)
