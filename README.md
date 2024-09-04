
# Regenesca

Refactoring Generator of Source Code for Scala

"Refactoring" because it **does not blindly overwrite** previously generated files.  
It only *changes what it needs to*.  

This means you can **add your own code into the generated one**, as long as it *does not clash* with it.
The goal is to have a *minimally intrusive* code generator.

It is best if you use it in combination with [Scalafmt](https://scalameta.org/scalafmt/), to minimize the git diff.

## How to use it?

See the [example](/example) folder


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

## Adopters

- [Squery generator](https://github.com/sake92/squery) from version 0.6.0
