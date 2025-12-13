# nimony

Nimony is a new Nim implementation that is in heavy development. See the [manual](https://nim-lang.github.io/nimony-website/) for up to date documentation. See [design.md](https://github.com/nim-lang/nimony/blob/master/doc/design.md) for lots of implementation details.
There is a [blog post](https://nim-lang.org/araq/nimony.html) about its design principles.

The current focus is on developing a compiler for a Nim dialect that offers:

- Incremental recompilations.
- Fully parallel builds.
- No forward declarations for procs and types required.
- Type-checked generics.
- Good editor support.

AI has created a good overview of our [compiler architecture](https://deepwiki.com/nim-lang/nimony).


## Getting started

Clone Nimony:

```
git clone https://github.com/nim-lang/nimony.git
cd nimony
```

Nimony uses a tool called `hastur` to build:

```
nim c -r src/hastur build all
```

The command above must be executed from the `nimony` directory with the
system Nim 2.2.6 compiler on your `PATH`. It produces the suite of helper
executables under `bin/` (including the `nimony` compiler itself) and can be
rerun whenever you pull new changes. The generated binaries are intentionally
left out of version control; rebuild them locally as needed.


## Hello World

`echo` is not part of `system.nim` anymore, so the hello world program is:

```nim

import std / syncio

echo "hi"
```

To compile and run it with the freshly built `nimony` compiler, write the
snippet to a file (for example `hello.nim`) and invoke:

```
./bin/nimony c -r hello.nim
```

The `-r` flag runs the produced binary after compilation, making it easy to
confirm the toolchain works end-to-end.
