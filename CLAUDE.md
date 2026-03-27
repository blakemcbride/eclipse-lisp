# CLAUDE.md

## Project Overview

Eclipse Common Lisp — an ANSI Common Lisp implementation written in C and Lisp. Originally by Howard Stearns (Elwood Corp), now maintained by Blake McBride. Includes an interpreter and a Lisp-to-C compiler. Unrelated to the Eclipse Java IDE. Licensed under BSD 2-Clause.

## Directory Structure

- `c/` — C runtime source (~80 files). Contains `makefile`, `eclipse.h` (main header), `eclipse.c` (entry point)
- `lisp/` — Lisp source (~115 files). `build.lisp` defines file loading order and build phases
  - `lisp/test/` — Test files
  - `lisp/present/` — HTML documentation
  - `lisp/non-build/` — Non-essential modules
- `doc/` — HTML documentation (`doc/eclipse/eclipse.htm` is the entry point)
- `bin/` — Precompiled binaries by platform (linux, windows, sparc, hppa)

## Build

**Requires:** Boehm-Demers-Weiser garbage collector (libgc), GCC, GNU Make.

```bash
cd c
make          # Build eclipse executable and libraries
make test     # Build and test
make setup    # Install to /usr/local
make clean    # Clean build artifacts
```

**Outputs:** `eclipse` (executable), `libeclipse.a`, `libeclipsed.a` (static libraries).

## Testing

Tests live in `lisp/test/`. Run from within the Eclipse interpreter. `lisp/test/tests.lsp` is the main test configuration covering types, predicates, control flow, symbols, characters, sequences, lists, arrays, strings, numbers, streams, I/O, etc.

## Code Conventions

### C
- Functions prefixed with `cl` (e.g., `clCons`, `clSymbolValue`)
- Lisp-to-C naming: remove dashes, capitalize words — `SYMBOL-VALUE` → `clSymbolValue`
- Internal/zero-overhead functions prefixed with `_cl`
- All public functions return `clObject`; argument lists end with `clEOA`
- Parameter handling via `clBeginParse`/`clEndParse` macros
- Copyright/license headers in every file

### Lisp
- Packages: `:eclipse`, `:eclipse-c`, `:user`
- `in-package` declarations at file start
- Feature-conditional code via `#+`/`#-` read macros
- kebab-case naming in source; CamelCase in generated C code
- `;;;` for section headers, `;;` for inline comments

### File Organization
- One concept per file, C and Lisp files paired by name (e.g., `symbol.c` ↔ `symbol.lisp`)
- Compiler-related files suffixed with `-comp`
- Build phases in `build.lisp`: run0 → dev0 → dev1 → run1 → run1a → run1b → run2 → run3 → dev4

## Known Issues

See `ISSUES.txt` — pointer/int type confusion on 64-bit systems in `exit_EXCEPTION` and `UnrecognizedControlTag()`.

## Platform Support

Primary: 64-bit Linux. Also: 32-bit Linux, macOS, Windows. Legacy: HP-UX, Solaris/SPARC.
