# Eclipse Common Lisp Compiler

Eclipse Common Lisp compiles Lisp source code to C, which is then compiled
by gcc and linked into the Eclipse executable. A byte-code compiler was
planned but never completed. There is no FASL format and no image
save/load facility.

## Quick Start

```lisp
;; From inside the Eclipse REPL:
(compile-file "mycode" :output-format :c)
```

This produces `mycode.c`. To link it into Eclipse:

1. Add `usrMycode();` to the init section of `c/eclipse.c`
2. Set `USERSOURCE = mycode.c` in `c/makefile`
3. Run `make` in the `c/` directory

## Building a Standalone Executable

You can compile Lisp code to C and produce a native executable that
includes your compiled Lisp. The executable links against the Eclipse
runtime libraries and includes the full Lisp system.

### Step-by-step Example

Suppose you have `myapp.lisp`:

```lisp
(defun main ()
  (format t "Hello from compiled Lisp!~%")
  (format t "2 + 3 = ~a~%" (+ 2 3)))
```

**1. Compile to C** (from the Eclipse REPL):

```lisp
(compile-file "myapp" 'output-format :c)
```

This produces `myapp.c` containing a function `usrMyapp` that registers
the compiled Lisp functions into the runtime.

**2. Edit `c/eclipse.c`** -- add the init call at line 123, where the
comment says `Init-functions for Eclipse-generated C files go here`:

```c
  clInit(0);
  clInitD();
  /* Init-functions for Eclipse-generated C files go here. */
  usrMyapp();       /* <-- add this */
```

You also need to declare the function before `main()`:

```c
extern void usrMyapp(void);
```

**3. Edit `c/makefile`** -- set `USERSOURCE`:

```makefile
USERSOURCE = myapp.c
```

Copy `myapp.c` into the `c/` directory (or adjust the path).

**4. Build:**

```bash
cd c
make
```

This produces the `eclipse` executable with your compiled code baked in.
When it starts, the compiled functions are available immediately without
needing to load any files.

### Custom Applications (replacing the REPL)

The file `c/eclipse.c` is the application entry point and is meant to be
customized. The default version launches the interactive REPL via
`clLispTopLevel()`, but you can replace that with your own logic.

For example, to build a non-interactive application:

```c
#include <eclipse.h>

extern void clInit(unsigned);
extern void clInitD(void);
extern void usrMyapp(void);
extern clObject clFuncall(clObject, ...);
extern clObject clIntern(clObject, clObject, ...);
extern clObject clCharpSimpleBaseString(const char *);
extern clObject clFdefinition(clObject, ...);

int main(int argc, char **argv) {
    clInit(0);
    clInitD();
    usrMyapp();    /* register compiled functions */

    /* Call your Lisp function directly: */
    clObject sym = clIntern(clCharpSimpleBaseString("MAIN"),
                            clNIL, clEOA);
    clFuncall(clFdefinition(sym, clEOA), clEOA);
    return 0;
}
```

Then link as usual:

```bash
gcc -I. -g -L. myeclipse.c myapp.o -o myapp \
    -leclipse -leclipsed -lm -ldl -lgc
```

The resulting `myapp` is a standalone native executable. It still
contains the full Eclipse runtime (garbage collector, type system, CLOS,
etc.) but does not need any Lisp source files at run time.

### Multiple Compiled Files

You can compile several Lisp files and link them all in:

```lisp
(compile-file "utils" 'output-format :c)
(compile-file "database" 'output-format :c)
(compile-file "main" 'output-format :c)
```

```makefile
USERSOURCE = utils.c database.c main.c
```

```c
  usrUtils();
  usrDatabase();
  usrMain();
```

Call the init functions in dependency order -- if `main.lisp` calls
functions defined in `utils.lisp`, initialize `usrUtils()` first.

## compile-file

```lisp
(compile-file input-file
              &key output-file
                   output-format    ; :c for C output (the only working format)
                   loader-name      ; init function name (defaults from filename)
                   if-exists        ; :supersede :rename :error etc.
                   verbose          ; controlled by *compile-verbose*
                   print            ; controlled by *compile-print*
                   external-format) ; :default :ascii etc.
```

Returns three values: output truename, warnings-p, failure-p.

The `output-format` parameter accepts `:c` for C code generation. The
values `:default`, `:bin`, and `:byte` select the byte-code backend, which
was never completed.

### Defined in

- `lisp/evaluation.lisp` -- `compile-file` entry point
- `lisp/file-walk.lisp` -- file-level compilation coordination

## compile

```lisp
(compile name &optional definition)
```

In-memory compilation of a single function. This compiles to an internal
representation used by the interpreter -- it does NOT generate C code.
Only `compile-file` produces C output.

## Compiler Pipeline

### Source Files

| File | Role |
|------|------|
| `lisp/walk-top.lisp` | Top-level form walker, dispatches on form type |
| `lisp/walk-special.lisp` | Special form handlers (let, if, block, etc.) |
| `lisp/walk-util.lisp` | Compiler infrastructure, environments, targets |
| `lisp/literal.lisp` | Literal value handling and load-forms |
| `lisp/c.lisp` | C identifier generation and naming rules |
| `lisp/c-walk.lisp` | C code generation backend |
| `lisp/file-walk.lisp` | File-level compilation coordination |
| `lisp/evaluation.lisp` | `compile-file` and `compile` entry points |

### Compilation Support Modules (*-comp.lisp)

| File | Role |
|------|------|
| `lisp/common-comp.lisp` | Basic form compilation |
| `lisp/prog-comp.lisp` | PROG, DO, BLOCK, TAGBODY |
| `lisp/cont-comp.lisp` | Control flow / continuations |
| `lisp/cont-comp2.lisp` | Additional control structures |
| `lisp/cond-comp.lisp` | IF, COND, CASE |
| `lisp/number-comp.lisp` | Numeric operations |
| `lisp/list-comp.lisp` | List operations |
| `lisp/clos-comp.lisp` | CLOS / method compilation |
| `lisp/struct-comp.lisp` | Structure definitions |
| `lisp/macro-comp.lisp` | Macro expansion |
| `lisp/env-comp.lisp` | Closures and environments |
| `lisp/more-compile.lisp` | Additional compilation support |

### Pipeline Stages

1. **Read** -- forms are read from the source file via `with-source`
2. **Walk** -- `walk1` dispatches each form: symbols become variable
   lookups, cons cells become function calls or special forms, atoms
   become literals
3. **Compile** -- special-form handlers and compilation support modules
   transform the walked form into compiler instructions
4. **Generate** -- `walk-instructions` calls the C backend
   (`c-walk.lisp`) to emit C source code
5. **Output** -- the generated `.c` file is written with includes,
   declarations, function bodies, and a loader/init function

### Compiler Environment

```
compiler-env
  functions         -- compiled function list
  inits             -- initialization code
  function-warnings -- warning hash table

file-compiler-env (extends compiler-env)
  constants          -- literal values
  basic-constants    -- pre-interned objects
  external-constants -- references to other files

c-file-compiler-env (extends file-compiler-env)
  include-files          -- #include directives (default: eclipse.h)
  function-declarations  -- C function prototypes
```

Defined in `lisp/walk-util.lisp`.

## Generated C Code Structure

A generated `.c` file contains, in order:

1. Header comment with timestamp
2. `#include <eclipse.h>`
3. Function prototypes for called functions
4. `extern` declarations for constants from other files
5. `static` declarations for local constants
6. Compiled function definitions
7. Loader/init function

### Naming Conventions

Lisp names are translated to C identifiers by `lisp/c.lisp`:

| Lisp | C | Rule |
|------|---|------|
| `FOO` | `usrFoo` | Remove dashes, capitalize words |
| `MY-FUNC` | `usrMyFunc` | Dashes removed, next letter capitalized |
| `ECLIPSE:BAR` | `clBar` | `eclipse` package uses `cl` prefix |
| `USER:BAZ` | `usrBaz` | `user` package uses `usr` prefix |
| `*MY-VAR*` | `my_var` | Variables: lowercase, underscores |

Special characters in names:
`-` becomes `_`, `+` becomes `PLUS`, `*` becomes `STAR`, etc.

### Function Calling Convention

All compiled functions use the variadic argument protocol defined in
`c/eclipse.h`:

```c
clObject usrMyFunc clVdecl(_ap)
{
  clObject arg1, arg2;
  { clBeginParse(_ap);
    clSetq(arg1,
      (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_1, clEOA)));
    clSetq(arg2,
      (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(I_2, clEOA)));
    if (_clVp(_ap)) clExtraArgs(clVargs(_ap), clEOA);
    clEndParse(_ap);
  }
  return(clValues1(clPlus(arg1, arg2, clEOA)));
}
```

Key macros:

| Macro | Purpose |
|-------|---------|
| `clVdecl(_ap)` | Declare variadic parameter list |
| `clBeginParse(_ap)` | Begin argument parsing |
| `clVp(_ap)` | Test if more arguments present |
| `clVpop(_ap)` | Pop next argument |
| `clVargs(_ap)` | Remaining arguments as a Lisp list |
| `clEndParse(_ap)` | End argument parsing |
| `clEOA` | End-of-arguments sentinel |
| `clSetq(var, val)` | Assignment (GC-safe) |
| `clValues1(val)` | Return single value |
| `clValues(v1, v2, ..., clEOA)` | Return multiple values |

### Closures

```c
clDeclareEnv(usrFoo);            /* declare closure environment */

clObject usrFoo clVdecl(_ap) {
  clUseEnv(usrFoo);              /* access captured environment */
  /* clEnv(0, var1) -- access captured Lisp variable */
  /* clCEnv(int, 1) -- access captured C value */
}
```

### Literal Values

Literals are handled by `lisp/literal.lisp`. Each literal gets a static
C variable initialized in the loader function. Types use `make-load-form`
to generate construction code:

- Characters: `(int-character <code>)`
- Symbols: `(intern <name> <package>)` or `(make-keyword <name>)`
- Strings: `clCharpSimpleBaseString("...")`
- Numbers: `clIntFixnum(n)` for fixnums

## Initialization Sequence

### System Boot

From `c/eclipse.c`:

```c
int main(int argc, char **argv) {
    clInit(0);       /* core runtime initialization */
    clInitD();       /* compiler/development module initialization */

    /* User-compiled init functions go here: */
    usrMycode();

    return(clLispTopLevel(...));
}
```

`clInit()` initializes the core runtime (defined in `c/interface.c`).

`clInitD()` initializes compiler and development modules (defined in
`c/initd.c`), calling init functions in sequence:
`clInitCommonComp`, `clInitProgComp`, `clInitContComp`, `clInitMacro`,
`clInitNumberComp`, `clInitListComp`, `clInitClosComp`, etc.

### Generated Init Functions

Each compiled file produces an init function (e.g., `clInitMycode`) that:

1. Creates interned constants (symbols, strings, numbers)
2. Builds closure objects for each compiled function
3. Registers functions via `cl_SETF_Fdefinition`

The `loader-name` parameter to `compile-file` controls the init function
name. By default it is derived from the filename:
`myfile.lisp` produces `clInitMyfile`.

## Makefile Integration

In `c/makefile`:

```makefile
USERSOURCE = myfile.c anotherfile.c

user-objects = $(USERSOURCE:.c=.o)

eclipse: $(l).a $(l)d.a eclipse.o $(user-objects)
    $(CC) $(CFLAGS) $(LDFLAGS) -L. eclipse.o -o eclipse \
          $(user-objects) -leclipse -leclipsed -lm -ldl -lgc
```

Set `USERSOURCE` to the list of generated `.c` files. They will be
compiled and linked into the Eclipse executable automatically.

## Build System Internals

The build system (`lisp/build.lisp`) uses the compiler to bootstrap
Eclipse itself. Key functions:

- `build-system` -- master build, controls phases
- `generator` -- calls `compile-file` with `:output-format :c`
- `generate-files` -- batch-generates C from Lisp sources
- `process-file` -- checks dates, skips if target is current

Build phases defined in `build.lisp`:

| Phase | Contents |
|-------|----------|
| `*run0*` | Common utilities, parameters, MOP init |
| `*dev0*` | Development compilation features |
| `*dev1*` | Compiler components |
| `*run1*` | Core runtime: kernel, symbols, classes, CLOS |
| `*run1a*` | Standard functions: arithmetic, hash, types, sequences |
| `*run1b*` | Environment and bindings |
| `*run2*` | Resources, conditions, data structures |
| `*run3*` | I/O: reader, printer, format |
| `*dev4*` | Debugging and compilation utilities |

## Interpreter

`eval` is defined in `lisp/evaluation.lisp`:

```lisp
(defun EVAL (form)
  (eval-env form))
```

`eval-env` supports `*evalhook*` and `*walkhook*` for custom evaluation.
The interpreter uses the same walker infrastructure as the compiler but
targets direct execution rather than C code generation.
