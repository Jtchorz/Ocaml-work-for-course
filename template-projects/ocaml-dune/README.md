# OCaml Dune Template

This an alternative OCaml template for the ID2202 course using the
[`dune`](https://dune.readthedocs.io/en/latest/quick-start.html) build system.
Like the other templates, building happens with `make`, running with `./hello`.
To install `dune`, it is easiest to first install
[`opam`](https://opam.ocaml.org/) and then run

```bash
opam install dune
```

One benefit of using this template, as opposed to the template using
`ocamlbuild`, is that you can get a language server working in for example
[vscode](https://code.visualstudio.com/) via the [ocaml
platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
extension. This will provide features such as auto-completion, linting, jump to
definition, documentation on hoover, and type information on hoover.

To install the language server run

```bash
opam install ocaml-lsp-server
```

To use this template for an assignment, modify the following line in the `Makefile`:

```
executable=hello
```

For example, if you want to name your executable `calc`, then the lines become:"

```
executable=calc
```

Remember to ignore the new file in `.gitignore`.
