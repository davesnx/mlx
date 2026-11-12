# mlx

An OCaml syntax dialect which adds JSX expressions to the language.

```ocaml
let header ~title () =
  <header>
    <h1>title</h1>
  </header>

let page =
  <html>
    <body>
      <header title="Hello, world!" />
      <div>
        "Some content goes here"
      </div>
    </body>
  </html>
```

This code is transformed into the following OCaml code:
```ocaml
let header ~title () =
  header () ~children:[ h1 () ~children:[ title ] [@JSX]; ] [@JSX]

let page =
  html () ~children:[
    body () ~children:[
      header () ~title:"Hello, world!" [@JSX];
      div () ~children:[ "Some content goes here" ] [@JSX];
    ] [@JSX];
  ] [@JSX]
```

It is expected to use `mlx-pp` preprocessor with either a runtime lib which
provides the implementation of such functions or a ppx which which further
transforms `[@JSX]` attributes into the desired output.

## HTML names

A prop name can have more segments, joined by `-`, `:`, `.` or `::`, and can
start with `@`. A lowercase tag name can have more segments joined by `:`.

```ocaml
let _ = <div data-foo="a" x-on:click.prevent=f @click=g />
let _ = <x:html />
```

Such a name is not an OCaml identifier, so mlx replaces each of these
characters with `_` and keeps the exact name in an `[@mlx.name]` attribute:

```ocaml
let _ =
  div () ~children:[]
    ~data_foo:("a" [@mlx.name "data-foo"])
    ~x_on_click_prevent:(f [@mlx.name "x-on:click.prevent"])
    ~_click:(g [@mlx.name "@click"]) [@JSX]
let _ = (x_html [@mlx.name "x:html"]) () ~children:[] [@JSX]
```

A function receives the prop under the label, for example `~data_foo`. A ppx
can read the attribute to get the exact name.

Limits:

- The segments of a name must touch: `data - foo` is an error.
- Such a prop needs a value: `<div data-foo />` is an error.
- A name cannot start with a keyword or with `:`, so `class=` and `:class=`
  are errors. A keyword is valid after the first segment, as in `x-if`.

## Installation & Usage

Use the following commands to install the necessary packages:
```sh
opam install mlx ocamlmerlin-mlx
```

To make dune consider `.mlx` files as OCaml files you need to configure an mlx
dialect, put this in your `dune-project` file:
```lisp
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))
```

### Editor Support

#### VS Code
For VSCode and its forks (Cursor, Windsurf, etc.), install the [Official OCaml Platform extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform) (version 2.0.0 or higher). It supports mlx syntax highlighting and all LSP features on par with `.ml` files.

#### Neovim
For Neovim users, install plugin: https://github.com/ocaml-mlx/ocaml_mlx.nvim

#### Emacs
For Emacs users, install mlx-mode: https://github.com/ocaml-mlx/emacs-mlx

## Useful Links

- Example with Dream web framework: https://github.com/aantron/dream/pull/330
- Template ReasonReact project: https://github.com/andreypopp/melange-mlx-template
- mlx announcement on OCaml Discourse: https://discuss.ocaml.org/t/ann-mlx-syntax-dialect/15035
