{
  mkShell,
  treefmt,
  ocamlPackages,
}:
mkShell {
  inputsFrom = [ ocamlPackages.croni ];
  buildInputs =
    (with ocamlPackages; [
      ocaml-lsp
      ocamlformat
      utop
    ])
    ++ [ treefmt ];
}
