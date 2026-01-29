{
  mkShell,
  treefmt,
  ocamlPackages,
}:
mkShell {
  inputsFrom = [ ocamlPackages.cron ];
  buildInputs =
    (with ocamlPackages; [
      ocaml-lsp
      ocamlformat
      utop
    ])
    ++ [ treefmt ];
}
