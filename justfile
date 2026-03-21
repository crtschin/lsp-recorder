style:
  find . -name "*.hs" | xargs -I {} fourmolu --mode inplace {}
  find . -name "*.nix" | xargs -I {} nixfmt {}
  find . -name "*.cabal" | xargs -I {} cabal-fmt -i {}

build:
  cabal build all

test:
  cabal test
