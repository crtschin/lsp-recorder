style:
  find . -name "*.hs" | xargs -I {} fourmolu --mode inplace {}
  find . -name "*.nix" | xargs -I {} nixfmt {}
  find . -name "*.py" | xargs -I {} ruff format {}
  find . -name "*.py" | xargs -I {} ruff check {}
  find . -name "*.cabal" | xargs -I {} cabal-fmt -i {}

build:
  cabal build all

test:
  cabal test

bench *ARGS:
  ./bench/run-benchmarks.sh {{ARGS}}

report input_dir output_dir=(input_dir / "reports"):
  python3 bench/aggregate-reports.py --input-dir {{input_dir}} --output-dir {{output_dir}}
  python3 bench/plot-charts.py --input-dir {{output_dir}} --output-dir {{output_dir}}/charts
