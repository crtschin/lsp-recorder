#!/usr/bin/env bash
# run-benchmarks.sh — Benchmark a language server across git commits using lsp-recorder replay.
#
# Usage:
#   ./bench/run-benchmarks.sh \
#     --trace /path/to/trace.jsonl \
#     --server-command "haskell-language-server-wrapper --lsp" \
#     --repo /path/to/language-server-repo \
#     --build-command "cabal build exe:hls" \
#     --commits "abc1234 def5678 HEAD" \
#     [--runs 5] \
#     [--warmup 1] \
#     [--output-dir ./bench-results] \
#     [--speedup-factor 1] \
#     [--timeout 60] \
#     [--no-restore] \
#     [--no-file-sync]

set -euo pipefail

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------
TRACE=""
SERVER_CMD=""
REPO=""
BUILD_CMD=""
COMMITS=""
RUNS=5
WARMUP=1
OUTPUT_DIR="./bench-results"
SPEEDUP_FACTOR=1
TIMEOUT=60
NO_RESTORE=0
NO_FILE_SYNC=0

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    --trace)           TRACE="$2";          shift 2 ;;
    --server-command)  SERVER_CMD="$2";     shift 2 ;;
    --repo)            REPO="$2";           shift 2 ;;
    --build-command)   BUILD_CMD="$2";      shift 2 ;;
    --commits)         COMMITS="$2";        shift 2 ;;
    --runs)            RUNS="$2";           shift 2 ;;
    --warmup)          WARMUP="$2";         shift 2 ;;
    --output-dir)      OUTPUT_DIR="$2";     shift 2 ;;
    --speedup-factor)  SPEEDUP_FACTOR="$2"; shift 2 ;;
    --timeout)         TIMEOUT="$2";        shift 2 ;;
    --no-restore)      NO_RESTORE=1;        shift ;;
    --no-file-sync)    NO_FILE_SYNC=1;      shift ;;
    *) echo "Unknown argument: $1" >&2; exit 1 ;;
  esac
done

# ---------------------------------------------------------------------------
# Validate required args
# ---------------------------------------------------------------------------
missing=()
[[ -z "$TRACE" ]]      && missing+=("--trace")
[[ -z "$SERVER_CMD" ]] && missing+=("--server-command")
[[ -z "$REPO" ]]       && missing+=("--repo")
[[ -z "$BUILD_CMD" ]]  && missing+=("--build-command")
[[ -z "$COMMITS" ]]    && missing+=("--commits")

if [[ ${#missing[@]} -gt 0 ]]; then
  echo "Error: missing required arguments: ${missing[*]}" >&2
  exit 1
fi

if ! command -v hyperfine &>/dev/null; then
  echo "Error: hyperfine not found in PATH" >&2
  exit 1
fi

LSP_RECORDER="$(cabal list-bin lsp-recorder 2>/dev/null)" || {
  echo "Error: could not resolve lsp-recorder binary via cabal list-bin" >&2
  exit 1
}

TRACE="$(realpath "$TRACE")"
REPO="$(realpath "$REPO")"
OUTPUT_DIR="$(realpath "$OUTPUT_DIR")"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ---------------------------------------------------------------------------
# Capture original ref for restore-on-exit
# ---------------------------------------------------------------------------
ORIG_REF="$(git -C "$REPO" rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
if [[ "$ORIG_REF" == "HEAD" ]]; then
  # Detached HEAD — capture the SHA instead
  ORIG_REF="$(git -C "$REPO" rev-parse HEAD)"
fi

restore_orig_ref() {
  if [[ "$NO_RESTORE" -eq 0 ]]; then
    echo "Restoring original ref: $ORIG_REF"
    git -C "$REPO" checkout "$ORIG_REF" || true
  fi
}
trap restore_orig_ref EXIT

# ---------------------------------------------------------------------------
# Resolve all commit refs to full SHAs upfront
# ---------------------------------------------------------------------------
declare -a SHAS=()
for ref in $COMMITS; do
  sha="$(git -C "$REPO" rev-parse "$ref")"
  SHAS+=("$sha")
  echo "Resolved $ref -> $sha"
done

mkdir -p "$OUTPUT_DIR"

# ---------------------------------------------------------------------------
# Per-commit loop
# ---------------------------------------------------------------------------
for SHA in "${SHAS[@]}"; do
  SHORT_SHA="${SHA:0:8}"
  COMMIT_DIR="$OUTPUT_DIR/$SHORT_SHA"
  echo ""
  echo "=========================================="
  echo "Benchmarking commit: $SHORT_SHA"
  echo "=========================================="

  # Checkout
  if ! git -C "$REPO" checkout "$SHA"; then
    echo "Error: checkout of $SHA failed (conflicting untracked files?). Aborting." >&2
    exit 1
  fi

  # Sync filesystem if requested (default on)
  if [[ "$NO_FILE_SYNC" -eq 0 ]]; then
    sync
  fi

  # Build
  mkdir -p "$COMMIT_DIR"
  echo "Building at $SHORT_SHA..."
  if ! direnv exec "$REPO" bash -c "cd $(printf '%q' "$REPO") && $BUILD_CMD"; then
    echo "Build failed for $SHORT_SHA — skipping."
    touch "$COMMIT_DIR/build_failed"
    continue
  fi

  # Counter file for the wrapper
  COUNTER_FILE="$COMMIT_DIR/.run_counter"
  echo "0" > "$COUNTER_FILE"

  # Write wrapper script — use printf '%q' for variable values so shell
  # metacharacters in paths/commands are safely quoted, then a quoted heredoc
  # for the logic (no variable interpolation in the heredoc itself).
  WRAPPER="$COMMIT_DIR/hyperfine-wrapper.sh"
  {
    printf '#!/usr/bin/env bash\nset -euo pipefail\n\n'
    printf 'COUNTER_FILE=%q\n' "$COUNTER_FILE"
    printf 'COMMIT_DIR=%q\n' "$COMMIT_DIR"
    printf 'TRACE=%q\n' "$TRACE"
    printf 'SERVER_CMD=%q\n' "$SERVER_CMD"
    printf 'SPEEDUP_FACTOR=%q\n' "$SPEEDUP_FACTOR"
    printf 'TIMEOUT=%q\n' "$TIMEOUT"
    printf 'LSP_RECORDER=%q\n' "$LSP_RECORDER"
    printf 'REPO=%q\n' "$REPO"
    cat <<'WRAPPER_EOF'

N=$(cat "$COUNTER_FILE")
echo $((N + 1)) > "$COUNTER_FILE"

REPORT="$COMMIT_DIR/report-run-$N.json"

cd "$REPO" && direnv exec "$REPO" "$LSP_RECORDER" replay \
  --trace "$TRACE" \
  --server-command "$SERVER_CMD" \
  --report "$REPORT" \
  --speedup-factor "$SPEEDUP_FACTOR" \
  --timeout "$TIMEOUT" \
  || touch "$COMMIT_DIR/run-$N-failed"
WRAPPER_EOF
  } > "$WRAPPER"
  chmod +x "$WRAPPER"

  # Run hyperfine
  echo "Running hyperfine ($RUNS runs, $WARMUP warmup)..."
  hyperfine \
    --runs "$RUNS" \
    --warmup "$WARMUP" \
    --export-json "$COMMIT_DIR/hyperfine.json" \
    "$WRAPPER"

  echo "Done: $SHORT_SHA"
done

# ---------------------------------------------------------------------------
# Restore original ref (also done by EXIT trap, but explicit here for clarity)
# ---------------------------------------------------------------------------
if [[ "$NO_RESTORE" -eq 0 ]]; then
  git -C "$REPO" checkout "$ORIG_REF"
  trap - EXIT
fi

# ---------------------------------------------------------------------------
# Aggregate + plot
# ---------------------------------------------------------------------------
echo ""
echo "Aggregating reports..."
python3 "$SCRIPT_DIR/aggregate-reports.py" \
  --input-dir "$OUTPUT_DIR" \
  --output-dir "$OUTPUT_DIR"

echo "Plotting charts..."
python3 "$SCRIPT_DIR/plot-charts.py" \
  --input-dir "$OUTPUT_DIR" \
  --output-dir "$OUTPUT_DIR/charts" \
  --repo "$REPO"

echo ""
echo "Results written to: $OUTPUT_DIR"
echo "Charts written to:  $OUTPUT_DIR/charts"
