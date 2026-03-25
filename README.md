# lsp-recorder

Transparent LSP proxy that records editor<->server communication to
newline-delimited json traces and allows replaying them.

## Install

I provide nix shells that facilitate working in this repo:

```bash
nix develop   # enters dev shell
cabal build all
```

Otherwise, cabal will get you started. The benchmark scripts require python with
some dependencies, check the [flake.nix](./flake.nix) file for a full list.

## Usage

### Record

Sit between your editor and a language server, logging all messages:

```bash
lsp-recorder record \
  --server-command "haskell-language-server-wrapper --lsp" \
  --trace-out trace.jsonl \
  --project-root .
```

Configure your editor to launch `lsp-recorder record ...` instead of the
language server directly. All options can also be supplied via a JSON config
file with `--config config.json`. See [here](./hls-config.json.example) for
an example that facilitates recording haskell-language-server traces.

The `snapshot` section controls which project files are captured alongside the
trace via glob patterns. This snapshot is embedded in the trace so that the
replay can restore exact file state the server originally saw.

### Replay

Replay a recorded trace against a fresh server and collect latency stats:

```bash
lsp-recorder replay \
  --trace trace.jsonl \
  --server-command "haskell-language-server-wrapper --lsp" \
  --report report.json
```

The report contains per-method p50/p95/p99 latency stats in JSON. Note that any
non-notification request that did not get a response within the timeout (for
example due to deduplication done by the language server) is omitted from the
statistics.

Server-emitted files (e.g. `.prof`, `.hp`, `.eventlog`) can be exported after
replay by adding an `export` section to the config:

```json
{
  "export": {
    "globs": ["*.prof", "*.hp", "*.eventlog"],
    "destination": "./profiling-output"
  }
}
```

Matched files from the server's working directory are copied to `destination`
(defaults to the caller's cwd when omitted).
