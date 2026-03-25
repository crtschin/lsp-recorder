#!/usr/bin/env python3
"""
plot-charts.py — Generate bar chart PNGs from benchmark CSVs.

Usage:
    python3 bench/plot-charts.py --input-dir ./bench-results --output-dir ./bench-results/charts
"""

import click
import csv
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

try:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import numpy as np
except ImportError:
    print(
        "Error: matplotlib is required. Install it or enter the nix dev shell.",
        file=sys.stderr,
    )
    sys.exit(1)

DPI = 150
FIG_SIZE = (10, 6)
MAX_MSG_LEN = 40


def resolve_commit_labels(commits: list[str], repo: Path | None) -> list[str]:
    """Map commit hashes to 'hash: truncated message' labels via git."""
    if repo is None:
        return commits
    labels = []
    for c in commits:
        try:
            msg = subprocess.check_output(
                ["git", "-C", str(repo), "log", "-1", "--format=%s", c],
                stderr=subprocess.DEVNULL,
                text=True,
            ).strip()
            if len(msg) > MAX_MSG_LEN:
                msg = msg[: MAX_MSG_LEN - 1] + "\u2026"
            labels.append(f"{c}: {msg}")
        except (subprocess.CalledProcessError, FileNotFoundError):
            labels.append(c)
    return labels


def read_csv(path: Path) -> list[dict]:
    if not path.exists():
        return []
    with path.open() as f:
        return list(csv.DictReader(f))


def save_fig(fig, path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=DPI, bbox_inches="tight")
    plt.close(fig)
    print(f"Written: {path}")


def plot_wallclock(input_dir: Path, output_dir: Path, repo: Path | None):
    rows = read_csv(input_dir / "wallclock.csv")
    if not rows:
        print("wallclock.csv not found or empty — skipping wallclock chart")
        return

    commits = [r["commit"] for r in rows]
    labels = resolve_commit_labels(commits, repo)
    means = [float(r["mean_s"]) for r in rows]
    stddevs = [float(r["stddev_s"]) for r in rows]

    fig, ax = plt.subplots(figsize=FIG_SIZE)
    x = range(len(commits))
    ax.bar(
        x,
        means,
        yerr=stddevs,
        capsize=4,
        color="steelblue",
        edgecolor="black",
        linewidth=0.5,
    )
    ax.set_xticks(list(x))
    ax.set_xticklabels(labels, rotation=45, ha="right")
    ax.set_xlabel("Commit")
    ax.set_ylabel("Wall-clock time (s)")
    ax.set_title("Replay wall-clock time per commit")
    ax.yaxis.grid(True, linestyle="--", alpha=0.7)
    ax.set_axisbelow(True)
    fig.tight_layout()
    save_fig(fig, output_dir / "wallclock.png")


def plot_total_duration(input_dir: Path, output_dir: Path, repo: Path | None):
    rows = read_csv(input_dir / "total_duration.csv")
    if not rows:
        print("total_duration.csv not found or empty — skipping total_duration chart")
        return

    commits = [r["commit"] for r in rows]
    labels = resolve_commit_labels(commits, repo)
    means = [float(r["mean_total_ms"]) for r in rows]
    mins = [float(r["min_total_ms"]) for r in rows]
    maxs = [float(r["max_total_ms"]) for r in rows]

    # Error bars: distance from mean to min/max
    err_low = [m - lo for m, lo in zip(means, mins)]
    err_high = [hi - m for m, hi in zip(means, maxs)]

    fig, ax = plt.subplots(figsize=FIG_SIZE)
    x = range(len(commits))
    ax.bar(
        x,
        means,
        yerr=[err_low, err_high],
        capsize=4,
        color="darkorange",
        edgecolor="black",
        linewidth=0.5,
    )
    ax.set_xticks(list(x))
    ax.set_xticklabels(labels, rotation=45, ha="right")
    ax.set_xlabel("Commit")
    ax.set_ylabel("Total replay duration (ms)")
    ax.set_title("Total replay duration per commit")
    ax.yaxis.grid(True, linestyle="--", alpha=0.7)
    ax.set_axisbelow(True)
    fig.tight_layout()
    save_fig(fig, output_dir / "total_duration.png")


def plot_methods(
    rows: list[dict], output_dir: Path, percentile: str, repo: Path | None
):
    """percentile is 'p50_ms' or 'p95_ms'."""
    # Collect data: {method: {commit: value}}
    commits_ordered: list[str] = []
    methods_ordered: list[str] = []
    data: dict[str, dict[str, float]] = defaultdict(dict)

    for row in rows:
        commit = row["commit"]
        method = row["method"]
        val = float(row[percentile])
        if commit not in commits_ordered:
            commits_ordered.append(commit)
        if method not in methods_ordered:
            methods_ordered.append(method)
        data[method][commit] = val

    n_methods = len(methods_ordered)
    n_commits = len(commits_ordered)
    if n_methods == 0 or n_commits == 0:
        return

    commit_labels = resolve_commit_labels(commits_ordered, repo)
    label_map = dict(zip(commits_ordered, commit_labels))

    x = np.arange(n_methods)
    width = 0.8 / n_commits
    colors = plt.cm.tab10.colors  # type: ignore[attr-defined]

    fig, ax = plt.subplots(figsize=FIG_SIZE)
    for i, commit in enumerate(commits_ordered):
        vals = [data[method].get(commit, 0.0) for method in methods_ordered]
        offset = (i - n_commits / 2 + 0.5) * width
        ax.bar(
            x + offset,
            vals,
            width,
            label=label_map[commit],
            color=colors[i % len(colors)],
            edgecolor="black",
            linewidth=0.3,
        )

    ax.set_xticks(x)
    ax.set_xticklabels(methods_ordered, rotation=45, ha="right")
    ax.set_xlabel("LSP method")
    ax.set_ylabel(f"Latency ({percentile})")
    ax.set_title(f"Per-method {percentile} latency per commit")
    ax.legend(title="Commit", bbox_to_anchor=(1.02, 1), loc="upper left")
    ax.yaxis.grid(True, linestyle="--", alpha=0.7)
    ax.set_axisbelow(True)
    fig.tight_layout()
    save_fig(fig, output_dir / f"methods_{percentile.replace('_ms', '')}.png")


@click.command()
@click.option(
    "--input-dir", required=True, type=click.Path(exists=True, path_type=Path)
)
@click.option("--output-dir", required=True, type=click.Path(path_type=Path))
@click.option(
    "--repo",
    default=None,
    type=click.Path(exists=True, path_type=Path),
    help="Path to the git repo whose commits are benchmarked (for resolving commit messages)",
)
def main(input_dir: Path, output_dir: Path, repo: Path | None):
    input_dir = input_dir.resolve()
    output_dir = output_dir.resolve()
    repo = repo.resolve() if repo else None
    print(input_dir, output_dir)

    plot_wallclock(input_dir, output_dir, repo)
    plot_total_duration(input_dir, output_dir, repo)
    methods_rows = read_csv(input_dir / "methods.csv")
    if not methods_rows:
        print("methods.csv not found or empty — skipping methods charts")
    else:
        plot_methods(methods_rows, output_dir, "p50_ms", repo)
        plot_methods(methods_rows, output_dir, "p95_ms", repo)


if __name__ == "__main__":
    main()
