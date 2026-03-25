#!/usr/bin/env python3
"""
aggregate-reports.py — Aggregate hyperfine JSON and lsp-recorder report JSONs into CSVs.

Usage:
    python3 bench/aggregate-reports.py --input-dir ./bench-results --output-dir ./bench-results
"""

import click
import csv
import json
import statistics
from pathlib import Path


def load_json(path: Path):
    with path.open() as f:
        return json.load(f)


def iter_commit_dirs(input_dir: Path):
    """Yield (short_sha, commit_dir) for each commit directory, skipping failed builds."""
    for d in sorted(input_dir.iterdir()):
        if not d.is_dir():
            continue
        if (d / "build_failed").exists():
            print(f"Skipping {d.name}: build_failed marker present")
            continue
        if not (d / "hyperfine.json").exists():
            continue
        yield d.name, d


def load_report_files(commit_dir: Path):
    """Load all report-run-N.json files that exist and are not marked failed."""
    reports = []
    for report_path in sorted(commit_dir.glob("report-run-*.json")):
        # Derive run index from filename
        stem = report_path.stem  # e.g. "report-run-3"
        run_idx = stem.split("-")[-1]
        failed_marker = commit_dir / f"run-{run_idx}-failed"
        if failed_marker.exists():
            print(f"  Skipping {report_path.name}: run-{run_idx}-failed marker present")
            continue
        try:
            reports.append(load_json(report_path))
        except json.JSONDecodeError as e:
            print(f"  Warning: could not parse {report_path}: {e}")
    return reports


def collect_all_data(input_dir: Path):
    """Single pass over commit dirs, returning row data for all three CSVs."""
    wallclock_rows = []
    methods_rows = []
    total_duration_rows = []

    for commit, commit_dir in iter_commit_dirs(input_dir):
        # Wallclock (from hyperfine)
        hf = load_json(commit_dir / "hyperfine.json")
        result = hf["results"][0]
        wallclock_rows.append({
            "commit": commit,
            "mean_s": result["mean"],
            "stddev_s": result["stddev"],
            "min_s": result["min"],
            "max_s": result["max"],
            "median_s": result["median"],
        })

        # Load report files once for both methods and total_duration
        reports = load_report_files(commit_dir)
        if not reports:
            print(
                f"  No valid reports for {commit}, skipping methods and total_duration CSV rows"
            )
            continue

        # Methods
        method_data: dict[str, dict[str, list[float]]] = {}
        for report in reports:
            for method, stats in report.get("methods", {}).items():
                if method not in method_data:
                    method_data[method] = {
                        "count": [],
                        "p50_ms": [],
                        "p95_ms": [],
                        "p99_ms": [],
                    }
                method_data[method]["count"].append(stats.get("count", 0))
                method_data[method]["p50_ms"].append(stats.get("p50_ms", 0.0))
                method_data[method]["p95_ms"].append(stats.get("p95_ms", 0.0))
                method_data[method]["p99_ms"].append(stats.get("p99_ms", 0.0))

        for method, vals in sorted(method_data.items()):
            methods_rows.append({
                "commit": commit,
                "method": method,
                "count": statistics.mean(vals["count"]),
                "p50_ms": statistics.mean(vals["p50_ms"]),
                "p95_ms": statistics.mean(vals["p95_ms"]),
                "p99_ms": statistics.mean(vals["p99_ms"]),
            })

        # Total duration
        durations = [
            r["total_duration_ms"] for r in reports if "total_duration_ms" in r
        ]
        timed_outs = [float(r.get("timed_out_requests", False)) for r in reports]

        if durations:
            total_duration_rows.append({
                "commit": commit,
                "mean_total_ms": statistics.mean(durations),
                "min_total_ms": min(durations),
                "max_total_ms": max(durations),
                "timed_out_avg": statistics.mean(timed_outs),
            })

    return wallclock_rows, methods_rows, total_duration_rows


@click.command()
@click.option(
    "--input-dir", required=True, type=click.Path(exists=True, path_type=Path)
)
@click.option("--output-dir", required=True, type=click.Path(path_type=Path))
def main(input_dir: Path, output_dir: Path):
    output_dir.mkdir(parents=True, exist_ok=True)

    wallclock_rows, methods_rows, total_duration_rows = collect_all_data(input_dir)

    out_path = output_dir / "wallclock.csv"
    with out_path.open("w", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=["commit", "mean_s", "stddev_s", "min_s", "max_s", "median_s"]
        )
        writer.writeheader()
        writer.writerows(wallclock_rows)
    print(f"Written: {out_path}")

    out_path = output_dir / "methods.csv"
    with out_path.open("w", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=["commit", "method", "count", "p50_ms", "p95_ms", "p99_ms"]
        )
        writer.writeheader()
        writer.writerows(methods_rows)
    print(f"Written: {out_path}")

    out_path = output_dir / "total_duration.csv"
    with out_path.open("w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "commit",
                "mean_total_ms",
                "min_total_ms",
                "max_total_ms",
                "timed_out_avg",
            ],
        )
        writer.writeheader()
        writer.writerows(total_duration_rows)
    print(f"Written: {out_path}")


if __name__ == "__main__":
    main()
