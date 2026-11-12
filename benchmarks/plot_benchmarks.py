#!/usr/bin/env python3
"""Visualize FunKit benchmark CSV as grouped bar charts.

Usage (from benchmarks/):
    python plot_benchmarks.py                       # latest CSV in results/
    python plot_benchmarks.py results/X.csv --reference results/Y.csv

--reference fills the QMeS/DoFun columns from an earlier full run, for CSVs
produced with FUNKIT_BENCH_ONLY=1. Both runs must come from the same machine.
"""

import argparse
import glob
import math

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# ── Configuration ────────────────────────────────────────────────────────────

# Subplot grouping: key = subplot title, value = list of substrings to match
# against the Title column.
GROUPS = {
    "NJL model": ["Four-Fermion"],
    "Scalar Theory": ["Scalar Theory"],
    "Yang-Mills": ["Yang-Mills"],
    "Yukawa": ["Yukawa"],
}

# FunKit rows: CSV stage -> bar label
FUNKIT_STAGES = {
    "mathematica": ("Full derivation", "FunKit (Mathematica)"),
    "cpp": ("Full derivation (C++)", "FunKit (C++)"),
}
REFERENCE_TOOLS = ["DoFun", "QMeS"]
COLORS = {
    "FunKit (Mathematica)": "#FDB863",
    "FunKit (C++)": "#FF7F0E",
    "QMeS": "#2077B4",
    "DoFun": "#2CA02C",
}

# ── Script ───────────────────────────────────────────────────────────────────


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("csv", nargs="?", help="benchmark CSV (default: latest in results/)")
    parser.add_argument("--reference", help="CSV to take the QMeS/DoFun columns from")
    parser.add_argument(
        "--funkit",
        choices=["cpp", "mathematica", "both"],
        default="cpp",
        help="which FunKit backend(s) to plot (default: cpp)",
    )
    parser.add_argument("-o", "--output", default="benchmark_plot.pdf")
    return parser.parse_args()


def load(path):
    df = pd.read_csv(path)
    for col in df.columns:
        if col.endswith("_Mean") or col.endswith("_StdDev"):
            df[col] = pd.to_numeric(df[col], errors="coerce")
    return df


def build_table(df_all, df_ref, funkit_choice):
    """One row per benchmark title; columns '<label>_Mean' / '<label>_StdDev'."""
    full = df_all[df_all["Stage"] == "Full derivation"].set_index("Title")
    table = pd.DataFrame(index=full.index)

    backends = ["mathematica", "cpp"] if funkit_choice == "both" else [funkit_choice]
    for backend in backends:
        stage, label = FUNKIT_STAGES[backend]
        rows = df_all[df_all["Stage"] == stage].set_index("Title")
        table[f"{label}_Mean"] = rows["FunKit_Mean"]
        table[f"{label}_StdDev"] = rows["FunKit_StdDev"]

    ref_full = full
    if df_ref is not None:
        ref_full = df_ref[df_ref["Stage"] == "Full derivation"].set_index("Title")
        missing = sorted(set(table.index) - set(ref_full.index))
        if missing:
            print("No reference numbers for:", *missing, sep="\n  ")
    for tool in REFERENCE_TOOLS:
        table[f"{tool}_Mean"] = ref_full[f"{tool}_Mean"].reindex(table.index)
        table[f"{tool}_StdDev"] = ref_full[f"{tool}_StdDev"].reindex(table.index)

    labels = [FUNKIT_STAGES[b][1] for b in backends] + REFERENCE_TOOLS
    return table.reset_index(), labels


def plot(table, tools, output):
    grouped = {}
    for group_name, patterns in GROUPS.items():
        sub = table[table["Title"].apply(lambda t: any(p in t for p in patterns))]
        if not sub.empty:
            grouped[group_name] = sub
    if not grouped:
        raise SystemExit("No data matched the configured groups.")

    ncols = math.ceil(math.sqrt(len(grouped)))
    nrows = math.ceil(len(grouped) / ncols)
    fig, axes = plt.subplots(nrows, ncols, figsize=(4 * ncols, 3 * nrows), squeeze=False)
    axes = axes.flatten()
    for ax in axes[len(grouped):]:
        ax.set_visible(False)

    bar_width = 0.75 / len(tools)

    for ax, (group_name, sub) in zip(axes, grouped.items()):
        # Short labels: strip "Category: " prefix and shorten common suffixes
        labels = [t.split(": ", 1)[-1] for t in sub["Title"]]
        labels = [
            l.replace(" (Wetterich)", " flow").replace(" function", "").replace(" vertex", "")
            for l in labels
        ]
        x = np.arange(len(labels))

        for i, tool in enumerate(tools):
            means = sub[f"{tool}_Mean"].values
            stds = sub[f"{tool}_StdDev"].values
            valid = ~np.isnan(means)
            if not valid.any():
                continue
            ax.bar(
                x[valid] + i * bar_width,
                means[valid],
                bar_width,
                yerr=stds[valid],
                label=tool,
                color=COLORS[tool],
                capsize=3,
            )

        ax.set_title(group_name)
        ax.set_xticks(x + bar_width * (len(tools) - 1) / 2)
        ax.set_xticklabels(labels, rotation=30, ha="right", fontsize=8)
        ax.set_ylabel("Time (s)")
        ax.set_yscale("log")

    handles, lbls = axes[0].get_legend_handles_labels()
    plt.tight_layout()
    fig.legend(handles, lbls, loc="upper center", bbox_to_anchor=(0.5, 0.0), ncol=len(tools), fontsize=9)
    plt.savefig(output, dpi=150, bbox_inches="tight")
    print("Wrote", output)


def main():
    args = parse_args()
    # File names carry a sortable timestamp (benchmark_YYYYMMDD_HHMM.csv).
    csv = args.csv or max(glob.glob("results/benchmark_*.csv"))
    print("FunKit numbers from:", csv)
    df_ref = None
    if args.reference:
        print("QMeS/DoFun numbers from:", args.reference)
        df_ref = load(args.reference)
    table, tools = build_table(load(csv), df_ref, args.funkit)
    plot(table, tools, args.output)


if __name__ == "__main__":
    main()
