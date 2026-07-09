#!/usr/bin/env python3
"""Visualize FunKit benchmark CSV as grouped bar charts."""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import math
import glob
import os

# ── Configuration ────────────────────────────────────────────────────────────

# Grab the latest CSV file from the "results" folder and set it here.
CSV_FILE = max(glob.glob("results/benchmark_*.csv"), key=os.path.getctime)

# Which titles to include. Set to a list of exact title strings to filter,
# or None to include all.
SELECTED_TITLES = None

# Subplot grouping: key = subplot title, value = list of substrings to match
# against the Title column.
GROUPS = {
    "NJL model": ["Four-Fermion"],
    "Scalar Theory": ["Scalar Theory"],
    "Yang-Mills": ["Yang-Mills"],
    "Yukawa": ["Yukawa"],
}

# Tools to compare (column prefixes)
TOOLS = ["FunKit", "DoFun", "QMeS"]
COLORS = {"FunKit": "#FF7F0E", "QMeS": "#2077B4", "DoFun": "#2CA02C"}

# ── Script ───────────────────────────────────────────────────────────────────

df_all = pd.read_csv(CSV_FILE)

# Cross-tool comparison uses the "Full derivation" rows, except that the FunKit
# column is taken from the C++ backend stage ("Full derivation (C++)") so that
# FunKit is represented by its C++ numbers rather than the bare derivation.
df = df_all[df_all["Stage"] == "Full derivation"].copy()
cpp = df_all[df_all["Stage"] == "Full derivation (C++)"].copy()

# Convert numeric columns (empty strings → NaN)
for frame in (df, cpp):
    for tool in TOOLS:
        frame[f"{tool}_Mean"] = pd.to_numeric(frame[f"{tool}_Mean"], errors="coerce")
        frame[f"{tool}_StdDev"] = pd.to_numeric(frame[f"{tool}_StdDev"], errors="coerce")

# Overwrite FunKit means/stddevs with the C++ backend numbers (matched by Title)
cpp_funkit = cpp.set_index("Title")[["FunKit_Mean", "FunKit_StdDev"]]
df["FunKit_Mean"] = df["Title"].map(cpp_funkit["FunKit_Mean"])
df["FunKit_StdDev"] = df["Title"].map(cpp_funkit["FunKit_StdDev"])

# Optional title filter
if SELECTED_TITLES is not None:
    df = df[df["Title"].isin(SELECTED_TITLES)]

# Build groups
grouped = {}
for group_name, patterns in GROUPS.items():
    mask = df["Title"].apply(lambda t: any(p in t for p in patterns))
    sub = df[mask]
    if not sub.empty:
        grouped[group_name] = sub

n_groups = len(grouped)
if n_groups == 0:
    raise SystemExit("No data matched the configured groups/titles.")

ncols = math.ceil(math.sqrt(n_groups))
nrows = math.ceil(n_groups / ncols)

fig, axes = plt.subplots(nrows, ncols, figsize=(4 * ncols, 3 * nrows), squeeze=False)
axes = axes.flatten()

# Hide unused subplots
for ax in axes[n_groups:]:
    ax.set_visible(False)

bar_width = 0.25

for ax, (group_name, sub) in zip(axes, grouped.items()):
    # Short labels: strip "Category: " prefix and shorten common suffixes
    labels = [t.split(": ", 1)[-1] if ": " in t else t for t in sub["Title"]]
    labels = [
        l.replace(" (Wetterich)", " flow")
        .replace(" function", "")
        .replace(" vertex", "")
        for l in labels
    ]
    x = np.arange(len(labels))

    for i, tool in enumerate(TOOLS):
        means = sub[f"{tool}_Mean"].values
        stds = sub[f"{tool}_StdDev"].values
        # Only plot bars where data exists
        valid = ~np.isnan(means)
        if not valid.any():
            continue
        positions = x[valid] + i * bar_width
        ax.bar(
            positions,
            means[valid],
            bar_width,
            yerr=stds[valid],
            label=tool,
            color=COLORS[tool],
            capsize=3,
        )

    ax.set_title(group_name)
    ax.set_xticks(x + bar_width)
    ax.set_xticklabels(labels, rotation=30, ha="right", fontsize=8)
    ax.set_ylabel("Time (s)")
    ax.set_yscale("log")

# Single legend from first axes
handles, lbls = axes[0].get_legend_handles_labels()
fig.legend(handles, lbls, loc=(0.86, 0.49), fontsize=9)

# fig.suptitle("  ", fontsize=13, y=1.02)
plt.tight_layout()
# add some padding at the right for the legend
plt.subplots_adjust(right=0.85)
plt.savefig("benchmark_plot.pdf", dpi=150, bbox_inches="tight")
# plt.show()
