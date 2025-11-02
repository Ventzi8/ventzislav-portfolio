#!/usr/bin/env python3
import math
import re
from collections import Counter
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ======================
# Config
# ======================
INPUT = Path("eminem_discography.csv")
OUT_DIR = Path("vocab_results")
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Toggle to also write Heaps curve PNGs (per era + overall)
MAKE_PLOTS = True

# Tokenization: letters, digits, apostrophes (keeps contractions)
WORD_RE = re.compile(r"[A-Za-z0-9']+")

def tokenize(text: str):
    if not isinstance(text, str):
        return []
    return WORD_RE.findall(text.lower())

def concat_tokens(series_of_texts: pd.Series):
    # Concatenate token streams preserving order (for cumulative Heaps curves)
    toks = []
    for t in series_of_texts:
        toks.extend(tokenize(t))
    return toks

def heaps_curve(tokens):
    """
    Build cumulative Heaps data:
    N_i = total tokens up to i
    V_i = unique types up to i
    We downsample to reduce size (every k tokens) for speed.
    """
    if not tokens:
        return np.array([]), np.array([])
    k = max(1, len(tokens) // 2000)  # keep ~2000 points max
    seen = set()
    Ns, Vs = [], []
    for i, w in enumerate(tokens, start=1):
        seen.add(w)
        if i % k == 0 or i == len(tokens):
            Ns.append(i)
            Vs.append(len(seen))
    return np.array(Ns, dtype=float), np.array(Vs, dtype=float)

def fit_heaps(Ns, Vs):
    """
    Fit log10(V) = a + b*log10(N); then K=10^a, beta=b
    Returns K, beta, R2
    """
    if len(Ns) < 2 or len(Vs) < 2 or (Ns <= 0).any() or (Vs <= 0).any():
        return np.nan, np.nan, np.nan
    x = np.log10(Ns)
    y = np.log10(Vs)
    # Linear regression y = a + b x
    b, a = np.polyfit(x, y, 1)  # note: numpy returns [b, a] for 1st degree if given x,y
    y_hat = a + b * x
    ss_res = float(np.sum((y - y_hat) ** 2))
    ss_tot = float(np.sum((y - np.mean(y)) ** 2))
    r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else np.nan
    K = 10 ** a
    beta = b
    return K, beta, r2

def main():
    # ---------- Load ----------
    df = pd.read_csv(INPUT)
    if "cleaned_lyrics" not in df.columns or "era" not in df.columns:
        raise SystemExit("CSV must contain 'cleaned_lyrics' and 'era' columns.")
    df["cleaned_lyrics"] = df["cleaned_lyrics"].fillna("")

    # ---------- Per-track token counts ----------
    df["tokens"] = df["cleaned_lyrics"].apply(lambda t: len(tokenize(t)))
    df["types"]  = df["cleaned_lyrics"].apply(lambda t: len(set(tokenize(t))))
    df["ttr"]    = df.apply(lambda r: (r["types"] / r["tokens"]) if r["tokens"] else 0.0, axis=1)

    # ---------- Per-era aggregates ----------
    era_grp = df.groupby("era", sort=False)

    era_metrics = era_grp.agg(
        total_tokens=("tokens", "sum"),
        unique_types=("cleaned_lyrics", lambda s: len(set().union(*map(set, (tokenize(x) for x in s))))),
        n_tracks=("tokens", "size")
    ).reset_index()

    era_metrics["ttr_corpus_level"] = era_metrics.apply(
        lambda r: (r["unique_types"] / r["total_tokens"]) if r["total_tokens"] else 0.0, axis=1
    )

    # Save per-era basic metrics
    era_metrics_path = OUT_DIR / "era_vocab_metrics.csv"
    era_metrics.to_csv(era_metrics_path, index=False, encoding="utf-8", lineterminator="\n")

    # ---------- Heaps’ law per era ----------
    heaps_params_rows = []
    for era, sub in era_grp:
        toks = concat_tokens(sub["cleaned_lyrics"])
        Ns, Vs = heaps_curve(toks)
        K, beta, r2 = fit_heaps(Ns, Vs)
        heaps_params_rows.append({
            "era": era,
            "K": K,
            "beta": beta,
            "R2": r2,
            "points_used": int(len(Ns)),
            "total_tokens": int(len(toks)),
            "vocab_size": int(len(set(toks)))
        })

        # Save the curve points
        curve_df = pd.DataFrame({"N_tokens": Ns, "V_types": Vs})
        curve_df.to_csv(OUT_DIR / f"heaps_points_{era}.csv", index=False, encoding="utf-8", lineterminator="\n")

        # Optional plot
        if MAKE_PLOTS and len(Ns) > 1:
            fig = plt.figure(figsize=(6,4))
            plt.loglog(Ns, Vs, marker=".", linewidth=1)
            plt.xlabel("N (tokens)")
            plt.ylabel("V (unique types)")
            plt.title(f"Heaps’ curve — {era}\nK={K:.3g}, beta={beta:.3f}, R²={r2:.3f}")
            plt.tight_layout()
            figpath = OUT_DIR / f"heaps_curve_{era}.png"
            plt.savefig(figpath, dpi=150)
            plt.close(fig)

    # ---------- Heaps’ law overall ----------
    all_tokens = concat_tokens(df["cleaned_lyrics"])
    Ns_all, Vs_all = heaps_curve(all_tokens)
    K_all, beta_all, r2_all = fit_heaps(Ns_all, Vs_all)
    pd.DataFrame({"N_tokens": Ns_all, "V_types": Vs_all}).to_csv(
        OUT_DIR / "heaps_points_overall.csv", index=False, encoding="utf-8", lineterminator="\n"
    )
    if MAKE_PLOTS and len(Ns_all) > 1:
        fig = plt.figure(figsize=(6,4))
        plt.loglog(Ns_all, Vs_all, marker=".", linewidth=1)
        plt.xlabel("N (tokens)")
        plt.ylabel("V (unique types)")
        plt.title(f"Heaps’ curve — Overall\nK={K_all:.3g}, beta={beta_all:.3f}, R²={r2_all:.3f}")
        plt.tight_layout()
        plt.savefig(OUT_DIR / "heaps_curve_overall.png", dpi=150)
        plt.close(fig)

    # ---------- Save Heaps parameters ----------
    heaps_params = pd.DataFrame(heaps_params_rows)
    overall_row = pd.DataFrame([{
        "era": "Overall",
        "K": K_all, "beta": beta_all, "R2": r2_all,
        "points_used": int(len(Ns_all)),
        "total_tokens": int(len(all_tokens)),
        "vocab_size": int(len(set(all_tokens)))
    }])
    heaps_params = pd.concat([heaps_params, overall_row], ignore_index=True)
    heaps_params_path = OUT_DIR / "heaps_fit_params.csv"
    heaps_params.to_csv(heaps_params_path, index=False, encoding="utf-8", lineterminator="\n")

    # ---------- Console summary ----------
    print("\n=== Per-era vocabulary metrics ===")
    print(era_metrics.sort_values("total_tokens", ascending=False).to_string(index=False))

    print("\n=== Heaps’ law parameters (per era + overall) ===")
    print(heaps_params.to_string(index=False))

    print(f"\nWrote:\n- {era_metrics_path}\n- {heaps_params_path}")
    if MAKE_PLOTS:
        print(f"- Heaps curves (PNGs) in {OUT_DIR}/")
        print(f"- Heaps point CSVs per era + overall in {OUT_DIR}/")

if __name__ == "__main__":
    main()
