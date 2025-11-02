#!/usr/bin/env python3
import pandas as pd
import re
from collections import Counter
from pathlib import Path

INPUT = Path("eminem_discography.csv")
OUT_PER_TRACK = Path("swear_counts_per_track.csv")
OUT_PER_ERA   = Path("swear_totals_per_era.csv")
OUT_PIVOT     = Path("swear_frequencies_per_era.csv")

# ---- Profanity lexicon ----
PROFANE = {
    "fuck","fucks","fucked","fucking","fucker","fuckers",
    "shit","shits","shitty", "shitting",
    "bitch","bitches",
    "ass","asses","asshole","assholes",
    "cunt","cunts",
    "dick","dicks",
    "pussy","pussies",
    "motherfucker","motherfuckers", "motherfucking",
    "nigga","niggas","nigger","niggers",
    "bastard","bastards",
    "prick","pricks",
    "slut","sluts","whore","whores"
}

WORD_RE = re.compile(r"[a-zA-Z']+")

def tokenize(text: str):
    return WORD_RE.findall(str(text).lower())

def count_profanity(text: str) -> Counter:
    if not isinstance(text, str) or not text.strip():
        return Counter()
    words = tokenize(text)
    return Counter(w for w in words if w in PROFANE)

# --- safe reducer for groupby ---
def safe_sum_counters(rows):
    total = Counter()
    for c in rows:
        if isinstance(c, Counter):
            total.update(c)
    return total

def main():
    df = pd.read_csv(INPUT)
    if "cleaned_lyrics" not in df.columns:
        raise SystemExit("Expected column 'cleaned_lyrics' not found.")
    if "era" not in df.columns:
        raise SystemExit("Expected column 'era' not found.")

    df["cleaned_lyrics"] = df["cleaned_lyrics"].fillna("")

    # Per-track profanity counts
    df["swear_counts"] = df["cleaned_lyrics"].apply(count_profanity)
    df["swear_total"] = df["swear_counts"].apply(lambda c: sum(c.values()))

    # Track length (optional normalization)
    df["total_words"] = df["cleaned_lyrics"].apply(lambda t: len(tokenize(t)))
    df["swears_per_1000"] = df.apply(
        lambda r: (1000.0 * r["swear_total"] / r["total_words"]) if r["total_words"] else 0.0,
        axis=1
    )

    # Aggregate per era
    era_totals = (
        df.groupby("era")
          .agg(total_swears=("swear_total", "sum"),
               total_words=("total_words", "sum"),
               n_tracks=("swear_total", "size"))
          .reset_index()
    )
    era_totals["swears_per_1000"] = era_totals.apply(
        lambda r: (1000.0 * r["total_swears"] / r["total_words"]) if r["total_words"] else 0.0,
        axis=1
    )

    # --- Safe per-era word counts ---
    def safe_sum_counters(rows):
        total = Counter()
        for c in rows:
            if isinstance(c, Counter):
                total.update(c)
        return total

    # Convert directly to dict {era: Counter}
    era_word_counts = {
        era: safe_sum_counters(rows)
        for era, rows in df.groupby("era")["swear_counts"]
    }

    # Overall totals
    overall_total = int(df["swear_total"].sum())
    overall_word_counts = sum(df["swear_counts"], Counter())

    # --- Console Output ---
    print("\n=== Total swears per era ===")
    print(era_totals[["era","total_swears","n_tracks","total_words","swears_per_1000"]]
          .sort_values("total_swears", ascending=False).to_string(index=False))

    print("\n=== Overall total swears ===")
    print(overall_total)

    print("\n=== Top swear words overall ===")
    for w, c in overall_word_counts.most_common(20):
        print(f"{w:15s} {c}")

    print("\n=== Top swears per era ===")
    for era, counts in era_word_counts.items():
        print(f"\n-- {era} --")
        for w, c in counts.most_common(10):
            print(f"{w:15s} {c}")


    # Save per-track and per-era totals
    df_out = df[["era","Title","Album","swear_total","total_words","swears_per_1000"]].copy() \
             if "Title" in df.columns and "Album" in df.columns else \
             df[["era","swear_total","total_words","swears_per_1000"]].copy()
    df_out.to_csv(OUT_PER_TRACK, index=False, encoding="utf-8", lineterminator="\n")
    era_totals.to_csv(OUT_PER_ERA, index=False, encoding="utf-8", lineterminator="\n")

    # Export pivot table: rows = eras, columns = swear words, values = counts
    pivot_data = {}
    for era, counts in era_word_counts.items():
        pivot_data[era] = {w: counts[w] for w in overall_word_counts.keys()}
    pivot_df = pd.DataFrame.from_dict(pivot_data, orient="index").fillna(0).astype(int)
    pivot_df.to_csv(OUT_PIVOT, encoding="utf-8", lineterminator="\n")

    print(f"\nWrote per-track metrics → {OUT_PER_TRACK}")
    print(f"Wrote per-era totals   → {OUT_PER_ERA}")
    print(f"Wrote swear pivot      → {OUT_PIVOT}")

if __name__ == "__main__":
    main()