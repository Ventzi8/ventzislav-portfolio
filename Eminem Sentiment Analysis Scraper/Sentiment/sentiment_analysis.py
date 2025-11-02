# sentiment_analysis.py
import pandas as pd
import nltk
from nltk.sentiment import SentimentIntensityAnalyzer
from collections import defaultdict

# Make sure VADER lexicon is downloaded
nltk.download("vader_lexicon")

def analyze_sentiment(df: pd.DataFrame):
    sia = SentimentIntensityAnalyzer()

    # Apply VADER to each song
    sentiment_scores = df["cleaned_lyrics"].apply(lambda x: sia.polarity_scores(str(x)))

    # Expand dicts into columns
    sentiment_df = pd.DataFrame(list(sentiment_scores))
    df = pd.concat([df, sentiment_df], axis=1)

    # Aggregate per era
    era_sentiment = df.groupby("era")[["neg", "neu", "pos", "compound"]].mean()
    return df, era_sentiment


def main():
    # Load dataset
    df = pd.read_csv("eminem_discography.csv")

    # Run sentiment analysis
    df, era_sentiment = analyze_sentiment(df)

    print("\n=== Average sentiment per era ===")
    print(era_sentiment)

    print("\n=== Example per-song sentiment scores ===")
    print(df[["Title", "era", "compound"]].head(10))

    # Save results for further exploration
    era_sentiment.to_csv("era_sentiment.csv", index=True)
    df.to_csv("song_sentiment.csv", index=False)


if __name__ == "__main__":
    main()
