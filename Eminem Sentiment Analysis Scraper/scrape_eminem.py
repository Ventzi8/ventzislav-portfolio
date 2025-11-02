# scrape_eminem.py
import os, re, json, time
from pathlib import Path
from dotenv import load_dotenv
import lyricsgenius
from tqdm import tqdm

# ================== Config ==================
OUTPUT_JSONL = Path("eminem_discography.jsonl")
INCLUDE_SHARED_SECTIONS = False

SECTION_KEYS = [
    "verse", "chorus", "intro", "bridge", "outro", "hook",
    "pre-chorus", "post-chorus", "refrain"
]

# ================== Text Cleaning ==================
def _normalize_newlines(s: str) -> str:
    return (s or "").replace("\r\n", "\n").replace("\r", "\n")

def _strip_preamble_and_footers(raw: str) -> str:
    text = _normalize_newlines(raw)
    m = re.search(r"\[(Intro|Verse|Chorus|Bridge|Outro|Hook|Refrain)[^\]]*\]", text, re.IGNORECASE)
    if m:
        text = text[m.start():]
    text = re.sub(r"(?im)^\s*You might also like.*$", "", text)
    text = re.sub(r"(?im)^\s*\d*\s*Embed\s*$", "", text)
    return text.strip()

def _remove_adlibs(text: str) -> str:
    return re.sub(r"\([^)]*\)", "", text)

def _is_multi_artist(text: str) -> bool:
    headers = re.findall(r"\[([^\]]+)\]", text)
    return any(":" in h for h in headers)

def _header_is_eminem(header: str) -> bool:
    h = header.lower()
    is_section = any(k in h for k in SECTION_KEYS)
    has_eminem = "eminem" in h
    if not (is_section and has_eminem):
        return False
    if INCLUDE_SHARED_SECTIONS:
        return True
    return not any(mark in h for mark in ("&", " and ", ","))

def clean_lyrics_solo(raw_lyrics: str) -> str:
    text = _strip_preamble_and_footers(raw_lyrics)
    return _remove_adlibs(text).strip()

def clean_lyrics_featured(raw_lyrics: str) -> str:
    text = _strip_preamble_and_footers(raw_lyrics)
    parts = re.split(r"(\[[^\]]+\])", text)
    eminem_blocks = []
    current_header = None
    for chunk in parts:
        if not chunk.strip():
            continue
        if chunk.startswith("["):
            current_header = chunk.strip("[]")
            continue
        if current_header and _header_is_eminem(current_header):
            eminem_blocks.append(f"[{current_header}]\n{chunk.strip()}")
    return _remove_adlibs("\n\n".join(eminem_blocks)).strip()

def smart_clean(raw_lyrics: str) -> str:
    text = _strip_preamble_and_footers(raw_lyrics)
    return clean_lyrics_featured(raw_lyrics) if _is_multi_artist(text) else clean_lyrics_solo(raw_lyrics)

# ================== Metadata Helpers ==================
def safe_get(d, path, default=None):
    cur = d
    for k in path:
        if not isinstance(cur, dict) or k not in cur:
            return default
        cur = cur[k]
    return cur

def fetch_song_with_meta(genius, song_obj):
    song_id = song_obj["id"]
    meta_json = genius.song(song_id)
    meta = safe_get(meta_json, ["song"], {})

    album_name = safe_get(meta, ["album", "name"])
    release_for_display = meta.get("release_date_for_display")
    primary_artist = safe_get(meta, ["primary_artist", "name"])
    featured_artists = [a.get("name") for a in meta.get("featured_artists", []) if isinstance(a, dict)]
    writers = [a.get("name") for a in meta.get("writer_artists", []) if isinstance(a, dict)]
    producers = [a.get("name") for a in meta.get("producer_artists", []) if isinstance(a, dict)]
    pageviews = safe_get(meta, ["stats", "pageviews"])

    # Fetch lyrics text
    song = genius.lyrics(song_id=song_id)
    if not song:
        return None

    cleaned_lyrics = smart_clean(song)

    record = {
        "id": song_id,
        "title": song_obj["title"],
        "album": album_name,
        "release_date_for_display": release_for_display,
        "primary_artist": primary_artist,
        "featured_artists": featured_artists,
        "writers": writers,
        "producers": producers,
        "pageviews": pageviews,
        "cleaned_lyrics": cleaned_lyrics,
    }
    return record

# ================== Resume Helpers ==================
def load_existing_ids(path: Path):
    if not path.exists():
        return set()
    ids = set()
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            try:
                data = json.loads(line)
                ids.add(data["id"])
            except Exception:
                continue
    return ids

# ================== Main ==================
def main():
    load_dotenv()
    token = os.getenv("GENIUS_ACCESS_TOKEN")
    if not token:
        raise SystemExit("Missing GENIUS_ACCESS_TOKEN in .env")

    genius = lyricsgenius.Genius(token, verbose=False, timeout=20, retries=3)

    # Get all songs for Eminem (artist_id=45)
    print("Fetching Eminem's discography list...")
    all_songs = []
    page = 1
    while True:
        songs_page = genius.artist_songs(45, per_page=50, page=page)
        if not songs_page or "songs" not in songs_page:
            break
        all_songs.extend(songs_page["songs"])
        if not songs_page.get("next_page"):
            break
        page = songs_page["next_page"]

    print(f"Found {len(all_songs)} songs total.")

    # Load already scraped IDs
    done_ids = load_existing_ids(OUTPUT_JSONL)
    print(f"Resuming from {len(done_ids)} already scraped songs...")

    # Open file in append mode
    out = OUTPUT_JSONL.open("a", encoding="utf-8")

    remaining = [s for s in all_songs if s["id"] not in done_ids]
    print(f"Scraping {len(remaining)} remaining songs...")

    for song_obj in tqdm(remaining, desc="Scraping songs"):
        try:
            rec = fetch_song_with_meta(genius, song_obj)
            if rec:
                out.write(json.dumps(rec, ensure_ascii=False) + "\n")
                out.flush()  # ensure data is saved incrementally
                print(f"✅ {rec['title']}")
        except Exception as e:
            print(f"⚠️ Skipped {song_obj.get('title')} due to error: {e}")
            time.sleep(1)

    out.close()
    print(f"\nDone! Saved {OUTPUT_JSONL} with {len(done_ids) + len(remaining)} entries.")

if __name__ == "__main__":
    main()
