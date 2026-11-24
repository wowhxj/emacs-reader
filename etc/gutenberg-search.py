"""Search Project Gutenberg's official catalog, caching it in a local SQLite file."""

import csv
import gzip
import io
import json
from pathlib import Path
import sqlite3
import sys
import time
import unicodedata
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, build_opener

DAY = 86400


def http_url(url):
    parts = urlsplit(url)
    if parts.scheme not in {"http", "https"} or not parts.hostname:
        raise ValueError("Expected an HTTP(S) Gutenberg catalog")
    return url


class CatalogRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return super().redirect_request(req, fp, code, msg, headers, http_url(newurl))


def han_table():
    lines = Path(__file__).with_name("gutenberg-han.txt").read_text(encoding="utf-8").splitlines()
    pairs = "".join(line for line in lines if not line.startswith("#"))
    # Mainland titles merge 遊 into 游, including 西游记.
    return str.maketrans(pairs[::2] + "遊", pairs[1::2] + "游")


def fold(text, table):
    return unicodedata.normalize("NFKC", text).casefold().translate(table)


def refresh(db, table, url):
    today = int(time.time() // DAY)

    def last_update():
        return db.execute("PRAGMA user_version").fetchone()[0]

    if today - last_update() < 7:
        return
    # Another search may finish downloading while this one waits for the lock.
    with db:
        db.execute("BEGIN IMMEDIATE")
        if today - last_update() < 7:
            return
        db.execute("CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY, title, author, words)")
        db.execute("DELETE FROM books")
        opener = build_opener(CatalogRedirect())
        with opener.open(http_url(url), timeout=25) as remote, gzip.GzipFile(fileobj=remote) as archive:
            with io.TextIOWrapper(archive, encoding="utf-8-sig", newline="") as stream:
                reader = csv.DictReader(stream, strict=True)
                if not {"Text#", "Title", "Authors", "Type"}.issubset(reader.fieldnames or []):
                    raise ValueError("Unrecognized Gutenberg catalog header")

                def records():
                    for row in reader:
                        if row["Type"] == "Text" and row["Text#"].isascii() and row["Text#"].isdigit():
                            title = " ".join(row["Title"].split())
                            author = " ".join(row["Authors"].split())
                            if title:
                                yield int(row["Text#"]), title, author, fold(title + " " + author, table)

                db.executemany("INSERT INTO books VALUES (?, ?, ?, ?)", records())
        if not db.execute("SELECT 1 FROM books LIMIT 1").fetchone():
            raise ValueError("Empty Gutenberg catalog")
        db.execute(f"PRAGMA user_version = {today}")


def search(cache, url, query):
    cache.parent.mkdir(parents=True, exist_ok=True)
    table = han_table()
    with sqlite3.connect(cache, timeout=35) as db:
        try:
            refresh(db, table, url)
        except Exception:
            if not db.execute("PRAGMA user_version").fetchone()[0]:
                raise
        terms = fold(query, table).split()
        if not terms:
            return []
        where = " AND ".join("instr(words, ?) > 0" for _ in terms)
        return db.execute(f"SELECT id, title, author FROM books WHERE {where} ORDER BY id",
                          terms).fetchall()


if __name__ == "__main__":
    try:
        rows = search(Path(sys.argv[1]), sys.argv[2], sys.argv[3])
        print(json.dumps({"books": [{"id": str(book_id), "title": title, "author": author}
                                    for book_id, title, author in rows]}))
    except Exception as error:
        sys.exit(str(error))
