---
status: done
created: 2026-11-07
scope: src/SignalToObsidian.hs, test/Spec.hs, README.md
---

# Task: output correctness fixes (collisions, Obsidian-safe names, missing-media note)

Three bugs found in a review of the Haskell port. Each one is independent; do them in
order, one commit each, with `cabal build` warning-free and `cabal test` green after every step.

**Ground rules for the agent**

- Never read or create real Signal data. Only `test/fixtures/sample.jsonl` and data you
  build inline in `test/Spec.hs` are allowed (see CONTRIBUTING.md → Privacy).
- Keep the public API in the module export list in step with any signature changes.
- Follow the existing test style: `hspec`, `describe` per function, `withSystemTempDirectory`
  for filesystem tests.
- Update README.md where user-visible behaviour changes.

---

## 1. Conversation filename collisions overwrite files

**Where:** `convert`, the `forM_ (Map.toList itemsByChat)` loop: `fname = sanitizeFilename convName <> ".md"`.

**Problem:** Two chats whose display names sanitize to the same string (two "Unknown"
contacts, two groups with the same title, `"a/b"` vs `"ab"`) write to the same file, and the
last one silently wins. Because macOS and Windows filesystems are case-insensitive by
default, `"Alex"` and `"alex"` collide there too.

**Fix (proposed):**

- Before writing anything, work out the final filenames purely: group chats by
  `T.toCaseFold (sanitizeFilename convName)`.
- A name used by exactly one chat keeps its plain filename.
- If a name is used by more than one chat, **every** chat in that group gets a
  deterministic suffix: `"<name> (<chatId>).md"`. Every chat rather than all-but-the-first,
  so that the result doesn't depend on map or file order.
- Pull this out into a pure, exported helper, e.g.
  `assignFilenames :: [(Text, Text)] -> Map Text FilePath` (chatId, displayName → chatId → filename),
  so it can be unit-tested without IO.

**Tests:**

- unit: unique names unchanged; two identical names → both suffixed with their chat ids;
  case-only difference (`"Alex"` / `"alex"`) → both suffixed; names that differ only in
  stripped characters (`"a/b"`, `"ab"`) → both suffixed.
- integration: build an inline JSONL in a temp dir with two contacts that both resolve to
  `"Unknown"`, run `convert`, and assert that two files exist and `sumConversations == 2`.

---

## 2. Filenames can break Obsidian wikilinks

**Where:** `sanitizeFilename`.

**Problem:** It only strips characters that are illegal on common filesystems. Obsidian
also can't link reliably to note names containing `#`, `^`, `[`, `]` or `|`
(heading, block-ref and alias syntax), and a leading `.` hides the file from Obsidian.

**Fix:**

- Also strip `#`, `^`, `[` and `]` (`|` is already stripped).
- Strip leading dots after collapsing whitespace.
- Keep the `"conversation"` fallback for an empty result.
- Optional: strip trailing dots/spaces, which Windows rejects.

**Tests:** extend `describe "sanitizeFilename"`:

- `"Team #1 [old]"` → `"Team 1 old"`
- `"^caret"` → `"caret"`
- `".hidden"` → `"hidden"`
- `"..."` → `"conversation"`
- existing cases unchanged

---

## 3. Missing-media summary is wrong and over-counts

**Where:** the end of `convert` (the missing counter and the final `printf`), and `Summary`.

**Problems:**

- The note says "links are still written", but `renderAttachments` writes an
  `*[attachment unavailable …]*` placeholder for these attachments instead.
- `missingRef` is incremented whenever `resolveOnDisk` returns `Nothing`, which also
  includes attachments with **no locator info** (hash not derivable). Those were never
  expected on disk and `renderAttachments` skips them entirely, so they shouldn't count as "missing".

**Fix:**

- Count separately:
  - `sumMissing` = hash derivable, but not found in the file index (renders as a placeholder)
  - new `sumNoLocator` = no derivable hash (not rendered at all)
- Correct the verbose message, e.g.:
  `Note: N attachment(s) were not found under <root>/files/ and are shown as "attachment unavailable" placeholders.`
  and, if `sumNoLocator > 0`: `M attachment(s) had no locator info and were skipped.`
- Add `sumNoLocator` to the export list and to the README if the summary is documented.

**Tests:**

- integration (fixture): `sumMissing` stays `1` (the PDF); add `sumNoLocator summary shouldBe 0`,
  or whatever the fixture actually contains (check it, don't guess).
- new inline case: one message with an attachment whose `pointer` has no `locatorInfo`
  → `sumMissing == 0`, `sumNoLocator == 1`.

---

## Out of scope (separate follow-ups, only if wanted)

- Header `_N messages_` counts all chat items, including skipped non-standard ones; consider
  counting only rendered blocks.
- Message bodies go in as raw Markdown: a body starting with `#` turns into a heading, and a
  line containing just `---` looks like the separator between messages. Consider escaping or blockquoting.
- The whole export is decoded into memory; stream line by line for very large backups.
- Delete the stale `.lsp/.cache` (clojure-lsp leftover, ~5 MB, already gitignored).

## Done when

- [x] 1, 2, 3 as separate commits (PR branch `fix/output-correctness`)
- [x] `cabal build all` with `-Wall` → no warnings
- [x] `cabal test --enable-tests --test-show-details=direct` green locally; CI green
- [x] README updated for the filename suffixing and the summary output
- [x] this note's `status:` set to `done`

**Known limitation (not fixed):** collision detection is case-insensitive but not
Unicode-normalization-aware (NFC vs NFD), which APFS also treats as equal. Fixing it
would need `text-icu`.
