# signal-to-obsidian

Convert a [Signal Desktop](https://signal.org/) **local backup** export (newline-delimited JSON) into [Obsidian](https://obsidian.md/)-friendly Markdown — one file per conversation, with media embedded inline and reactions rendered in chronological order.

Written in [Haskell](https://www.haskell.org/), built with Cabal.

## Features

- One Markdown file per conversation, named after the contact or group.
- Messages rendered newest-first, with sender and time.
- Attachments embedded as Obsidian wikilinks (`![[files/xx/<hash>.<ext>]]`), so images and PDFs preview inline.
- Reactions shown inline beneath each message, in the order they were added.
- Optional `--copy-media` mode produces a self-contained Obsidian vault.
- Attachments that were never downloaded to the device render as a clear placeholder instead of a broken link.

## Requirements

- GHC (≥ 9.6) and cabal-install, most easily via [GHCup](https://www.haskell.org/ghcup/).
- A Signal Desktop local backup, unpacked so you have the `main.jsonl` export and its sibling `files/` directory.

## Usage

```bash
# build and run in one step
cabal run signal-to-obsidian -- main.jsonl ./obsidian-out --files-root /path/to/backup --copy-media

# or install the binary onto your PATH first
cabal install
signal-to-obsidian main.jsonl ./obsidian-out --files-root /path/to/backup --copy-media
```

Arguments:

| Argument | Meaning |
| --- | --- |
| `<export.jsonl>` | The newline-delimited JSON export. |
| `<output-dir>` | Where the per-conversation `.md` files are written. |
| `--files-root <dir>` | The directory that **contains** `files/`. Defaults to the export file's own directory. |
| `--copy-media` | Copy each referenced attachment into `<output-dir>/files/xx/`, making the output a self-contained vault. |

Open `<output-dir>` as an Obsidian vault (or drop it inside an existing one) and the embeds resolve.

## Output

- **Filenames.** Each note is named after the conversation, with characters that are illegal on common filesystems or that break Obsidian links (`\ / : * ? " < > | # ^ [ ]`) removed, and leading/trailing dots trimmed.
- **Name collisions.** If several conversations end up with the same name (compared case-insensitively, since macOS and Windows filesystems are case-insensitive), every one of them gets its chat id appended, e.g. `Unknown (10).md` and `Unknown (20).md`, so none overwrite each other.
- **Summary.** After converting, the tool reports how many attachments were not found under `<files-root>/files/` (rendered as *attachment unavailable* placeholders) and, separately, how many had no locator info at all (skipped).

## How attachment paths are derived

Signal stores backup media under `files/<first two hex chars>/<hash>.<ext>`. The hash is **not** a digest of the file contents; it is:

```
hex( sha256( base64decode(plaintextHash) ++ base64decode(localKey) ) )
```

where `plaintextHash` and `localKey` come from each attachment's `locatorInfo` block. (Both are base64-encoded in the JSON export.) This mirrors the local-backup filename logic in Signal Desktop's source.

## Running the tests

```bash
cabal test --enable-tests
```

The suite covers the hash derivation (with real verified vectors as regression guards), name resolution, extension handling, filename sanitization, index building, reaction ordering, end-to-end rendering, a filesystem round-trip, and an integration pass over a synthetic fixture (`test/fixtures/sample.jsonl`, which contains only fabricated data).

## Scope and limitations

- Only `standardMessage` items are rendered. Other `chatItem` types (call events, group-membership changes, disappearing-timer notices, contact/sticker shares) are currently skipped, so a conversation's rendered message count may be lower than its raw item count.
- Quoted replies render the reply body but not the quoted context.
- Tested against one real export's schema; other Signal versions may differ. Contributions welcome.

## License

MIT — see [LICENSE](LICENSE).
