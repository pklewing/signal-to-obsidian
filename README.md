# signal-to-obsidian

Convert a [Signal Desktop](https://signal.org/) **local backup** export (newline-delimited JSON) into [Obsidian](https://obsidian.md/)-friendly Markdown — one file per conversation, with media embedded inline and reactions rendered in chronological order.

Written in [Babashka](https://babashka.org/) (a fast-starting Clojure scripting runtime), so the whole tool is a single script with no compile step.

## Features

- One Markdown file per conversation, named after the contact or group.
- Messages rendered newest-first, with sender and time.
- Attachments embedded as Obsidian wikilinks (`![[files/xx/<hash>.<ext>]]`), so images and PDFs preview inline.
- Reactions shown inline beneath each message, in the order they were added.
- Optional `--copy-media` mode produces a self-contained Obsidian vault.
- Attachments that were never downloaded to the device render as a clear placeholder instead of a broken link.

## Requirements

- [Babashka](https://github.com/babashka/babashka#installation) (`bb`). On macOS: `brew install borkdude/brew/babashka`.
- A Signal Desktop local backup, unpacked so you have the `main.jsonl` export and its sibling `files/` directory.

## Usage

```bash
# via the bb task (recommended)
bb run main.jsonl ./obsidian-out --files-root /path/to/backup --copy-media

# or run the script directly
bb src/signal_to_obsidian.clj main.jsonl ./obsidian-out --files-root /path/to/backup --copy-media
```

Arguments:

| Argument | Meaning |
| --- | --- |
| `<export.jsonl>` | The newline-delimited JSON export. |
| `<output-dir>` | Where the per-conversation `.md` files are written. |
| `--files-root <dir>` | The directory that **contains** `files/`. Defaults to the export file's own directory. |
| `--copy-media` | Copy each referenced attachment into `<output-dir>/files/xx/`, making the output a self-contained vault. |

Open `<output-dir>` as an Obsidian vault (or drop it inside an existing one) and the embeds resolve.

## How attachment paths are derived

Signal stores backup media under `files/<first two hex chars>/<hash>.<ext>`. The hash is **not** a digest of the file contents; it is:

```
hex( sha256( base64decode(plaintextHash) ++ base64decode(localKey) ) )
```

where `plaintextHash` and `localKey` come from each attachment's `locatorInfo` block. (Both are base64-encoded in the JSON export.) This mirrors the local-backup filename logic in Signal Desktop's source.

## Running the tests

```bash
bb test
```

The suite covers the hash derivation (with real verified vectors as regression guards), name resolution, extension handling, filename sanitization, index building, reaction ordering, end-to-end rendering, a filesystem round-trip, and an integration pass over a synthetic fixture (`test/fixtures/sample.jsonl`, which contains only fabricated data).

## Scope and limitations

- Only `standardMessage` items are rendered. Other `chatItem` types (call events, group-membership changes, disappearing-timer notices, contact/sticker shares) are currently skipped, so a conversation's rendered message count may be lower than its raw item count.
- Quoted replies render the reply body but not the quoted context.
- Tested against one real export's schema; other Signal versions may differ. Contributions welcome.

## License

MIT — see [LICENSE](LICENSE).
