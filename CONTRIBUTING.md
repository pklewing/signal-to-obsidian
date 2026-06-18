# Contributing

Thanks for your interest in contributing! This is a small [Babashka](https://babashka.org/) tool that converts a Signal Desktop local-backup export into Obsidian Markdown. Contributions of all sizes are welcome.

## Prerequisites

- [Babashka](https://github.com/babashka/babashka#installation) (`bb`).
  On macOS: `brew install borkdude/brew/babashka`.
- Git.

That's it — there's no compile step and no JVM tooling required.

## Running the tool locally

Via the bb task (recommended):

```bash
bb run <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
```

Or run the script directly:

```bash
bb src/signal_to_obsidian.clj <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
```

Flags:

- `--files-root <dir>` — the directory that contains `files/`. Defaults to the export file's own directory.
- `--copy-media` — copy referenced attachments into `<output-dir>/files/` so the output is a self-contained Obsidian vault.

See the [README](README.md) for a fuller description of how the conversion works.

## Running the tests

```bash
bb test
```

The suite (`test/signal_to_obsidian_test.clj`) runs against a small synthetic fixture at `test/fixtures/sample.jsonl` and covers the attachment-hash derivation, name resolution, rendering, and an end-to-end pass. CI runs the same `bb test` on every push and pull request.

If you fix a parsing or formatting bug, please add a test that would have caught it.

## Privacy

This is the one rule that really matters for this project:

- **Never commit a real Signal export or any personal media.** The `.gitignore` excludes `*.jsonl` (and `obsidian-out/`, `files/`) for exactly this reason; the only tracked `.jsonl` is the synthetic fixture.
- Use redacted or fabricated data in fixtures and examples.
- If you file a bug report, redact personal data (names, phone numbers, message text) before attaching any sample.

## Pull requests

- Keep changes focused; a short description of what and why is plenty.
- Make sure `bb test` passes locally before opening the PR.
- Update the README if you change behavior or flags.
- Add or update a test for any behavior you change.

## Scope and known limitations

The tool currently renders `standardMessage` items only. Other `chatItem` types — call events, group-membership changes, disappearing-timer notices, contact/sticker shares — are skipped, and quoted replies render the reply body but not the quoted context. These are good areas for contribution. If you're adding support for a new message type, a redacted/synthetic example of that type in the fixture helps reviewers a lot.

This tool was developed against one real export's schema. Other Signal versions may differ; if you hit a format mismatch, an issue describing the structure (with redacted data) is welcome.
