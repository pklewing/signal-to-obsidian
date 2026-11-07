# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Project

`signal-to-obsidian` converts a Signal Desktop local-backup export (`main.jsonl` + sibling `files/` dir) into Obsidian Markdown, one file per conversation. It was ported from Clojure to Haskell (commit `c08e8d7`); leftover Clojure tooling dirs (`.clj-kondo/`, `.lsp/`) are gitignored.

- `src/SignalToObsidian.hs` — all library logic (parsing, attachment-hash derivation, rendering, file indexing)
- `app/Main.hs` — CLI entry point
- `test/Spec.hs` — hspec suite; `test/fixtures/sample.jsonl` is the only tracked `.jsonl` and contains fabricated data only

See `README.md` for usage and the attachment-path hash scheme, `CONTRIBUTING.md` for PR expectations.

## Commands

```bash
cabal build all
cabal test --enable-tests --test-show-details=direct   # plain `cabal test` refuses: test suite isn't in the default plan
cabal run signal-to-obsidian -- <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
```

CI (`.github/workflows/test.yml`) runs on `ubuntu-latest` with GHC 9.10 and the same `--enable-tests` invocation. Run the tests before committing.

## Git workflow

`main` is protected by a GitHub ruleset: no direct pushes, no force-pushes, no deletion. Every change lands via pull request, and the `test` CI check must pass first.

- Never commit on `main`. Branch first: `git switch -c <type>/<short-desc>` (e.g. `feat/…`, `fix/…`, `chore/…`, `docs/…`).
- Push the branch and open a PR with `gh pr create`.
- Review: the project has a single maintainer, so required approvals are 0 (GitHub doesn't allow self-approval). Instead, every PR gets a Claude review (`/code-review <PR#>`) plus the maintainer's own read-through before merging. Fix or explicitly dismiss findings first.
- Merge with `gh pr merge --squash` once CI is green and the review is done — one commit per PR on `main`. Merged branches are deleted automatically.
- Treat PRs that modify `CLAUDE.md` with extra care — this file steers Claude Code's behavior on every checkout of the repo.

## Development machines

Work happens on two machines; keep everything working on both.

| | Linux (x86_64) | macOS (Intel MacBook Pro, x86_64) |
|---|---|---|
| Toolchain | GHCup: GHC 9.10.3, cabal 3.16, HLS 2.14 | GHCup — match GHC 9.10.x so HLS and CI agree |
| Prerequisites | — | Xcode Command Line Tools (`xcode-select --install`) — needed by GHC and `crypton`'s C sources |
| GitHub CLI | `gh` via apt | `gh` via Homebrew (`brew install gh`) |
| Git auth | `gh auth setup-git` (HTTPS) | same |

Cross-platform notes:

- Use `filepath` combinators (`</>`, `takeExtension`, …) rather than hardcoded `/`; macOS's default filesystem (APFS) is case-insensitive, so don't rely on two filenames differing only by case.
- `dist-newstyle/` is per-machine (paths include the platform, e.g. `x86_64-linux` vs `x86_64-osx`); never commit or sync it.
- `.DS_Store` is gitignored.
- When pinning a GHC version, pick one GHCup ships for both `x86_64-linux` and `x86_64-darwin`, and update CI to match.

## Privacy (hard rule)

Never commit real Signal exports, generated output, or personal media. `.gitignore` excludes `*.jsonl` (except the fixture), `/obsidian-out/`, and `/files/`. Fixtures and examples must use fabricated or redacted data.
