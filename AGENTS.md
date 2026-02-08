# Workspace Agent Instructions

## Projects

- **Crichton** (`crichton/`) — Secure SBCL Common Lisp AI agent daemon. See `AGENT_SPEC.md` for full design spec, `crichton.asd` for module structure.

## Workflow Practices

- **Issue tracking**: Use BEADS (`bd` command) for all issue tracking. Beads live in `.beads/` (prefix: `cricht-`). File new beads for bugs, feature requests, style warnings, and cleanup items. Do not create ISSUES.md or TODO files.
- **Git commits**: Commit changes to git as you go. Every meaningful change gets a commit. The user expects this and will call you out for forgetting. Do NOT add "Co-Authored-By" lines to commit messages.
- **CLI binary rebuild**: Rebuild the Crichton CLI binary via `save-lisp-and-die` at suitable stopping points — after finishing a feature implementation or fixing a bug. The binary is at `crichton/crichton` (symlinked from `~/bin/crichton`). Rebuild command:
  ```
  sbcl --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
    --eval '(asdf:load-system :crichton)' \
    --eval "(sb-ext:save-lisp-and-die #p\"/home/gjbroom/devel/crichton/crichton\" :toplevel #'crichton:main :executable t :compression t)"
  ```

## Crichton Development Notes

- Phases 1-4 are complete: daemon skeleton, WASM skill runner, encrypted credentials/sessions, LLM integration (Anthropic), agent loop with tool use.
- 6 built-in skills: weather, system-info, timing, scheduler, RSS, token-usage (general-purpose metering).
- Built-in skills follow a dual interface: plist-returning function for programmatic use, formatted report function for humans.
- After creating new modules, always hot-load them into the running daemon via SLIME.
- TOML config values come through as strings but defaults use keywords. Normalize in consumers.
- Libraries: dexador, shasht, cl-toml, ironclad, log4cl, bordeaux-threads, cffi, hunchentoot, swank-client, usocket, xmls.
- Config lives at `~/.crichton/config.toml`. Default model: `claude-sonnet-4-20250514`.
- API key stored in age-encrypted credential store as `anthropic-api-key` with field `:api-key`.

## Build & Check

- **Load/compile check**: `sbcl --noinform --non-interactive --eval '(require :asdf)' --eval '(push #p"/home/gjbroom/devel/ai/crichton/" asdf:*central-registry*)' --eval '(asdf:load-system :crichton :force t)'`
- Pre-existing style warnings in scheduler.lisp and cli/remote.lisp are tracked as cricht-048 and cricht-049.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
