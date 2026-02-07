# Crichton — Open Issues

Tracked issues to address. Items from the OpenClaw audit findings are in AGENT_SPEC.md.

---

## Style Warnings (SBCL compilation)

- [ ] **scheduler.lisp:68** — `m` and `h` unused in `next-daily-ut`. The `decode-universal-time` destructuring binds them but only uses `day`, `month`, `year`. Should `(declare (ignore m h))`.
- [ ] **cli/remote.lisp:16** — `timeout` parameter in `remote-eval` is defined but never used. Either wire it into the socket/swank call or remove it.

## Code Quality

- [ ] **Keyword interning from untrusted JSON** — credentials and sessions deserializers intern keywords from external input. Should validate/whitelist or use strings.
- [ ] **Duplicate TOML-to-plist helpers** — `skills/manifest.lisp` and `config/loader.lisp` both have TOML-to-plist conversion. Factor into shared utility.
- [ ] **TOML string-vs-keyword normalization** — TOML config values arrive as strings but defaults use keywords. Fixed in `llm/registry.lisp` but other consumers will need the same treatment as they come online.

## Security

- [ ] **Weather skill bypasses egress control** — makes direct HTTP calls via dexador without going through an allowlist check. Not critical (it's a daemon-side built-in, not a WASM skill) but inconsistent with the spec's egress philosophy.
- [ ] **Session encryption toggle hazard** — `.age` file extension is always used regardless of the `encrypt` config setting. Could mislead about whether data is actually encrypted.
- [ ] **libsecret backend stubbed** — OS keychain credential backend (Linux desktop via libsecret) is defined but not implemented. Only the age-file backend works.

## Dead Code

- [ ] **cli/remote.lisp** — `timeout` variable bound but unused (see style warnings above).
