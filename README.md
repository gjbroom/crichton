# Crichton

*The quietly competent agent daemon.*

A secure background AI agent with WASM-sandboxed skills, encrypted credentials,
network egress control, and Ed25519 code signing. Built in Common Lisp (SBCL),
informed by a security audit of OpenClaw.

Named for [*The Admirable Crichton*](https://en.wikipedia.org/wiki/The_Admirable_Crichton) —
the servant who's actually the most capable person in the room.

## Quick Start

```bash
# Load in SBCL REPL
(push #p"/path/to/crichton/" asdf:*central-registry*)
(ql:quickload :crichton)

# CLI commands
(crichton/cli:main '("crichton" "help"))
(crichton/cli:main '("crichton" "doctor"))
(crichton/cli:main '("crichton" "start"))

# Connect SLIME/SLY to running daemon
;; M-x slime-connect RET 127.0.0.1 RET 4005
```

## Building a standalone binary

```bash
sbcl --eval '(push #p"/path/to/crichton/" asdf:*central-registry*)'  \
     --eval '(ql:quickload :crichton)'  \
     --eval '(sb-ext:save-lisp-and-die "crichton"
               :toplevel #'"'"'crichton:main
               :executable t
               :compression t)'
```

## Architecture

```
┌─────────────────────────────────────────────┐
│              SBCL Daemon                    │
│                                             │
│  ┌──────────┐ ┌──────────┐ ┌──────────────┐ │
│  │ LLM      │ │ Channel  │ │ Credential   │ │
│  │ Providers│ │ Adapters │ │ Store        │ │
│  └────┬─────┘ └────┬─────┘ └──────┬───────┘ │
│       │            │              │         │
│  ┌────┴────────────┴──────────────┴───────┐ │
│  │          Core Agent Loop               │ │
│  └────────────────┬───────────────────────┘ │
│                   │ RPC (unix socket)       │
└───────────────────┼─────────────────────────┘
                    │
        ┌───────────┴───────────┐
        │   Skill Runner        │
        │   (separate process)  │
        │  ┌─────────────────┐  │
        │  │ wasmtime engine │  │
        │  │ (WASI sandbox)  │  │
        │  └─────────────────┘  │
        │  No FS / No network   │
        └───────────────────────┘
```

## Project Layout

```
crichton/
├── crichton.asd            # ASDF system definition
├── package.lisp            # Package declarations
├── crichton.lisp           # Top-level entry point
├── config/
│   ├── paths.lisp          # ~/.crichton/ directory layout
│   └── loader.lisp         # TOML config loading
├── logging/
│   └── logger.lisp         # Structured JSON logging (0600 perms)
├── rpc/
│   └── protocol.lisp       # NDJSON protocol for daemon↔runner RPC
├── wasm/
│   ├── ffi.lisp            # CFFI bindings to wasmtime C API
│   └── engine.lisp         # Higher-level WASM execution engine
├── skills/
│   ├── manifest.lisp       # Skill capability manifest parsing
│   └── signing.lisp        # Ed25519 skill signing/verification
├── crypto/
│   ├── wipe.lisp           # Best-effort secret zeroization
│   └── age.lisp            # age CLI wrapper for encryption
├── credentials/
│   ├── protocol.lisp       # CLOS credential store protocol
│   ├── backend-age-file.lisp # Age-encrypted file backend
│   └── mediator.lisp       # Daemon credential mediation API
├── sessions/
│   ├── store.lisp          # Encrypted session storage
│   └── retention.lisp      # Session retention policy + purge
├── daemon/
│   ├── lifecycle.lisp      # Start/stop/status + PID management
│   ├── swank.lisp          # SLIME/SLY connectivity
│   └── runner-client.lisp  # Daemon-side runner process client
├── cli/
│   ├── main.lisp           # CLI: start, stop, status, doctor, cred
│   └── remote.lisp         # Swank client for remote eval
└── runner/
    └── server.lisp         # WASM skill runner process
```

## Design Spec

See [AGENT_SPEC.md](./AGENT_SPEC.md) for the full design spec including
security requirements derived from the OpenClaw audit.

## License

MIT
