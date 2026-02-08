# Crichton

*The quietly competent agent daemon.*

A secure background AI agent with WASM-sandboxed skills, encrypted credentials,
network egress control, and Ed25519 code signing. Built in Common Lisp (SBCL),
informed by a security audit of OpenClaw.

Named for [*The Admirable Crichton*](https://en.wikipedia.org/wiki/The_Admirable_Crichton) —
the servant who's actually the most capable person in the room.

## Quick Start

```bash
# Install systemd user service
cp assets/crichton.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user start crichton

# Chat with the agent
./crichton-client
./crichton-client "What's the weather like?"

# Connect SLIME/SLY to running daemon
# M-x slime-connect RET 127.0.0.1 RET 4005
```

Management uses systemctl:

```bash
systemctl --user start crichton
systemctl --user stop crichton
systemctl --user status crichton
journalctl --user -u crichton -f   # live logs
```

The old CLI still works for eval, cred, doctor, and weather commands.

## Building the client binary

```bash
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(push #p"/path/to/crichton/" asdf:*central-registry*)' \
  --eval '(asdf:load-system :crichton-client)' \
  --eval "(sb-ext:save-lisp-and-die \"crichton-client\" \
            :toplevel #'crichton-client:main :executable t :compression t)"
```

The daemon loads fresh via systemd — no binary needed.

## Architecture

```
┌─────────────────────────────────────────────┐
│              SBCL Daemon (systemd)          │
│     started fresh via sbcl --eval           │
│                                             │
│  ┌──────────┐ ┌──────────┐ ┌──────────────┐ │
│  │ LLM      │ │ Scheduler│ │ Credential   │ │
│  │ Providers│ │ + Skills │ │ Store        │ │
│  └────┬─────┘ └────┬─────┘ └──────┬───────┘ │
│       │            │              │         │
│  ┌────┴────────────┴──────────────┴───────┐ │
│  │          Core Agent Loop               │ │
│  └──────────────────┬────────────────────┘  │
│                     │                       │
│  ┌──────────────────┴────────────────────┐  │
│  │   RPC Server (daemon.sock)            │  │
│  └──┬──────────┬──────────────┬──────────┘  │
│     │          │              │ NDJSON/UDS   │
└─────┼──────────┼──────────────┼─────────────┘
      │          │              │
┌─────┴────┐ ┌──┴───────┐ ┌───┴──────────┐
│crichton- │ │ Discord  │ │ Skill Runner │
│client    │ │ Adapter  │ │ (wasmtime)   │
└──────────┘ └──────────┘ └──────────────┘
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
│   ├── runner-client.lisp  # Daemon-side runner process client
│   └── rpc-server.lisp     # NDJSON RPC server (daemon.sock)
├── client/                 # Minimal chat client (separate ASDF system)
├── cli/
│   ├── main.lisp           # CLI: eval, cred, doctor, weather
│   └── remote.lisp         # Swank client for remote eval
├── assets/                 # systemd unit and startup script
└── runner/
    └── server.lisp         # WASM skill runner process
```

## Design Spec

See [AGENT_SPEC.md](./AGENT_SPEC.md) for the full design spec including
security requirements derived from the OpenClaw audit.

## License

MIT
