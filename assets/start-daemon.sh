#!/bin/bash
#
# Crichton daemon startup wrapper for systemd.
# Loads the system fresh via SBCL each time, avoiding stale foreign
# pointer bugs that occur with save-lisp-and-die binaries.

exec sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
  --eval '(asdf:load-system :crichton)' \
  --eval '(crichton/daemon:start-daemon :foreground t)'
