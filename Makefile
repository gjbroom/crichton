all: build-crichton-tui reload-daemon

test:
	sbcl --noinform --non-interactive \
	--eval '(require :asdf)' \
	--eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
	--eval '(asdf:load-system :crichton)' \
	--eval '(load "tests/run-all-tests.lisp")' \
	--eval '(unless (crichton/tests:run-all) (sb-ext:exit :code 1))' \
	--eval '(sb-ext:exit :code 0)'

reload-daemon:
	sbcl --noinform --non-interactive \
	--eval '(require :swank-client)' \
	--eval "(swank-client:with-slime-connection (conn \"localhost\" 4005) (swank-client:slime-eval '(asdf:load-system :crichton :force t) conn))"
	@echo "Be sure to reload configurations if needed"

rebuild-daemon:
	sbcl --noinform --non-interactive \
	--eval '(require :asdf)' \
	--eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
	--eval '(asdf:load-system :crichton :force t)' \
	--eval '(sb-ext:exit :code 0)'
	@echo "ASDF fasl cache updated — safe to restart daemon"

build-crichton-tui:
	sbcl --noinform --non-interactive \
	--eval '(require :asdf)' \
	--eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
	--eval '(asdf:load-system :crichton-tui)' \
	--eval "(sb-ext:save-lisp-and-die #p\"crichton-tui\" :toplevel #'crichton-tui:main :executable t :compression t)"

