all: build-crichton-tui reload-daemon

reload-daemon:
	sbcl --noinform --non-interactive \
	--eval '(require :swank-client)' \
	--eval "(swank-client:with-slime-connection (conn \"localhost\" 4005) (swank-client:slime-eval '(asdf:load-system :crichton :force t) conn))"
	@echo "Be sure to reload configurations if needed"

build-crichton-tui:
	sbcl --noinform --non-interactive \
	--eval '(require :asdf)' \
	--eval '(push #p"/home/gjbroom/devel/crichton/" asdf:*central-registry*)' \
	--eval '(asdf:load-system :crichton-tui)' \
	--eval "(sb-ext:save-lisp-and-die #p\"crichton-tui\" :toplevel #'crichton-tui:main :executable t :compression t)"

