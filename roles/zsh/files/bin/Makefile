.PHONY: help
help:
	@awk -F':.*##' '/^[-_a-zA-Z0-9]+:.*##/{printf"%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: nrepl
nrepl: ## Start a nREPL server.
	bb --nrepl-server

.PHONY: repl
repl: ## Start a REPL.
	rlwrap -c -b "(){}[],^%@\"\";:''|\\" bb
