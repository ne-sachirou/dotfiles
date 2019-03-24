.PHONY: help
help:
	@awk -F':.*##' '/^[-_a-zA-Z0-9]+:.*##/{printf"%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: clean
clean: ## Clean.
	find . -name '*.retry' -exec rm -v {} \+
	find . -name '.DS_Store' -exec rm -v {} \+
	find . -name '.*.un~' -exec rm -v {} \+

.PHONY: install
PLAYBOOK ?= $(shell perl -e 'map{print $$_,"\n"}grep /\.yml$$/,<*>' | peco --select-1)
install: ## ansible-playbook
	ansible-playbook -v -i hosts $(PLAYBOOK)
	topgrade -c

.PHONY: test
test: ## Test.
	find . -name '*.yml' -exec yamllint {} \+ || true
	find . -name '*.yml' -exec ansible-lint -x ANSIBLE0012 {} \+ || true
	find . -name '*.sh' -exec shellcheck {} \+ || true
	zsh -n roles/zsh/files/.z* || true
	shellcheck -e SC1090,SC1091,SC2148 roles/zsh/files/.z* || true
	ag -l '^#!.*runghc' | xargs -t hlint || true

# vim:set noet:
