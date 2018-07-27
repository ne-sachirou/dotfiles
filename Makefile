.PHONY: help
help:
	@awk -F':.*##' '/^[-_a-zA-Z0-9]+:.*##/{printf"%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: clean
clean: ## Clean.
	find . -name '*.retry' -exec rm -v {} \+
	find . -name '.DS_Store' -exec rm -v {} \+

.PHONY: install
PLAYBOOK ?= $(shell perl -e 'map{print $$_,"\n"}grep /\.yml$$/,<*>' | peco --select-1)
install: ## ansible-playbook
	ansible-playbook -i hosts $(PLAYBOOK)

.PHONY: test
test: ## Test.
	find . -name '*.yml' -exec yamllint {} \+ || true
	find . -name '*.yml' -exec ansible-lint -x ANSIBLE0012 {} \+ || true
	find . -name '*.sh' -exec shellcheck {} \+ || true

# vim:set noet:
