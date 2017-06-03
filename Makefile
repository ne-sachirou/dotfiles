default: help

.PHONY: help
help:
	@awk -F ':.*##' '/^[a-zA-Z_-]+:.*##/{printf "%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: clean
clean: ## Clean
	find . -name '*.retry' -exec rm {} \;

.PHONY: install
PLAYBOOK ?= $(shell perl -e 'map{print $$_,"\n"}grep /\.yml$$/,<*>' | peco --select-1)
install: ## ansible-playbook
	ansible-playbook -i hosts $(PLAYBOOK)

.PHONY: test
test: ## Test
	find . -name '*.yml' -exec ansible-lint -x ANSIBLE0012 {} \;
	find . -name '*.sh' -exec shellcheck {} \;

# vim:set noet:
