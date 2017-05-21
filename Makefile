default: help

.PHONY: help
help:
	@awk -F ':.*##' '/^[a-zA-Z_-]+:.*##/{printf "%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: test
test: ## Test
	find . -name '*.yml' -exec ansible-lint -x ANSIBLE0012 {} \;

# vim:set noet:
