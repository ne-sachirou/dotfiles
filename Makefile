.PHONY: help
help:
	@awk -F':.*##' '/^[-_a-zA-Z0-9]+:.*##/{printf"%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: clean
clean: ## Clean.
	find . -name '*.retry' -exec rm -v {} \+
	find . -name '.DS_Store' -exec rm -v {} \+
	find . -name '.*.un~' -exec rm -v {} \+

.PHONY: install
PLAYBOOK ?= $(shell perl -e 'map{print $$_,"\n"}grep /\.yml$$/,<*>' | peco --select-1 --on-cancel error)
install: ## ansible-playbook
	rm -fv .tool-versions
	ansible-playbook -v -K -i hosts $(PLAYBOOK)
	topgrade -c -v -y --no-retry --disable go || true
	topgrade -c -v -y --no-retry --only go || true

.PHONY: format
format: ## Format files.
	# ag -l '\r' | xargs -t -I{} sed -i -e 's/\r//' {}
	npx prettier --write README.md
	find . -name '*.yml' -exec npx prettier --write {} \+
	find . -name '*.py' -exec black {} \+
	find . -name '*.py' -exec isort {} \+

.PHONY: test
test: ## Test.
	ansible -i hosts -m setup default > /dev/null 2>&1
	find . -name '*.yml' -exec yamllint {} \+ || true
	ansible-playbook -v -K -i hosts --syntax-check $(PLAYBOOK)
	find . -name '*.yml' -exec ansible-lint {} \+
	ansible-playbook -v -C -K -i hosts $(PLAYBOOK)
	find . -name '*.sh' -exec shellcheck {} \+
	zsh -n roles/zsh/files/.z* || true
	shellcheck -e SC1090,SC1091,SC2148 roles/zsh/files/.z* || true
	ag -l '^#!.*runghc' | xargs -t hlint
	rubocop asdf-check-updates.rb || true

# vim:set noet:
