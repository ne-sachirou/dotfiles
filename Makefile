.PHONY: help
help:
	@awk -F':.*##' '/^[-_a-zA-Z0-9]+:.*##/{printf"%-12s\t%s\n",$$1,$$2}' $(MAKEFILE_LIST) | sort

.PHONY: clean
clean: ## Clean.
	rm -fv .tool-versions
	find . -name '*.retry' -exec rm -v {} \+
	find . -name '.*.un~' -exec rm -v {} \+
	find . -name '.*.~undo-tree~' -exec rm -v {} \+
	find . -name '.*.swp' -exec rm -v {} \+
	find . -name '.*.swo' -exec rm -v {} \+
	find . -name '.DS_Store' -exec rm -v {} \+

.PHONY: format
format: format-clojure format-markdown format-python format-yaml ## Format files.
	# ag -l '\r' | xargs -t -I{} sed -i -e 's/\r//' {}
.PHONY: format-clojure
format-clojure:
	cljstyle fix || true
.PHONY: format-markdown
format-markdown:
	npx prettier --write README.md
.PHONY: format-python
format-python:
	git ls-files | grep -E '\.py$$' | xargs -t black
	git ls-files | grep -E '\.py$$' | xargs -t isort
.PHONY: format-yaml
format-yaml:
	git ls-files | grep -E '\.ya?ml$$' | xargs -t npx prettier --write

.PHONY: install
PLAYBOOK ?= $(shell perl -e 'map{print $$_,"\n"}grep /\.yml$$/,<*>' | peco --select-1 --on-cancel error)
install: ## ansible-playbook
	topgrade -c -v -y --no-retry || true
	rm -fv .tool-versions
	ansible-playbook -v -K -i hosts $(PLAYBOOK)

.PHONY: test
test: test-ansible test-clojure test-sh test-yaml ## Test.
.PHONY: test-ansible
test-ansible:
	ansible -i hosts -m setup default > /dev/null 2>&1
	ls *.yml | xargs -I{} -t ansible-playbook -v -K -i hosts --syntax-check {}
	ansible-lint *.yml roles/*/tasks/*.yml
	ansible-playbook -v -C -K -i hosts $(PLAYBOOK)
.PHONY: test-clojure
test-clojure:
	cljstyle check || true
	cljstyle find | xargs -t clj-kondo --no-warnings --parallel --lint
	cljstyle find | xargs -t clj-kondo --parallel --lint || true
.PHONY: test-sh
test-sh:
	git ls-files | grep -E '\.sh$$' | xargs -t shellcheck
	zsh -n roles/zsh/files/.z* || true
	shellcheck -e SC1090,SC1091,SC2148 roles/zsh/files/.z* || true
.PHONY: test-yaml
test-yaml:
	git ls-files | grep -E '\.ya?ml$$' | xargs -t yamllint || true

# vim:set noet:
