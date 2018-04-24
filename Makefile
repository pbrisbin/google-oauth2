all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --dependencies-only --test --no-run-tests
	stack install hlint weeder
	gpg --output test/oauth.token --decrypt test/oauth.token.gpg

.PHONY: build
build:
	stack build \
	  --coverage \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --coverage \
	  --fast --pedantic --test

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: clean
clean:
	stack clean
