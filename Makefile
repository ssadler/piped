SHELL = bash

dev:
	ghcid -c 'stack repl --test'

dev-test:
	ghcid -c 'stack repl --test --main-is=piped:test:piped-test' -T ':main --color always'
