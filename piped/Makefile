
dev:
	ghcid -c 'stack repl --test'

dev-test:
	ghcid -c 'stack repl --test --main-is=piped:test:piped-test' -T ':main --color always'

build-bench:
	stack build --flag piped:build-bench piped:piped-bench
