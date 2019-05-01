SHELL = bash

dev:
	ghcid -c 'stack repl --test'

dev-test:
	ghcid -c 'stack repl --test --main-is=piped:test:piped-test' -T ':main --color always'

build-bench:
	stack build --flag piped:bench piped:piped-bench

benchmarks: build-bench
	[[ -d charts0 ]] && rm -rf charts1 && mv charts0 charts1 || true
	mkdir charts0
	cd charts0 && stack exec piped-bench -- --csv report.csv
	cd charts0 && stack exec piped-bench -- charts report.csv

