#!/bin/bash

set -e

function build () {
	stack build --flag piped:bench piped:piped-bench
}
function run () {
    build
	stack exec piped-bench -- $@
}
function charts () {
    cd charts
	stack exec piped-bench -- charts report.csv
    cd ..
}
function all () {
	[[ -d charts ]] && rm -rf charts1 && mv charts charts1 || true
	mkdir charts
    cd charts
    run --csv "report.csv" $@
    cd ..
    charts
}
function main () (
    cmd="$1"
    shift
    if [ ! -z "$cmd" ];
    then
        $cmd $@
    else
        run --quick
    fi
)
main $@
