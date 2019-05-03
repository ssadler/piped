#!/bin/bash

set -e

function build () {
	stack build --flag piped:bench piped:piped-bench
}
function run () {
	stack exec piped-bench -- $@
}
function charts () {
    ( cd charts && stack exec piped-bench -- charts report.csv )
}
function all () {
	[[ -d charts ]] && rm -rf charts1 && mv charts charts1 || true
	mkdir charts
    build
    ( cd charts; run --csv "report.csv" $@ )
    charts
}
function main () {
    cmd="$1"
    if [ ! -z "$cmd" ];
    then
        shift
        $cmd $@
    else
        run --quick
    fi
}

main $@
