# Piped

Piped is a is a lightweight and extremely performant coroutine based steaming library.

It was built as a learning exercise and to test the idea of a streaming library without using a sum type to represent state changes. It owes alot to Conduit; the contination passing monad, the default behaviours (demand driven, auto terminating), plus the major part of it's API.

For now, familiarity with Conduit is assumed (due to non extensivity of documentation).

## Features

* Depends only on base and mtl.
* Pipes are resumable via Piped.Resume.
* Pipes can be run in demand driven (the default) or supply driven mode.
* Composable with existing Conduit pipes using `fromConduit` in piped-conduit.

## Performance

All the below benchmarks take advantage of Conduit's fusion feature. Piped still comes out faster. The performance difference also increases with the number of pipes composed.


<img align="center" src="piped_vs_conduit-median-Mean.svg" width="400" height="300">


