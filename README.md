# Piped

Piped is a is a very performant coroutine based steaming library with minimal dependencies.

It was built as a learning exercise and to test the idea of a streaming library without using a sum type to represent state changes. It owes alot to Conduit; the contination passing monad, the default behaviours (demand driven, auto terminating), plus the major part of it's API.

## Features

* Depends only on `base` and `mtl`.
* Piped are resumable via `import Piped.Resume`.
* Piped can be composed demand driven (the default, with `.|`) or supply driven (`|.`).
* Composable with existing Conduit pipes using `fromConduit` in piped-conduit.

## Quickstart

For now, familiarity with Conduit is assumed, since documentation is not extensive.

```haskell
import Piped
import qualified Piped.Prelude as P  -- for map, foldl, takeWhile and friends

runPiped $ sourceList [1,2,3] .| P.map (+1) .| sinkList
```

## Performance

Piped implements a similar scheme to the one described in [this paper](https://www.semanticscholar.org/paper/Faster-coroutine-pipelines-Spivey/003ae593edc83996b550bf53c865f2f33bfc1c67). In this section you can see benchmarks between Piped and Conduit. On very short (source direct to sink) pipelines, Piped is faster by a small margin. However, this margin grows quickly as the number of stages increases. For a pipeline with 4 stages (source-a-b-sink), Piped is roughly twice as fast.

#### Naive implementations of common functions
<img src="charts/mixed_naive-median-Mean.svg" width="400" height="300">

#### Hand-optimised implementations of common functions (taking advantage of fusion etc)
<img src="charts/mixed_optimised-median-Mean.svg" width="400" height="300">

#### How performance changes with additional stages
<img src="charts/n_stages-median-Mean.svg" width="400" height="300">

