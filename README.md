# piped

Piped is a Haskell streaming library with the same core functionality as Conduit, but without using sum types. This gives it a performance advantage in many circumstances.

## Features

* Basically a copy of Conduit API. Equivalent functionality is directly verified in tests.
* Composable with Conduit stages using `fromConduit`.
* Resumable pipelines.
* Extremely good performance.
* Highly flexible.

## Planned

* Bidirectional communication
* Demand driven mode
* Customisable termination
