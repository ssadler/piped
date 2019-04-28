
import Criterion.Main

import BenchYields
import BenchPipes


main = defaultMain
  [ benchPipes
  , benchYields
  ]
