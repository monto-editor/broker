Haskell Monto Broker
====================

Alternative implementation of the original [Monto broker](https://bitbucket.org/inkytonik/monto).
This implementation supports servers that have dependencies to each other. For
example a type checking server could have a dependency on a parsing server that
produces ASTs.

Build Instructions
------------------

1. Build Requirements
    * GHC >= 7.10.2
    * Cabal >= 1.18
    * ZeroMQ 4
    * pkgconfig

2. Update the local repository of cabal, install the dependencies of the
   project and build the project.

   ```shell
   $ cd monto-broker
   $ cabal update
   $ cabal sandbox init
   $ cabal install --dependencies-only
   $ cabal build
   ```

3. Start the broker with `bash start.sh`
