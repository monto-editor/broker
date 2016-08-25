Haskell Monto Broker
====================

Alternative implementation of the original [Monto broker](https://bitbucket.org/inkytonik/monto).
This implementation supports servers that have dependencies to each other. For
example a type checking server could have a dependency on a parsing server that
produces ASTs.

Build Instructions
------------------

1. Build Requirements
    * Stack
    * ZeroMQ 4
    * pkgconfig

2. Build the project with stack.

   ```shell
   $ cd monto-broker
   $ stack build
   ```

3. Start the broker with `start.sh`
