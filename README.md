Haskell Monto Broker
====================

Alternative implementation of the original [Monto broker](https://bitbucket.org/inkytonik/monto).
This implementation supports servers that have dependencies to each other. For
example a type checking server could have a dependency on a parsing server that
produces ASTs.

Build Instructions
------------------

1. Build Requirements:
    * [Haskell Platform 2014](https://www.haskell.org/platform).
      This includes:
        - GHC 7.8.3
        - Cabal 1.18
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

3. Start the broker with `./start.sh`

Building the Broker using Docker
--------------------------------

There is also an alternative to building the broker directly with system
packages. This section assumes that docker has already been installed.

```shell
$ cd monto-broker
$ sudo docker build -t monto-broker .

# This command binds ports all required ports from the host os to the docker
# container and then starts the broker with the appropriate configuration
$ sudo docker run \
    -p 5000:5000 \
    -p 5001:5001 \
    -p 5010:5010 \
    -p 5011:5011 \
    -p 5012:5012 \
    -p 5013:5013 \
    monto-broker \
    broker --debug \
        --source 'tcp://*:5000' \
        --sink 'tcp://*:5001' \
        --servers '[(tokens/json,[Source],"tcp://*:5010"),(ast/json,[Source],"tcp://*:5011"),(outline/json,[Source,ast/json],"tcp://*:5012"),(completions/j
son,[Source,ast/json],"tcp://*:5013")]'
```
