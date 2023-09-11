# LowCal v0.1

An experimental language with minimal syntactical sugar, written in Haskell with
[Megaparsec](https://markkarpov.com/tutorial/megaparsec.html).

    def example() do
      1
    end

It transpiles to JavaScript.

    function example() {
      return 1;
    }

See the `examples` folder for more examples.

## Getting started

1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run `stack build` to build the transpiler
3. Run `stack run examples/sort.lowcal | node -` to run a transpiled program!
