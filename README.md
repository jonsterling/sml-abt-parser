### Development

To develop this library, first get its submodules:

````sh
git submodule init --recursive
git submodule update --recursive
````

Then, load `development.cm` in the SML/NJ repl:

````sh
rlwrap sml
> CM.make "development.cm";
````

### Library Integration

To use this library as a dependency, you need to source `abt-parser.cm` in your CM
file, and bind the libs anchor to a location which contains this library's
submodule dependencies.

    abt-parser.cm (bind:(anchor:libs value:vendor))

