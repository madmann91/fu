# Fu

Fu is a simple functional language, whose name is inspired by Dr. Fu Manchu in the Sax Rohmer book series of the same name.

This language and implementation has the following goals:

- Being embeddable easily into C or C++ programs,
- Being compilable to C or LLVM,
- Having its own graph-based IR to produce high-quality executables,
- Serve as learning material for people wanting to learn about advanced compiler design,
- Progress in tandem with [coding videos](https://www.youtube.com/channel/UCBFJ3wD3qNSlZ4MqOA9KntA)
  in which I talk about the compiler.

## Building

To build Fu, just type the following commands:

    mkdir build
    cd build
    cmake .. -DCMAKE_BUILD_TYPE=<Debug|Release>
    make

## Documentation

The documentation can be found [here](doc/index.md).
