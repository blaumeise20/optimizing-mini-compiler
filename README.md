# Optimizing Mini compiler

This compiler has been written as the project for the [Advanced Compiler Construction](https://ssw.jku.at/Teaching/Lectures/ACC/) course at the Johannes Kepler University Linz.

I chose to do the [second](https://ssw.jku.at/Teaching/Lectures/ACC/Project_2.pdf) available project, because I am more interested in optimizing topics than code generation.

I also did some cleanup work afterwards, you should be able to pretty easily understand what is going on.

To run the compiler, you have to pass it the input file like `cargo run --release myprogram.mini`. It will **not** execute your code, but compile it into a control flow graph. The graph will be outputted as a Graphviz graph. You can optinally pass a `--output <file>` parameter to save the graph into a file. Examples can be found in `examples.mini`.