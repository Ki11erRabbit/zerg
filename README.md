# Zerg Lang

## What is Zerg
Zerg is a research programming language focusing on experimenting with the idea that you can build a fully featured compiler by doing the following:
1. Implement an compile time interpreter for special functions in the language.
2. Provide an interface for the compile time executed code to manipulate compiler state.
3. Make a core library that wraps the minimal api that can now target arbitrary machine architecture.

###### TLDR
Using compile time execution, we can make the code generation part of a language small and open the door to niche optimizations by allowing direct access to the code generation process.

## Targets
Right now Zerg only targets WASI WASM for its simple stack-based virtual machine. 
In the future it would be great to support actual hardware like x86 and Arm but register machines are notoriously hard to compile for.
The goal is to research compile time execution, not implement a register allocation algorithm.
