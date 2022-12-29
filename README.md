# despair

`despair` is a Brainfuck interpreter written in Haskell. It's rather slow, but
it'll get there eventually.

## Getting started

### Requirements

- stack, for building

### Installation

Clone this repository locally and build:

```
$ git clone https://github.com/xylous/despair despair
$ cd despair
$ stack build
```

### Usage

When ran, `despair` accepts only one argument: the path to a brainfuck file,
which it parses and executes, eventually printing the output of the machine.

## Roadmap

- [x] implement parser
    - [x] parse all character-commands
    - [x] parse comments
        - [x] extension: comments starting with `;`
    - [ ] return precise error messages
        - [ ] character number and line number
        - [ ] reason for error
- [x] implement evaluator
    - [x] basic operations: `+`, `-`, `>`, `<`
    - [x] loops: `[`, `]`
    - [x] I/O operations: `.`, `,`
    - [x] optimise
        - [x] reduce arithmetic whenever possible: e.g. `+++-` would change a
            cell's value by a total of 3
        - [x] simplify pointer movements: e.g. `>>>>>` moves the pointer right
            by five cells
        - [ ] ~~associate pointer positions with operations, creating pairs~~
- [ ] add tests
    - [x] sample Brainfuck programs
    - [ ] parser tests
    - [ ] evaluator tests
        - [ ] optimiser tests
- [ ] extra features
    - [x] arbitrary cell indexes - pointer can be anywhere
    - [ ] allow using different cell sizes: 8, 16, 32, 64 bits (for wrapping)
    - [ ] specify signed or unsigned cell bytes
- [ ] REPL mode
    - [ ] `reset` command, for bringing the machine back to the initial state
- [ ] debug mode - run simulation step-by-step
    - NOTE: this mode would be identical to a REPL except
    - [ ] execute next instruction(s)
    - [ ] undo instruction(s), i.e. rewind
    - [ ] `. N` command - display the Nth cell's value
    - [ ] `show` command - print the state of the entire machine

## Contributing

Pull requests are welcome. For major changes, please open an issue first to
discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[BSD3](./LICENSE)
