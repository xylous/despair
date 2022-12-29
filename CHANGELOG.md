# Changelog for `despair`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.2.0.0 - 2022-12-29

- parser: add support for comments, i.e. all characters that are not the 8
    controls
- implement optimiser, which simplifies instructions and executes them so that
    less computing is done
- add sample brainfuck programs/files

## 0.1.0.0 - 2022-12-27

- implement parser
- implement evaluator, with support for ALL operations
- dynamically allocate tape (memory) when it's needed
