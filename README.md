[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CI](https://github.com/tminor/jsonnet-mode/workflows/CI/badge.svg)](https://github.com/tminor/jsonnet-mode/actions?query=workflow%3A%22CI%22+branch%3Amain)
[![Coverage Status](https://coveralls.io/repos/github/tminor/jsonnet-mode/badge.svg?branch=main)](https://coveralls.io/github/tminor/jsonnet-mode?branch=main)

# `jsonnet-mode`

Provides Emacs font-lock, indentation, and some useful functions for the Jsonnet templating language.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [jsonnet-mode](#jsonnet-mode)
- [Dependencies](#dependencies)
- [Features](#features)
    - [Indentation](#indentation)
    - [Formatting](#formatting)
    - [Rendering](#rendering)
    - [Navigation](#navigation)
- [Configuration](#configuration)

<!-- markdown-toc end -->

# Dependencies

The `jsonnet-eval-buffer` method depends on the `jsonnet` binary,
which should be available on your `exec-path`. For installation
details, see
[here](https://github.com/google/go-jsonnet#installation-instructions)
for `go-jsonnet` and
[here](https://github.com/google/jsonnet#packages) for the C++
implementation.

# Features

## Indentation

Indentation is provided by a function implemented with
[SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html). To
use the old indentation function (not recommended), set
`jsonnet-use-smie` to `nil`.

## Formatting

`jsonnet-reformat-buffer` (bound to <kbd>C-c C-r</kbd> by default)
uses the `jsonnetfmt` utility to reformat your
buffer. `jsonnet-mode`'s indentation function should match its output
in most cases.

## Rendering

`jsonnet-eval-buffer` runs `jsonnet` to evaluate and render a
`jsonnet-mode` buffer. It's bound to <kbd>C-c C-c</kbd> by default. If
any errors are encountered, they should be conveniently displayed in
`compilation-mode`.

It will create a buffer called `*jsonnet output*`. Default display
behaviour of these buffers can be customized using
`display-buffer-alist` (see [The Zen of Buffer
Display](https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html)
for examples of how to do this).

## Navigation

`jsonnet-mode` also provides some methods to make navigation easier. In
particular, `jsonnet-jump` (bound to <kbd>C-c C-f</kbd>) allows you to jump to the
definition of a given identifier.

# Configuration

There are several customizable parameters that you may configure in this mode:

- `jsonnet-command` allows you to indicate which Jsonnet binary should
  be used to render a JSON document.
- `jsonnet-command-options` can be specified if additional options
  should be sent with `jsonnet-command`.
- `jsonnet-library-search-directories` specifies the sequence of
  Jsonnet library search directories use during evaluation.  Relative
  paths in this sequence must resolve from the directory of the buffer
  being evaluated.
- `jsonnet-enable-debug-print` will cause methods in jsonnet-mode to
  write messages to the status bar if enabled.
- `jsonnet-use-smie` enables SMIE-provided indentation.
- `jsonnet-indent-level` changes the number of spaces used to indent
  Jsonnet code.
