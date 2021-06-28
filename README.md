[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CI](https://github.com/tminor/jsonnet-mode/workflows/CI/badge.svg)](https://github.com/tminor/jsonnet-mode/actions?query=workflow%3A%22CI%22+branch%3Amain)
[![Coverage Status](https://coveralls.io/repos/github/tminor/jsonnet-mode/badge.svg?branch=code-coverage)](https://coveralls.io/github/tminor/jsonnet-mode?branch=code-coverage)

# jsonnet-mode

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

The `jsonnet-eval` method depends on the `jsonnet` binary, which should be
available on your `exec-path`. Install this on OSX with Homebrew by running
`brew install jsonnet`.

# Features

## Indentation

Default indentation is provided by `jsonnet-calculate-indent` and
accommodates basic indentation. It isn't consistent with `jsonnetfmt`
and will be replaced by an SMIE based implementation in the near
future. If you prefer the newer implementation, you may enable it by
setting `jsonnet-use-smie` to `t`.

## Formatting

Closely related to indentation is formatting. By running
`jsonnet-reformat-buffer` (bound to `C-c C-r`), the `jsonnet fmt` utility will
reformat your buffer and make it pretty.

## Rendering

To get a snapshot of what your Jsonnet file will render to, run `jsonnet-eval`
(bound to `C-c C-e`). This will popup another window and show the result of
running `jsonnet-command` on the current buffer.

## Navigation

`jsonnet-mode` also provides some methods to make navigation easier. In
particular, `jsonnet-jump` (bound to `C-c C-f`) allows you to jump to the
definition of a given identifier.

# Configuration

There are three customizable parameters that you may configure in this mode:

- `jsonnet-command` allows you to indicate which Jsonnet binary should be used to render a JSON document.
- `jsonnet-library-search-directories` specifies the sequence of Jsonnet library search directories use during evaluation.
  Relative paths in this sequence must resolve from the directory of the buffer being evaluated.
- `jsonnet-enable-debug-print` will cause methods in jsonnet-mode to write messages to the status bar if enabled.
- `jsonnet-use-smie` enables SMIE-provided indentation.
