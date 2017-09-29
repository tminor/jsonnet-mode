[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# jsonnet-mode

Provides Emacs font-lock, indentation, and some useful functions for the Jsonnet templating language.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Dependencies](#dependencies)

<!-- markdown-toc end -->

# Dependencies

The `jsonnet-eval` method depends on the `jsonnet` binary, which should be available on your `exec-path`. Install this on OSX with Homebrew by running `brew install jsonnet`.

# Indentation

The indentation rules are a set of rules which I put together based on my experience with Jsonnet. You can see them by looking at the implementation of `jsonnet-calculate-indent`.

# Configuration

There are two customizable parameters which you may configure in this mode:

- `jsonnet-command` allows you to indicate which Jsonnet binary should be used to render a JSON document.
- `jsonnet-enable-debug-print` will cause methods in jsonnet-mode to write messages to the status bar if enabled.

# Known Issues

Indentation support is not perfect. Here are a couple known problems with it:
- When the parameter list to a function is multiple lines long, we should indent 2x tab-width.
