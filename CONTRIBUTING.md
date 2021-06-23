# Contributing

We love pull requests from everyone. By participating in this project,
you agree to abide by the [code of conduct](https://github.com/tminor/jsonnet-mode/blob/master/CODE_OF_CONDUCT.md).

Fork, then clone the repo:

```bash
git clone git@github.com:your-username/jsonnet-mode.git
```

This project uses [Cask](https://github.com/cask/cask); install it by
following Cask's [installation
documentation](https://github.com/cask/cask#installation).

To install `jsonnet-mode`'s dependencies, use Cask:

```bash
cask install
```

After adding your change, ensure that the unit tests pass:

```bash
cask exec buttercup -L .
```

After you've committed your changes and tested them, [submit a pull
request](https://github.com/tminor/jsonnet-mode/compare/).

Some things that will increase the chance that your pull request is
accepted:

- Follow bbatsov's [emacs lisp style guide](https://github.com/bbatsov/emacs-lisp-style-guide); and
- Write a [good commit message](https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
