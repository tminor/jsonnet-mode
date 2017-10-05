# Contributing

We love pull requests from everyone. By participating in this project, you agree
to abide by the [code of conduct].

[code of conduct]: https://github.com/mgyucht/jsonnet-mode/blob/master/CODE_OF_CONDUCT.md

Fork, then clone the repo:

    git clone git@github.com:your-username/jsonnet-mode.git

Make your change. Make sure the project byte-compiles cleanly and that Emacs approves of your new
documentation:

    M-x package-install SPC package-lint RET RET
    (package-lint-current-buffer)
    (checkdoc)

Push to your fork and [submit a pull request][pr].

[pr]: https://github.com/mgyucht/jsonnet-mode/compare/

At this point you're waiting on me. I like to at least comment on pull requests within three
business days (and, typically, one business day). We may suggest some changes or improvements or
alternatives.

Some things that will increase the chance that your pull request is accepted:

* Follow bbatsov's [emacs lisp style guide][style].
* Write a [good commit message][commit].

[style]: https://github.com/bbatsov/emacs-lisp-style-guide
[commit]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
