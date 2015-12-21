# 🎵#️⃣
A programming language for exploring and creating music

# Authors
- [Kevin Chen](http://kevinchen.co/)
- Brian Kim
- Edward Li

# Compiling

Run the `install_dependencies.sh` script to set up submodules, install OCaml, and install third-party libraries.

This script is maintained on Ubuntu 15.04 and OS X 10.11 El Capitan, although it will probably work on Ubuntu 14.x,
OS X 10.10 Yosemite, and Debian as well.

Once you have the dependencies, just `cd` into the directory you want and `make`.

# Contributing

1. Grab the lastest master: `git checkout master && git pull origin master`
2. Create your branch: `git checkout -b alice_feature_name`
3. Make some changes: `git commit ...`
4. Squash commits: `git rebase -i master`, then change everything except the first commit to `squash`
5. Rebase on the latest master: `git checkout master && git pull origin master && git rebase master alice_feature_name`
6. Run tests: `make test`
7. Push for code review: `git push -f origin alice_feature_name` (only use push -f on feature branches, not master)

# Syntax Highlighting

Syntax highlighting is provided for Sublime Text. In Sublime, go to:
Preferences -> Browse Packages -> User
and copy the file (in style/)
note-hashtag.tmLanguage
into that directory.
