# Emacs configuration
This is my Emacs configuration

## Using
Clone the repo and copy the content into your emacs directory (Backup
your existing config before). For example if you using an Emacs
version >= 27
```
git clone https://gitlab.com/flexw/emacs-config ~/.config/emacs
```
You may need some extra programs to get everything working (look
through comments in the code).

## Platform specific notes

### Windows
To use grep and other tools that use unix tools, install `msys` and
add the `/usr/bin` directory of `msys2` to path. For the `C++`
language server to work, install clang and put it into the path. For
`magit` to work, `git` needs to be installed and also added to path.
