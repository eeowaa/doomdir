#!/bin/sh

# Install Homebrew
curl -Lo- https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh | bash

# Install NVM
curl -Lo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh | bash

# Install prerequisites for `completion/ivy` module
brew install ripgrep

# Install prerequisites for `ui/treemacs` module
brew install python3

# Install prerequisites for `emacs/dired` module
brew install coreutils

# Install prerequisites for `term/eshell` module
brew install fish

# Install prerequisites for `term/vterm` module
brew install libvterm cmake

# Install prerequisites for `checkers/spell` module
brew install aspell

# Install prerequisites for `tools/ansible` module
brew install ansible

# Install prerequisites for `tools/docker` module
brew install docker
npm install -g dockerfile-language-server-nodejs

# Install prerequisites for `tools/editorconfig` module
brew install editorconfig

# Install prerequisites for `tools/ein` module
brew install python
pipx install --include-deps jupyter

# Install prerequisites for `tools/lookup` module
brew install ripgrep sqlite3

# Install prerequisites for `tools/magit` module
cd ~/stow-dotfiles && make perl
brew install perl git-absorb
cpan install App::Git::Autofixup

# Install prerequisites for `tools/pdf` module
brew install pkg-config poppler automake

# Install prerequisites for `tools/terraform` module
brew install terraform

# Install prerequisites for `lang/cc` module
brew install ccls gdb glslang

# Install prerequisites for `lang/data` module
curl --create-dirs \
    -o ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.13.1-uber.jar \
    https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.13.1/org.eclipse.lemminx-0.13.1-uber.jar

# Install prerequisites for `lang/go` module
cd ~/stow-dotfiles && make go
brew install go gopls golangci-lint
# FIXME (see https://github.com/rocky/ssa-interp)
# curl -Lo- https://raw.githubusercontent.com/rocky/ssa-interp/HEAD/gub-installer | bash
go get -v -u github.com/motemen/gore/cmd/gore
go get -v -u github.com/stamblerre/gocode
go get -v -u golang.org/x/tools/cmd/godoc
go get -v -u golang.org/x/tools/cmd/goimports
go get -v -u golang.org/x/tools/cmd/gorename
go get -v -u golang.org/x/tools/cmd/guru
go get -v -u github.com/cweill/gotests/...
go get -v -u github.com/fatih/gomodifytags

# Install prerequisites for `lang/json` module
brew install jq
npm install -g vscode-json-languageserver

# Install prerequisites for `lang/javascript` module
npm install -g typescript javascript-typescript-langserver eslint trepan-ni

# Install prerequisites for `lang/latex` module
brew install mactex texlab wget

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint marked

# Install prerequisites for `lang/org` module
brew install gnuplot pandoc graphviz pngpaste
mkdir -p ~/org/roam

# Install prerequisites for `lang/python` module
brew install python
pipx install pylint
pipx install flake8
pipx install pytest
pipx install nose
pipx install pyflakes
pipx install isort
pipx install --include-deps jupyter
npm install -g pyright

# Install prerequisites for `lang/sh` module
brew install bash zsh fish powershell bashdb zshdb shellcheck
npm install -g bash-language-server

# Install prerequisites for `lang/web` module
brew install tidy-html5
npm install -g js-beautify stylelint

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server

# Install prerequisites for `app/irc` module
brew install gnutls
