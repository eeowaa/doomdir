#!/bin/sh

# Install NVM
curl -Lo /tmp/nvm-install.sh https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh
chmod +x /tmp/nvm-install.sh
PROFILE=/dev/null /tmp/nvm-install.sh
rm /tmp/nvm-install.sh

# Install prerequisites for `completion/ivy` module
sudo sudo dnf -y install ripgrep

# Install prerequisites for `ui/treemacs` module
sudo dnf -y install python3

# Install prerequisites for `emacs/dired` module
sudo dnf -y install coreutils

# Install prerequisites for `term/eshell` module
sudo dnf -y install fish

# Install prerequisites for `term/vterm` module
sudo dnf -y install libvterm cmake

# Install prerequisites for `checkers/spell` module
sudo dnf -y install aspell

# Install prerequisites for `tools/ansible` module
sudo dnf -y install pipx
pipx install ansible-core
ansible-galaxy collection install community.general

# Install prerequisites for `tools/docker` module
sudo dnf -y install shadow-utils fuse-overlayfs iptables
sudo systemctl disable --now docker.service docker.socket
dockerd-rootless-setuptool.sh install
curl -fsSL https://get.docker.com/rootless | sh
cat >"$HOME/.profile.d/docker-rootless.sh" <<\EOF
export PATH=$HOME/bin:$PATH
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
EOF
. "$HOME/.profile.d/docker-rootless.sh"
systemctl --user start docker.service
sudo loginctl enable-linger `whoami`

# Install prerequisites for `tools/editorconfig` module
sudo dnf -y install editorconfig

# Install prerequisites for `tools/ein` module
sudo dnf -y install python3 pipx
pipx install --include-deps jupyter

# Install prerequisites for `tools/lookup` module
sudo dnf -y install ripgrep sqlite-3

# Install prerequisites for `tools/magit` module
(cd ~/Documents/src/life/stow-dotfiles && make perl)
sudo dnf -y install perl
# TODO: Find equivalent to git-absorb
# TODO: Fix cpan install command
cpan install App::Git::Autofixup

# Install prerequisites for `tools/terraform` module
sudo dnf -y install terraform

# Install prerequisites for `lang/cc` module
sudo dnf -y install gdb glslang
# TODO: Install ccls

# Install prerequisites for `lang/csharp` module

# Install prerequisites for `lang/data` module
curl --create-dirs \
    -o ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.20.0-uber.jar \
    https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.20.0/org.eclipse.lemminx-0.20.0-uber.jar

# Install prerequisites for `lang/go` module
(cd ~/Documents/src/life/stow-dotfiles && make go)
sudo dnf -y install golang golang-x-tools-gopls golang-github-golangci-lint-1
# FIXME (see https://github.com/rocky/ssa-interp)
# curl -L https://raw.githubusercontent.com/rocky/ssa-interp/HEAD/gub-installer | bash
# FIXME (go: go.mod file not found in current directory or any parent directory.)
# go get -v -u github.com/motemen/gore/cmd/gore
# go get -v -u github.com/stamblerre/gocode
# go get -v -u golang.org/x/tools/cmd/godoc
# go get -v -u golang.org/x/tools/cmd/goimports
# go get -v -u golang.org/x/tools/cmd/gorename
# go get -v -u golang.org/x/tools/cmd/guru
# go get -v -u github.com/cweill/gotests/...
# go get -v -u github.com/fatih/gomodifytags

# Install prerequisites for `lang/json` module
dnf -y install jq
npm install -g vscode-json-languageserver

# Install prerequisites for `lang/javascript` module
npm install -g typescript javascript-typescript-langserver eslint trepan-ni

# Install prerequisites for `lang/latex` module

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint marked

# Install prerequisites for `lang/org` module
sudo dnf -y install gnuplot pandoc graphviz
# TODO: Find equivalent to pngpaste
mkdir -p ~/org/roam

# Install prerequisites for `lang/python` module
sudo dnf -y install python3 pipx
pipx install pylint
pipx install flake8
pipx install pytest
pipx install nose
pipx install pyflakes
pipx install isort
pipx install --include-deps jupyter
npm install -g pyright

# Install prerequisites for `lang/racket` module
sudo dnf -y install racket

# Install prerequisites for `lang/sh` module
sudo dnf -y install bash zsh fish powershell ShellCheck
# TODO: Find equivalent to bashdb and zshdb
npm install -g bash-language-server

# Install prerequisites for `lang/web` module
sudo dnf -y install tidy
npm install -g js-beautify stylelint
npm install -g vscode-html-languageserver-bin vscode-css-languageserver-bin

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server

# Install prerequisites for `app/irc` module
sudo dnf -y install gnutls
