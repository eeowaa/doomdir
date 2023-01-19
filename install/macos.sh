#!/bin/sh

# Install Homebrew to install system packages
curl -Lo- https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh | bash

# Install NVM to install Node.js packages
curl -Lo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh \
    | env PROFILE=/dev/null bash

# Function to install a binary asset from the latest release of a GitHub repo
brew install jq
github_binary_release() {
    local func='github_binary_release'
    local repo= asset= prefix= root= binary=
    while [ $# -gt 0 ]
    do
        case $1 in
        --repo)
            # A "<user>/<repo>" string to identify a GitHub repo
            repo=$2 ;;
        --asset)
            # An anchored regular expression used by `jq` for a named tarball
            asset=$2 ;;
        --prefix)
            # Directory to pass to `tar -C`
            prefix=$2 ;;
        --path)
            # Directory path within the tarball to the data root
            # NOTE: This variable cannot be named `path` due to conflict with ZSH
            root=$2 ;;
        --binary)
            # Path relative to the data root to the executable file
            binary=$2 ;;
        esac
        shift; shift
    done
    for arg in "$repo" "$asset" "$prefix" "$root" "$binary"
    do
        [ "X$arg" = X ] && {
            echo >&2 "ERROR: $func: missing argument"
            return 1
        }
    done
    local url=`
        curl -s https://api.github.com/repos/$repo/releases/latest | jq -r \
        '.assets[] | select(.name | test("^'"$asset"'$")) | .browser_download_url'
    `
    [ "X$url" = X ] && {
        echo >&2 "ERROR: $func: could not find URL"
        return 1
    }
    local canonical_path=`readlink -m "$prefix/$root"`
    [ -e "$canonical_path" ] && {
        printf "\
$func: found existing: $canonical_path
$func: (recursively) delete? [y/N]: "
        read delete
        case $delete in
        [yY]*)
            rm -rf "$canonical_path" ;;
        *)  echo >&2 "ERROR: $func: refusing to download"
            return 1 ;;
        esac
    }
    mkdir -p "$prefix" "$HOME/.local/bin"
    curl -Lo- "$url" | tar -C "$prefix" -xzf -
    [ -x "$canonical_path/$binary" ] || {
        echo >&2 "ERROR: $func: not an executable file: $canonical_path/$binary"
        return 1
    }
    ln -sf "$canonical_path/$binary" "$HOME/.local/bin"
}

# Install prerequisites for `completion/vertico` module
brew install ripgrep

# Install prerequisites for `ui/doom` module
brew install font-terminus

# TODO: Comic Mono

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

# Install prerequisites for `tools/debugger` module
# https://stackoverflow.com/questions/18423124/please-check-gdb-is-codesigned-see-taskgated8-how-to-get-gdb-installed-w
brew install gdb

# Install prerequisites for `tools/direnv` module
brew install direnv

# Install prerequisites for `tools/docker` module
brew install docker
npm install -g dockerfile-language-server-nodejs

# Install prerequisites for `tools/editorconfig` module
brew install editorconfig

# Install prerequisites for `tools/ein` module
brew install python
pipx install --include-deps jupyter

## Install the bash_kernel package into the jupyter virtualenv
pipx inject jupyter bash_kernel

## Run the installer script to install the kernel in the virtualenv
. "$(pipx environment -v PIPX_LOCAL_VENVS)/jupyter/bin/activate"
python -m bash_kernel.install --sys-prefix
deactivate

## Verify that the bash kernel is visible to jupyter
jupyter kernelspec list

# Install prerequisites for `tools/lookup` module
brew install ripgrep sqlite3

# Install prerequisites for `tools/magit` module
(cd ~/Documents/src/life/stow-dotfiles && make perl)
brew install perl git-absorb
cpan install App::Git::Autofixup

# Install prerequisites for `tools/nginx` module
pipx install --python `which python3.10` nginx-language-server

# Install prerequisites for `tools/pdf` module
brew install pkg-config poppler automake

# Install prerequisites for `tools/pdf` module
sudo dnf -y install pkgconf pkgconf-pkg-config poppler automake

# Install prerequisites for `tools/terraform` module
brew install terraform hashicorp/tap/terraform-ls

# Install prerequisites for `lang/cc` module
brew install ccls gdb glslang

# Install prerequisites for `lang/csharp` module

# Install prerequisites for `lang/data` module
curl --create-dirs \
    -o ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.20.0-uber.jar \
    https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.20.0/org.eclipse.lemminx-0.20.0-uber.jar

# Install prerequisites for `lang/go` module
(cd ~/Documents/src/life/stow-dotfiles && make go)
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
npm install -g vscode-langservers-extracted

# Install prerequisites for `lang/javascript` module
npm install -g typescript typescript-language-server eslint trepan-ni

# Install prerequisites for `lang/latex` module
brew install mactex texlab wget

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint-cli marked

# Install prerequisites for `lang/org` module
brew install gnuplot pandoc graphviz pngpaste
mkdir -p ~/org/roam

# Install prerequisites for `lang/python` module
brew install python
npm install -g pyright
pipx install pipenv
pipx install pylint
pipx install flake8
pipx install pytest
pipx install nose2
pipx install pyflakes
pipx install isort
pipx install --include-deps jupyter

## Debugging
pip3 install --user debugpy

# Install prerequisites for `lang/racket` module
brew install --cask racket

# Install prerequisites for `lang/sh` module
brew install bash zsh fish powershell bashdb zshdb shellcheck
npm install -g bash-language-server

# Install prerequisites for `lang/web` module
brew install tidy-html5
npm install -g js-beautify stylelint
npm install -g vscode-html-languageserver-bin vscode-css-languageserver-bin

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server

# Install prerequisites for `app/irc` module
brew install gnutls
