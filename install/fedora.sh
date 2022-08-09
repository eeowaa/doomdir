#!/bin/sh

# Install NVM to install packages
curl -Lo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh \
    | env PROFILE=/dev/null bash

# Install pipx to install packages
sudo dnf -y install pipx

# Function to install a binary asset from the latest release of a GitHub repo
sudo dnf -y install jq
github_binary_release() {
    local func='github_binary_release'
    local repo= asset= prefix= root= binary=
    while [ $# -gt 0 ]
    do
        case $1 in
        --repo) repo=$2 ;;
        --asset) asset=$2 ;;
        --prefix) prefix=$2 ;;
        --path) root=$2 ;; # cannot use "path" due to conflict with ZSH
        --binary) binary=$2 ;;
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
        '.assets[] | select(.name == "'"$asset"'") | .browser_download_url'
    `
    [ "X$url" = X ] && {
        echo >&2 "ERROR: $func: could not find URL"
        return 1
    }
    [ -e "$prefix/$root" ] && {
        local canonical_path=`readlink -e "$prefix/$root"`
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

# Install prerequisites for `completion/ivy` module
sudo sudo dnf -y install ripgrep

# Install prerequisites for `ui/doom` module
sudo dnf -y install terminus-fonts

# Comic Mono
# <https://dtinth.github.io/comic-mono-font>
# <https://docs.fedoraproject.org/en-US/quick-docs/fonts/#unpackaged>
npm install -g comic-mono
mkdir -p "$XDG_DATA_HOME/fonts"
ln -s "`npm root -g`/comic-mono" "$XDG_DATA_HOME/fonts"
fc-cache -v

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
pipx install ansible-core
ansible-galaxy collection install community.general

# Install prerequisites for `tools/debugger` module
sudo dnf -y install lldb gdb unzip
nvm install node

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
sudo dnf -y install terraform terraform-ls

# Install prerequisites for `lang/cc` module

## GCC
sudo dnf -y install gcc gdb

## clangd
sudo dnf -y install clang clang-tools-extra

## ccls
sudo dnf -y install cmake clang clang-devel llvm-devel rapidjson
(
    set -e
    mkdir -p "$HOME/.local/src" && cd "$HOME/.local/src"
    if [ -d ccls ]
    then git -C ccls pull -f
    else git clone --depth=1 --recursive https://github.com/MaskRay/ccls
    fi
    cd ccls && rm -rf Release
    cmake -H. -BRelease -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
    cmake --build Release
    sudo cmake --install Release
)

## GLSL
sudo dnf -y install glslang

## CMake
sudo dnf -y install cmake
pipx install cmake-language-server

# Install prerequisites for `lang/csharp` module

## dotnet
sudo dnf -y install dotnet

## omnisharp-roslyn
github_binary_release \
    --repo OmniSharp/omnisharp-roslyn \
    --asset omnisharp-linux-x64-net6.0.tar.gz \
    --prefix "$HOME/.local/opt/microsoft/omnisharp-roslyn" \
    --path . \
    --binary OmniSharp

# netcoredbg
github_binary_release \
    --repo Samsung/netcoredbg \
    --asset netcoredbg-linux-amd64.tar.gz \
    --prefix "$HOME/.local/opt/microsoft" \
    --path netcoredbg \
    --binary netcoredbg

# Install prerequisites for `lang/data` module
curl --create-dirs \
    -o ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.20.0-uber.jar \
    https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.20.0/org.eclipse.lemminx-0.20.0-uber.jar

# Install prerequisites for `lang/go` module
(cd ~/Documents/src/life/stow-dotfiles && make go)
export GOPATH=$HOME/go

## Required dependencies
sudo dnf -y install golang
go install golang.org/x/tools/gopls@latest
go install github.com/x-motemen/gore/cmd/gore@latest
go install github.com/stamblerre/gocode@latest
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install github.com/cweill/gotests/gotests@latest
go install github.com/fatih/gomodifytags@latest

## Linting
asset=`
    curl -s https://api.github.com/repos/golangci/golangci-lint/releases/latest | jq -r \
    '.assets[] | select(.name | endswith("-linux-amd64.tar.gz")) | .name'
`
github_binary_release \
    --repo golangci/golangci-lint \
    --asset "$asset" \
    --prefix "$HOME/.local/opt/golangci" \
    --path "${asset%*.tar.gz}" \
    --binary golangci-lint

## Debugging
sudo dnf -y install llvm

# Install prerequisites for `lang/json` module
dnf -y install jq
npm install -g vscode-langservers-extracted

# Install prerequisites for `lang/javascript` module
npm install -g typescript typescript-language-server eslint trepan-ni

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint-cli marked
pipx install grip

# Install prerequisites for `lang/org` module
sudo dnf -y install gnuplot pandoc graphviz sqlite-3
pipx install --include-deps jupyter
mkdir -p ~/org/roam

# Install prerequisites for `lang/python` module
sudo dnf -y install python3 pipx
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
sudo dnf -y install racket
raco pkg install --auto racket-langserver

# Install prerequisites for `lang/rust` module
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- --no-modify-path --quiet -y
cargo install cargo-check
rustup component add rustfmt-preview clippy-preview

## rust-analyzer
rustup component add rust-src
rustup +nightly component add rust-analyzer-preview
ln -s ~/.rustup/toolchains/nightly-*/bin/rust-analyzer ~/.cargo/bin

## rls
rustup component add rls rust-analysis rust-src

# Install prerequisites for `lang/sh` module
sudo dnf -y install bash zsh fish powershell ShellCheck
npm install -g bash-language-server

## FIXME bashdb
## This does not work because there is no version of bashdb that matches the
## current version of bash on my system (my bash is too new)
# mkdir -p "$HOME/.local/src/bashdb"
# version=5.0-1.1.2
# [ -d "$HOME/.local/src/bashdb/bashdb-$version" ] || {
#     url=https://sourceforge.net/projects/bashdb/files/bashdb/"$version"/bashdb-"$version".tar.gz/download
#     curl -Lo- "$url" | tar -C "$HOME/.local/src/bashdb" -xzf -
# }

## TODO zshdb

# Install prerequisites for `lang/web` module
sudo dnf -y install tidy
npm install -g js-beautify stylelint
npm install -g vscode-html-languageserver-bin vscode-css-languageserver-bin

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server

# Install prerequisites for `app/everywhere` module
# XXX This will only work for X sessions (not Wayland)
sudo dnf -y install xclip xdotool xprop xwininfo

# Install prerequisites for `app/irc` module
sudo dnf -y install gnutls
