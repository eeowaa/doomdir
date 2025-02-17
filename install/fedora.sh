#!/bin/sh

# Prevent Emacs 29.4 from being installed
# <https://github.com/doomemacs/doomemacs/issues/7915>
sudo dnf -y install 'dnf-command(versionlock)'
sudo dnf versionlock exclude --raw 'emacs-1:29.4-*'

# Install the newest release of the desired <name>-<epoch>:<version> RPM and
# prevent upgrades (even to a newer <name>-<epoch>:<version>-<release>) to avoid
# ever having to run 'doom upgrade' unexpectedly after upgrading system packages
# NOTE: "epoch" is an RPM construct and is required for proper version comparison
target="1:29.2"
sudo dnf versionlock delete emacs
rpm -q emacs >/dev/null || {
    sudo dnf -y install "emacs-$target" || :
} || {
    current=`rpm -q --qf '%{epoch}:%{version}' emacs`
    case `rpm -E "%{lua:print(rpm.vercmp('$current', '$target'))}"` in
     0) ;;
    -1) sudo dnf -y upgrade "emacs-$target" ;;
     1) sudo dnf -y downgrade "emacs-$target" ;;
    esac
}
current=`rpm -q --qf '%{epoch}:%{version}' emacs`
if [ "X$target" = "X$current" ]
then sudo dnf versionlock add emacs
else cat >&2 <<EOF
WARNING: Incorrect Emacs version (using $actual_version, want $target_version)
EOF
fi

# Obtain Emacs source code corresponding to installed RPM
(
    # Create ~/rpmbuild directory tree
    sudo dnf -y install rpmdevtools
    rpmdev-setuptree

    # Download Emacs source RPM into ~/rpmbuild/SRPMS
    cd "`rpm --eval '%{_srcrpmdir}'`"
    sourcerpm=`rpm -q --qf '%{sourcerpm}' emacs`
    [ -f "$sourcerpm" ] || dnf download emacs --source

    # Install Mock to silence rpmbuild warnings (though we'll use rpmbuild directly)
    # <https://unix.stackexchange.com/questions/459384/how-to-eliminate-the-warnings-mockbuild-does-not-exist>
    sudo dnf -y install mock
    grep -q '^mockbuild:' /etc/passwd || sudo useradd -r mock
    sudo usermod -G mock mockbuild
    sudo usermod -aG mock `whoami`

    # Unpack sources and apply patches
    sudo dnf -y install rpm-build
    rpmbuild -rp "$sourcerpm"

    # Install build dependencies
    sudo dnf -y builddep --srpm "$sourcerpm"

    # Configure sources to match the installed Emacs package
    version=`rpm -q --qf '%{version}' emacs`
    srcdir=`rpm --eval '%{_builddir}'`/emacs-$version
    flags=`command emacs -Q --batch --eval "\
    (let ((text-quoting-style 'straight))
      (message system-configuration-options))" 2>&1`
    cd "$srcdir"
    eval "./configure $flags"

    # NOTE Unfortunately the spec file does not have a %conf stage. It just has
    # one big %build stage that unconditionally configures and compiles multiple
    # builds of Emacs corresponding to different emacs-* packages. The above
    # script block just configures a single build of Emacs, specifically the one
    # in use, and does not compile anything.

    # Symlink Emacs source tree to platform-independent source directory
    mkdir -p ~/.local/src/emacs && cd ~/.local/src/emacs
    ln -sf "$srcdir" .
)

# Install openssh-askpass to install Elisp packages
sudo dnf -y install openssh-askpass

# Install NVM
curl -fsSLo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh \
    | env PROFILE=/dev/null bash

# Install NodeJS and NPM through NVM
nvm install node

# Install pipx to install Python packages
sudo dnf -y install pipx

# Function to install a binary asset from the latest release of a GitHub repo
sudo dnf -y install jq
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
        curl -fsSLo- https://api.github.com/repos/$repo/releases/latest | jq -r \
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
    curl -fsSLo- "$url" | tar -C "$prefix" -xzf -
    [ -x "$canonical_path/$binary" ] || {
        echo >&2 "ERROR: $func: not an executable file: $canonical_path/$binary"
        return 1
    }
    ln -sf "$canonical_path/$binary" "$HOME/.local/bin"
}

# Scaffolding for tree-sitter support

## Install and use the correct version of the tree-sitter CLI
npm -g install tree-sitter-cli@0.19.3
for ts in `which -a tree-sitter | sort -u`
do
    case `$ts --version` in
    tree-sitter\ 0.19.3*)
        export PATH=${ts%/*}:$PATH
        break ;;
    esac
done

## Install Cask
if [ -d ~/.local/opt/cask ]
then
    git -C ~/.local/opt/cask pull
else
    mkdir -p ~/.local/opt
    git clone https://github.com/cask/cask ~/.local/opt/cask
    ln -sf ~/.local/opt/cask/bin/cask ~/.local/bin
fi

## Obtain elisp-tree-sitter source code in its own directory
[ -d ~/.local/src/emacs/tree-sitter-langs ] || {
    mkdir -p ~/.local/src/emacs
    git clone https://github.com/emacs-tree-sitter/tree-sitter-langs \
        ~/.local/src/emacs/tree-sitter-langs
}

## Install dependencies for tree-sitter-langs
cd ~/.local/src/emacs/tree-sitter-langs
cask install

# Install prerequisites for `completion/vertico` module
sudo dnf -y install ripgrep

# Install prerequisites for `ui/doom` module
sudo dnf -y install terminus-fonts

## Comic Mono
## <https://dtinth.github.io/comic-mono-font>
## <https://docs.fedoraproject.org/en-US/quick-docs/fonts/#unpackaged>
npm install -g comic-mono
mkdir -p "$XDG_DATA_HOME/fonts"
ln -sf "`npm root -g`/comic-mono" "$XDG_DATA_HOME/fonts"

## Iosevka Comfy
if [ -e "$XDG_DATA_HOME/fonts/iosevka-comfy/.git" ]
then
    git -C "$XDG_DATA_HOME/fonts/iosevka-comfy" pull
else
    git clone --depth 1 \
        'https://git.sr.ht/~protesilaos/iosevka-comfy' \
        "$XDG_DATA_HOME/fonts/iosevka-comfy"
fi

## Update fonts
fc-cache -v

# Install prerequisites for `ui/treemacs` module
sudo dnf -y install python3

# Install prerequisites for `emacs/dired` module
sudo dnf -y install coreutils

# Install prerequisites for `term/eshell` module
sudo dnf -y install fish

# Install prerequisites for `term/vterm` module
sudo dnf -y install libvterm cmake

## Soft line wrapping
## <https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359>
sudo dnf -y install screen

# Install prerequisites for `checkers/spell` module
sudo dnf -y install aspell

# Install prerequisites for `checkers/grammar` module
# Reference: <https://raw.githubusercontent.com/languagetool-org/languagetool/master/install.sh>
sudo dnf -y install unzip java-latest-openjdk-headless
(
    set -e
    mkdir -p ~/.local/src/doom
    cd ~/.local/src/doom

    # Download stable release of LanguageTool
    rm -f LanguageTool-stable.zip
    curl -fsSLo LanguageTool-stable.zip \
         -l https://languagetool.org/download/LanguageTool-stable.zip

    # Unzip the tool and determine the release number
    release_dir=$(unzip -u LanguageTool-stable.zip | awk '!x&&/creating:/{print$2;x=1}')

    # Move into place and clean up
    rm -rf LanguageTool
    mv "$release_dir" LanguageTool
    rm LanguageTool-stable.zip
)

# Install prerequisites for `tools/ansible` module
pipx install ansible-core
ansible-galaxy collection install community.general

# Install prerequisites for `tools/bitwarden` module
npm install -g @bitwarden/cli

# Install prerequisites for `tools/debugger` module
sudo dnf -y install lldb gdb unzip
nvm install node

# Install prerequisites for `tools/direnv` module
sudo dnf -y install direnv

# Install prerequisites for `tools/docker` module
which dockerd-rootless-setuptool.sh >/dev/null 2>&1 || {
    sudo systemctl disable --now docker.service docker.socket
    curl --proto '=https' --tlsv1.2 -fsSLo- https://get.docker.com | sudo sh -s
    dockerd-rootless-setuptool.sh install
    sudo loginctl enable-linger `whoami`
    systemctl --user start docker.service
}
npm install -g dockerfile-language-server-nodejs

# Install prerequisites for `tools/editorconfig` module
sudo dnf -y install editorconfig

# Install prerequisites for `tools/Kubernetes` module

## Helm language server
curl -fsSLo ~/.local/bin/helm_ls \
    https://github.com/mrjosh/helm-ls/releases/download/master/helm_ls_linux_amd64

## Build and install the tree-sitter grammar for Go templates
(
    cd ~/.local/src/emacs/tree-sitter-langs

    # Register a submodule for tree-sitter-go-template
    git submodule add -b master -- \
        https://github.com/ngalaiko/tree-sitter-go-template repos/gotmpl
    ed .gitmodules <<\EOF
/\[submodule "repos\/gotmpl"]/a
	update = none
	ignore = dirty
.
wq
EOF

    # Copy the highlights file into place (for internal consistency)
    mkdir -p queries/gotmpl
    cp repos/gotmpl/queries/highlights.scm queries/gotmpl

    # Build the grammar for Go templates
    script/compile gotmpl

    # Install the shared object into place
    mkdir -p ~/.tree-sitter/bin
    cp bin/gotmpl.so ~/.tree-sitter/bin

    # Install the highlights file into place
    mkdir -p ~/.tree-sitter/queries/gotmpl
    cp queries/gotmpl/highlights.scm ~/.tree-sitter/queries/gotmpl
)

# Install prerequisites for `tools/lookup` module
sudo dnf -y install ripgrep sqlite wordnet

# Install prerequisites for `tools/magit` module
(cd ~/Documents/src/life/stow-dotfiles && make perl)
sudo dnf -y install perl
# TODO: Find equivalent to git-absorb
# TODO: Fix cpan install command
cpan install App::Git::Autofixup

# Install prerequisites for `tools/nginx` module
pipx install --python `which python3.10` nginx-language-server

# Install prerequisites for `lang/cc` module

## GCC
sudo dnf -y install gcc gdb

## clangd
sudo dnf -y install clang clang-tools-extra

## ccls
sudo dnf -y install cmake clang clang-devel llvm-devel rapidjson-devel
(
    set -e
    mkdir -p "$HOME/.local/src/doom" && cd "$HOME/.local/src/doom"
    if [ -d ccls ]
    then git -C ccls fetch && git -C ccls reset --hard '@{u}'
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

# Install prerequisites for `lang/data` module
curl --create-dirs \
    -fsSLo ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.20.0-uber.jar \
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
    curl -fsSLo- https://api.github.com/repos/golangci/golangci-lint/releases/latest | jq -r \
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
sudo dnf -y install jq
npm install -g vscode-langservers-extracted

# Install prerequisites for `lang/javascript` module
npm install -g typescript typescript-language-server eslint trepan-ni

## Debugging
(
    set -e
    mkdir -p "$HOME/.local/src/doom" && cd "$HOME/.local/src/doom"
    if [ -d vscode-node-debug2 ]
    then git -C vscode-node-debug2 fetch && git -C vscode-node-debug2 reset --hard '@{u}'
    else git clone --depth=1 --recursive https://github.com/microsoft/vscode-node-debug2
    fi
    cd vscode-node-debug2
    git clean -fd
    npm ci
    npm run package # FIXME: This has been breaking lately
    set -- *.vsix
    test $# -eq 1
    destdir=$XDG_CONFIG_HOME/emacs/.local/etc/dap-extension/vscode/ms-vscode.node-debug2
    mkdir -p "$destdir"
    unzip -o $1 -d "$destdir"
)

# Install prerequisites for `lang/latex` module
(cd ~/Documents/src/life/stow-dotfiles && make lua)
sudo dnf -y install texlive-scheme-basic

## Install lsp server: digestif
sudo dnf -y install luarocks lua-devel
luarocks --local install digestif

## Install ulem.sty for org preview
sudo dnf -y install texlive-ulem

# Install prerequisites for `lang/lua` module
github_binary_release \
    --repo LuaLS/lua-language-server \
    --asset 'lua-language-server-.*-linux-x64.tar.gz' \
    --prefix "$HOME/.local/opt/lua-language-server" \
    --path . \
    --binary bin/lua-language-server

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint-cli marked
pipx install grip

## Install reveal.js for Markdown presentations
## <https://revealjs.com/installation/>
(
    set -e
    mkdir -p "$HOME/.local/src/utils" && cd "$HOME/.local/src/utils"
    if [ -d reveal.js ]
    then git -C reveal.js fetch && git -C reveal.js reset --hard '@{u}'
    else git clone --depth=1 https://github.com/hakimel/reveal.js.git
    fi
    cd reveal.js && npm install
)

## Generate a wrapper script for reveal.js
cat > ~/.local/bin/revealjs <<\EOF
#!/usr/bin/env bash
## This file was generated by my private Doom Emacs literate config
set -xe
[[ $# -eq 1 && -f "$1" ]] || {
    echo >&2 'Usage: revealjs HTML_FILE'
    exit 1
}
slides_path=$(realpath "$1")
slides_file=$(basename "$1")
cd ~/.local/src/utils/reveal.js
ln -sf "${slides_path}" .
trap "rm '$PWD/${slides_file}'" EXIT
npm start &
sleep 2
xdg-open "http://localhost:8000/${slides_file}"
wait
EOF
chmod +x ~/.local/bin/revealjs

# Install prerequisites for `lang/org` module
sudo dnf -y install ditaa gnuplot pandoc graphviz sqlite
pipx install --include-deps jupyter
pipx inject jupyter ipykernel
sudo dnf -y install zeromq-devel libstdc++-static

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

# Install prerequisites for `lang/rest` module
sudo dnf -y install jq

# Install prerequisites for `lang/rust` module
curl --proto '=https' --tlsv1.2 -fsSLo- https://sh.rustup.rs \
    | sh -s -- --no-modify-path --quiet -y
cargo install cargo-check
rustup component add rustfmt-preview clippy-preview

## rust-analyzer
rustup component add rust-src
rustup +nightly component add rust-analyzer-preview
ln -sf ~/.rustup/toolchains/nightly-*/bin/rust-analyzer ~/.cargo/bin

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
#     curl -fsSLo- "$url" | tar -C "$HOME/.local/src/bashdb" -xzf -
# }

## TODO zshdb

# Install prerequisites for `lang/web` module
sudo dnf -y install tidy
npm install -g js-beautify stylelint
npm install -g vscode-html-languageserver-bin vscode-css-languageserver-bin

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server

# Install prerequisites for `app/Miscellany`
sudo dnf -y install w3m
