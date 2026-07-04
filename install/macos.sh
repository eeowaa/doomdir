#!/bin/sh

cat >&2 <<EOF
WARNING: If installed by a package manager, Emacs might not be pinned at a
specific version (Doom has no knowledge of whether this is the case). System
package updates may upgrade Emacs, resulting in:

  1. Potential package breakages
  2. Potential configuration breakages
  3. The need to run \`doom build'

*** Use caution when performing system updates! ***
EOF

# Install Homebrew to install system packages
curl -fsSLo- https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh | bash

# Install NVM
curl -fsSLo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh \
    | env PROFILE=/dev/null bash

# Install NodeJS and NPM through NVM
nvm install node

# Install the gh-release-dl script to install binary assets from GitHub releases
brew install jq
which gh-release-dl >/dev/null 2>&1 || {
    # TODO: Install to a more "permanent" directory used by Doom Emacs
    test -f "${TMPDIR:-/tmp}/gh-release-dl" || {
        test -d "${TMPDIR:-/tmp}" || mkdir -p "${TMPDIR:-/tmp}"
        curl -fsSLo "${TMPDIR:-/tmp}/gh-release-dl" https://raw.githubusercontent.com/eeowaa/stow-dotfiles/refs/heads/main/utils/.local/bin/gh-release-dl
    }
    chmod +x "${TMPDIR:-/tmp}/gh-release-dl"
    alias gh-release-dl="${TMPDIR:-/tmp}/gh-release-dl"
}

# Install prerequisites for `completion/vertico` module
brew install ripgrep

# Install prerequisites for `ui/doom` module
brew install font-terminus

# TODO: Comic Mono

# Install prerequisites for `emacs/dired` module
brew install coreutils

# Install prerequisites for `term/eshell` module
brew install fish

# Install prerequisites for `term/vterm` module
brew install libvterm cmake

## Soft line wrapping
## <https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359>
brew install screen

# Install prerequisites for `checkers/spell` module
brew install aspell
mkdir -p ~/.local/share/dict
aspell dump master en_US | sort -u > ~/.local/share/dict/words-en_US.txt

# Install prerequisites for `checkers/grammar` module
brew install languagetool
sudo ln -sfn /usr/local/opt/openjdk@11/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-11.jdk

# Install prerequisites for `tools/ansible` module
brew install ansible

# Install prerequisites for `tools/direnv` module
brew install direnv

# Install prerequisites for `tools/docker` module
brew install docker
npm install -g dockerfile-language-server-nodejs

# Install prerequisites for `tools/editorconfig` module
brew install editorconfig

# Install prerequisites for `tools/lookup` module
brew install ripgrep sqlite3

# Install prerequisites for `tools/magit` module
(cd ~/Documents/src/life/stow-dotfiles && make perl)
brew install perl git-absorb
cpan install App::Git::Autofixup

# Install prerequisites for `tools/pdf` module
brew install pkg-config poppler automake

# Install prerequisites for `lang/cc` module
brew install ccls gdb glslang

# Install prerequisites for `lang/csharp` module

# Install prerequisites for `lang/data` module

## xmllint
brew install libxml2

## xmlls
(
    xmlls_baseurl=https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx
    xmlls_version=`curl -fsSLo- "$xmlls_baseurl/maven-metadata.xml" | xmllint --xpath '/metadata/versioning/release/text()' -`
    mkdir -p ~/.config/emacs/.local/etc/lsp/xmlls
    cd ~/.config/emacs/.local/etc/lsp/xmlls
    curl -fsSLO "$xmlls_baseurl/$xmlls_version/org.eclipse.lemminx-$xmlls_version-uber.jar"
    ln -sf "org.eclipse.lemminx-$xmlls_version-uber.jar" org.eclipse.lemminx-uber.jar
)

# Install prerequisites for `lang/json` module
brew install jq
npm install -g vscode-langservers-extracted

# Install prerequisites for `lang/javascript` module
npm install -g typescript typescript-language-server eslint trepan-ni

# Install prerequisites for `lang/markdown` module
npm install -g markdownlint-cli marked

# Install prerequisites for `lang/org` module
brew install ditaa gnuplot pandoc graphviz pngpaste

# Create `org-directory` if missing
mkdir -p ~/Documents/notes

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

# Install prerequisites for `lang/rest` module
brew install jq

# Install prerequisites for `lang/sh` module
brew install bash zsh fish powershell bashdb zshdb shellcheck
npm install -g bash-language-server

# Install prerequisites for `lang/web` module
brew install tidy-html5
npm install -g js-beautify stylelint
npm install -g vscode-html-languageserver-bin vscode-css-languageserver-bin

# Install prerequisites for `lang/yaml` module
npm install -g yaml-language-server
