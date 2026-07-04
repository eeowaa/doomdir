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

# Determine the WSL version (0 means not on WSL)
case `uname -r` in
*-WSL2)      WSL_VERSION=2 ;;
*-Microsoft) WSL_VERSION=1 ;;
*)           WSL_VERSION=0 ;;
esac

# TODO: Obtain Emacs source code corresponding to installed DPKG

# Install Emacs info manuals
sudo apt-get -y install emacs-common-non-dfsg

# Install ripgrep and fd (Doom prerequisites)
sudo apt-get -y install ripgrep fd-find
mkdir -p ~/.local/bin && ln -sf /usr/bin/fdfind ~/.local/bin/fd

if [ $WSL_VERSION -eq 1 ]
then
    # FIXME: Support NodeJS on WSL 1 (requires configuration hacks)
    echo >&2 'ERROR: NodeJS configuration is unimplemented on WSL 1'
    exit 1
else
    # Install NVM
    curl -fsSLo- https://raw.githubusercontent.com/nvm-sh/nvm/HEAD/install.sh \
        | env PROFILE=/dev/null bash

    # Install NodeJS and NPM through NVM
    nvm install node
fi

# Install pipx to install Python packages
sudo apt-get -y install pipx

# Install the gh-release-dl script to install binary assets from GitHub releases
sudo apt-get -y install jq
which gh-release-dl >/dev/null 2>&1 || {
    # TODO: Install to a more "permanent" directory used by Doom Emacs
    test -f "${TMPDIR:-/tmp}/gh-release-dl" || {
        test -d "${TMPDIR:-/tmp}" || mkdir -p "${TMPDIR:-/tmp}"
        curl -fsSLo "${TMPDIR:-/tmp}/gh-release-dl" https://raw.githubusercontent.com/eeowaa/stow-dotfiles/refs/heads/main/utils/.local/bin/gh-release-dl
    }
    chmod +x "${TMPDIR:-/tmp}/gh-release-dl"
    alias gh-release-dl="${TMPDIR:-/tmp}/gh-release-dl"
}

# Install prerequisites for `ui/doom` module
sudo apt-get -y install fonts-terminus

## TODO: Comic Mono

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

# Install prerequisites for `emacs/dired` module
sudo apt-get -y install coreutils

# Install prerequisites for `term/eshell` module
sudo apt-get -y install fish

# Install prerequisites for `term/vterm` module
# REVIEW: Ensure this is everything you need
sudo apt-get -y install libvterm-dev cmake

## Soft line wrapping
## <https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359>
sudo apt-get -y install screen

# Install prerequisites for `checkers/spell` module
sudo apt-get -y install aspell
mkdir -p ~/.local/share/dict
aspell dump master en_US | sort -u > ~/.local/share/dict/words-en_US.txt

# Install prerequisites for `checkers/grammar` module
# Reference: <https://raw.githubusercontent.com/languagetool-org/languagetool/master/install.sh>
sudo apt-get -y install unzip default-jre-headless
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

# Install prerequisites for `tools/lookup` module
sudo apt-get -y install sqlite3 wordnet

# Install prerequisites for `tools/pdf` module
sudo apt-get -y install \
    autoconf \
    automake \
    gcc \
    libpng-dev \
    libpoppler-dev \
    libpoppler-glib-dev \
    libz-dev \
    make \
    pkg-config
