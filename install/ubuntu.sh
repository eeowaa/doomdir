# Install prerequisites for `checkers/grammar` module
# Reference: <https://raw.githubusercontent.com/languagetool-org/languagetool/master/install.sh>
sudo apt -y install unzip default-jre-headless
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
sudo apt -y install ripgrep sqlite3 wordnet

# Install prerequisites for `tools/pdf` module
sudo apt -y install \
    autoconf \
    automake \
    gcc \
    libpng-dev \
    libpoppler-dev \
    libpoppler-glib-dev \
    libz-dev \
    make \
    pkg-config
