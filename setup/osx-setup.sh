### COMMAND LINE TOOLS ###
printf "\n Installing command line tools...\n"
xcode-select --install &> /dev/null
until $(xcode-select --print-path &> /dev/null); do
  sleep 5;
done
sudo xcode-select --reset

### OSX DEFAULTS ###
printf "\n Setting OSX defaults...\n"
defaults write com.apple.finder AppleShowAllFiles -boolean true
killall Finder
defaults write -g ApplePressAndHoldEnabled -bool true
# Minimal dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock orientation left
defaults delete com.apple.dock persistent-apps
defaults delete com.apple.dock persistent-others
defaults delete com.apple.dock recent-apps
killall Dock

### BREW ###
printf "\n Installing brew...\n"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
echo >> ~/.zprofile
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"
brew analytics off
brew update

### GIT ###
brew install git
# Dot files
git clone https://github.com/andersmurphy/.emacs.d.git
# User
git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"
git config --global core.editor 'nano'
git config --global pull.rebase true
git config --global fetch.prune true
git config --global color.ui true
# Better dif alogorithm
git config --global diff.algorithm histogram
# Transparent encrypted diffs
git config --global diff.gpg.textconv "gpg --no-tty --decrypt"
# Credentials - we want to handle these ourselves
git config --local --unset credential.helper
git config --global --unset credential.helper
git config --system --unset credential.helper
# Set up gitignore
rm ~/.gitignore
ln -s ~/.emacs.d/setup/dotfiles/.gitignore ~/.gitignore
git config --global core.excludesfile '~/.gitignore'

### EMACS (build from source) ###
printf "\n Installing emacs...\n"
brew install libxml2 gcc libgccjit tree-sitter jansson pkg-config gnutls texinfo autoconf
wait
# We clone just the tag for a smaller download
git clone --depth 1 --branch emacs-29.3 https://git.savannah.gnu.org/git/emacs.git
wait
cd emacs
git checkout emacs-29.3
./autogen.sh
./configure --with-cairo --with-native-compilation
make -j$(nproc)
wait
make clean install
wait
mv nextStep/Emacs.app /Applications
cd

# spelling
brew install aspell

# Conversion between json and edn.
brew install borkdude/brew/jet

# Encryption
brew install gnupg

# Fast grep
brew install ripgrep

# Font
brew install --cask font-fira-code

# github cli (pull requests, approve etc)
brew install gh

### Babashka ###
brew install borkdude/brew/babashka

### Java ###
brew install openjdk
echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc

### Clojure ###
brew install clojure/tools/clojure
rm ~/.clojure/deps.edn
rm ~/.clojure/user.clj
ln -s ~/.emacs.d/setup/dotfiles/.clojure/deps.edn ~/.clojure/
ln -s ~/.emacs.d/setup/dotfiles/.clojure/user.clj ~/.clojure/

brew install leiningen
rm ~/.lein/profiles.clj
ln -s ~/.emacs.d/setup/dotfiles/.lein/profiles.clj ~/.lein/

brew install clojure-lsp/brew/clojure-lsp-native
rm -r ~/.clj-kondo
mkdir ~/.clj-kondo
ln -s ~/.emacs.d/setup/dotfiles/.clj-kondo/config.edn ~/.clj-kondo/

### Go ###
brew install go
echo 'export PATH="$PATH:$(go env GOPATH)/bin"' >> ~/.zshrc

### Proxy ###
brew install caddy
