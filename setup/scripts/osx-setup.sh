### OSX settings ###
defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder
defaults write -g ApplePressAndHoldEnabled -bool true
defaults write com.apple.dock autohide -bool true

### XCODE ###
# We reset the path to prevent:
# 
# xcrun: error: active developer path ("/Applications/Xcode.app/Contents/Developer") does not exist
# 
# Which seems to happen if you are installing a new version of xcode.
xcode-select --install
wait
sudo xcode-select --reset

# To make homebrew work properly you need to fix ruby script permissions.
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
brew analytics off
wait
brew update
wait
brew tap homebrew/cask

### GIT ###
brew install git
wait

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
# Specify username on all repositories
git config --global \
    url.https://andersmurphy@github.com.insteadOf \
    https://github.com
# Set up gitignore
rm ~/.gitignore
ln -s ~/.emacs.d/setup/dotfiles/.gitignore ~/.gitignore
git config --global core.excludesfile '~/.gitignore'

### EMACS ###
brew install emacs --cask
wait

# spelling
brew install aspell
wait

# Music
brew install mpv
wait
rm ~/.config/mpv/mpv.conf
ln -s ~/.emacs.d/setup/dotfiles/.mpv/mpv.conf ~/.config/mpv/

# Conversion between json and edn.
brew install borkdude/brew/jet
wait

# Encryption
brew install gnupg
wait

# Fast grep
brew install ripgrep

### JAVA ###
brew tap AdoptOpenJDK/openjdk
brew install adoptopenjdk21
wait

### Clojure ###
brew install clojure
wait
rm ~/.clojure/deps.edn
rm ~/.clojure/user.cljc
ln -s ~/.emacs.d/setup/dotfiles/.clojure/deps.edn ~/.clojure/
ln -s ~/.emacs.d/setup/dotfiles/.clojure/user.cljc ~/.clojure/

brew install leiningen
wait
rm ~/.lein/profiles.clj
ln -s ~/.emacs.d/setup/dotfiles/.lein/profiles.clj ~/.lein/

brew install clojure-lsp/brew/clojure-lsp-native

rm -r ~/.clj-kondo
mkdir ~/.clj-kondo
ln -s ~/.emacs.d/setup/dotfiles/.clj-kondo/config.edn ~/.clj-kondo/

### BABASHKA ###
brew install borkdude/brew/babashka

### FENNEL ###
brew install fennel
