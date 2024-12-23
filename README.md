# .emacs.d

>"This, milord, is my family's emacs. We have owned it for almost nine hundred years, see.
Of course, sometimes it needed a new major-mode. And sometimes it has required a new key binding,
new designs on the modeline, a little refreshing of the theme . . .
but is this not the nine hundred-year-old emacs of my family? And because it has changed gently over time,
it is still a pretty good emacs, y'know. Pretty good."

## OSX setup script

```sh
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

### Download .emacs.d
git clone https://github.com/andersmurphy/.emacs.d.git
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

### Download .emacs.d
git clone https://github.com/andersmurphy/.emacs.d.git

### EMACS (build from source) ###
brew install libxml2 gcc libgccjit tree-sitter jansson pkg-config gnutls textinfo autoconf
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
wait

# Font
brew tap homebrew/cask-fonts
brew install --cask font-fira-code
wait

# github cli (pull requests, approve etc)
brew install gh
wait

### BABASHKA ###
brew install borkdude/brew/babashka
wait

### JAVA ###
brew install openjdk
wait

### Clojure ###
brew install clojure/tools/clojure
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

### Node ###
brew install node
wait

### NBB (node babashka) ###
npm install nbb -g

### solana ###
# homebrew version doesn't come with a test validator
sh -c "$(curl -sSfL https://release.anza.xyz/stable/install)"
echo 'export PATH="$HOME/.local/share/solana/install/active_release/bin:$PATH"' >> ~/.zshrc
wait
```
