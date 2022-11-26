defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder
defaults write -g ApplePressAndHoldEnabled -bool true
defaults write com.apple.dock autohide -bool true

xcode-select --install
wait

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
wait

brew update
wait

brew analytics off
brew tap homebrew/cask

brew install --cask chrysalis
wait

brew install git
wait

git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"

git config --global credential.helper osxkeychain

git config --global core.editor 'emacsclient'

git config --global pull.rebase true

git config --global fetch.prune true

git config --global color.ui true

rm ~/.gitignore
ln -s ~/.emacs.d/setup/dotfiles/.gitignore ~/.gitignore

git config --global core.excludesfile '~/.gitignore'

git config --global diff.algorithm histogram

brew install libxml2 gcc libgccjit cairo
wait

# We clone a single branch for a slightly smaller download
git clone https://git.savannah.gnu.org/git/emacs.git --branch master --single-branch
wait

cd emacs
git checkout master
./autogen.sh
./configure --with-cairo --with-imagemagick --with-xwidgets --with-native-compilation

make -j$(nproc)
wait

make clean install
wait

mv nextStep/Emacs.app /Applications
cd

brew install aspell
wait

brew install mpv
wait
rm ~/.config/mpv/mpv.conf
ln -s ~/.emacs.d/setup/dotfiles/.mpv/mpv.conf ~/.config/mpv/

brew install multimarkdown
wait

brew install pandoc
wait

brew install borkdude/brew/jet
wait

brew tap AdoptOpenJDK/openjdk
brew install adoptopenjdk17
wait

brew install clojure
wait
rm ~/.clojure/deps.edn
ln -s ~/.emacs.d/setup/dotfiles/.clojure/deps.edn ~/.clojure/

brew install leiningen
wait
rm ~/.lein/profiles.clj
ln -s ~/.emacs.d/setup/dotfiles/.lein/profiles.clj ~/.lein/

brew install clojure-lsp/brew/clojure-lsp-native

rm -r ~/.clj-kondo
mkdir ~/.clj-kondo
ln -s ~/.emacs.d/setup/dotfiles/.clj-kondo/config.edn ~/.clj-kondo/

brew install node
wait

npm install -g yarn
wait

npm i -g typescript-language-server; npm i -g typescript

npm install -g vscode-html-languageserver-bin
wait

brew install --cask dropbox
wait

brew install --cask calibre
wait
