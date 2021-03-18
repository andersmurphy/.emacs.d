defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder

defaults write com.apple.dock autohide -bool true

xcode-select --install

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

brew update

brew analytics off

brew install git

git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"

git config --global credential.helper osxkeychain

git config --global core.editor 'emacsclient'

git config --global pull.rebase true

git config --global color.ui true

rm ~/.gitignore
ln -s ~/.emacs.d/setup/dotfiles/.gitignore ~/.gitignore

git config --global core.excludesfile '~/.gitignore'

git config --global diff.algorithm histogram

brew tap daviderestivo/emacs-head
brew install emacs-head@28 --with-cocoa --with-imagemagick --with-xwidgets

rm /Applications/Emacs.app
ln -s /usr/local/Cellar/emacs-head@28/28.0.50_1/Emacs.app /Applications

brew install aspell

brew install mpv
rm ~/.config/mpv/mpv.conf
ln -s ~/.emacs.d/setup/dotfiles/.mpv/mpv.conf ~/.config/mpv/

brew install multimarkdown

brew tap AdoptOpenJDK/openjdk
brew cask install adoptopenjdk11

brew install --cask chromium
open -a "Chromium" --args --make-default-browser

brew install clojure
rm ~/.clojure/deps.edn
ln -s ~/.emacs.d/setup/dotfiles/.clojure/deps.edn ~/.clojure/

brew install leiningen
rm ~/.lein/profiles.clj
ln -s ~/.emacs.d/setup/dotfiles/.lein/profiles.clj ~/.lein/

brew install --build-from-source ~/.emacs.d/setup/brew-formulae/clj-zprint.rb

brew install clojure-lsp

brew install node

npm install -g yarn

npm i -g typescript-language-server; npm i -g typescript

npm install -g vscode-html-languageserver-bin
