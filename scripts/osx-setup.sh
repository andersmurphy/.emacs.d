defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder

defaults write com.apple.dock autohide -bool true

xcode-select --install

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew update

brew install git

git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"

git config --global credential.helper osxkeychain

git config --global core.editor 'emacsclient'

git config --global pull.rebase true

git config --global color.ui true

echo '.DS_Store' > .gitignore

git config --global core.excludesfile '~/.gitignore'

brew cask install emacs

brew install aspell

brew install mpv

brew install multimarkdown

brew install clojure
rm ~/.clojure/deps.edn
ln -s ~/.emacs.d/dotfiles/.clojure/deps.edn ~/.clojure/

brew install leiningen
rm ~/.lein/profiles.clj
ln -s ~/.emacs.d/dotfiles/.lein/profiles.clj ~/.lein/

brew install joker
rm ~/.joker
ln -s ~/.emacs.d/dotfiles/.joker ~
