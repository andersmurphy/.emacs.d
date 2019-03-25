defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder

defaults write com.apple.dock autohide -bool true

xcode-select --install

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew update

git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"

git config --global core.editor 'emacsclient'

git config --global pull.rebase true

git config --global color.ui true

echo '.projectile
.DS_Store' > .gitignore

git config --global core.excludesfile '~/.gitignore'

brew cask install emacs

brew install aspell

brew install mpv

brew install multimarkdown

brew install clojure

brew install leiningen

brew install joker
ln -s ~/.emacs.d/dotfiles/.joker ~
