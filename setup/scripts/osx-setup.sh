defaults write com.apple.finder AppleShowAllFiles -boolean true ; killall Finder
defaults write -g ApplePressAndHoldEnabled -bool true
defaults write com.apple.dock autohide -bool true

xcode-select --install
wait
sudo xcode-select --reset

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
wait

brew update
wait

brew analytics off
brew tap homebrew/cask

brew install git
wait

git config --global user.name "andersmurphy"
git config --global user.email "andersmurphy@gmail.com"

git config --global diff.gpg.textconv "gpg --no-tty --decrypt"

git config --global core.editor 'nano'

git config --global pull.rebase true

git config --global fetch.prune true

git config --global color.ui true

rm ~/.gitignore
ln -s ~/.emacs.d/setup/dotfiles/.gitignore ~/.gitignore

git config --global core.excludesfile '~/.gitignore'

git config --global diff.algorithm histogram

brew install emacs --cask
wait

brew install aspell
wait

brew install mpv
wait
rm ~/.config/mpv/mpv.conf
ln -s ~/.emacs.d/setup/dotfiles/.mpv/mpv.conf ~/.config/mpv/

brew install borkdude/brew/jet
wait

curl https://raw.githubusercontent.com/Homebrew/homebrew-core/59edfe598541186430d49cc34f42671e849e2fc9/Formula/gnupg.rb > gnupg.rb
wait
brew install gnupg.rb
wait
rm gnupg.rb

brew install ripgrep

brew tap AdoptOpenJDK/openjdk
brew install adoptopenjdk17
wait

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

brew install borkdude/brew/babashka

brew install fennel
