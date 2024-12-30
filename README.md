# .emacs.d

>"This, milord, is my family's emacs. We have owned it for almost nine hundred years, see.
Of course, sometimes it needed a new major-mode. And sometimes it has required a new key binding,
new designs on the modeline, a little refreshing of the theme . . .
but is this not the nine hundred-year-old emacs of my family? And because it has changed gently over time,
it is still a pretty good emacs, y'know. Pretty good."

## OSX bootstrap script

```sh
echo 'Installing command line tools...\n'
check=$((xcode-\select --install) 2>&1)
echo $check
str="xcode-select: note: install requested for command line developer tools"
while [[ "$check" == "$str" ]];
do
  sleep 1
done
sudo xcode-select --reset

echo 'Installing brew...\n'
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
echo >> ~/.zprofile
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"
brew analytics off
wait
brew update
wait

echo 'Installing emacs...\n'
brew install git
wait
git clone https://github.com/andersmurphy/.emacs.d.git
wait
bash .emacs.d/setup/scripts/osx-setup.sh
wait
```
