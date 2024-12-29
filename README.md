# .emacs.d

>"This, milord, is my family's emacs. We have owned it for almost nine hundred years, see.
Of course, sometimes it needed a new major-mode. And sometimes it has required a new key binding,
new designs on the modeline, a little refreshing of the theme . . .
but is this not the nine hundred-year-old emacs of my family? And because it has changed gently over time,
it is still a pretty good emacs, y'know. Pretty good."

## OSX bootstrap script

```sh
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

### GIT ###
brew install git
wait

### Download .emacs.d
git clone https://github.com/andersmurphy/.emacs.d.git
wait

### Handoff to seput script
bash .emacs.d/setup/scripts/osx-setup.sh
wait
```
