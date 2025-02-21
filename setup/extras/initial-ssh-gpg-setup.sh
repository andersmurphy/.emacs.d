# passphrase2pgp
cd
go install nullprogram.com/x/passphrase2pgp@latest

# set realname and email
printf "\nGenerating ssh...\n"
echo "Real Name: "
read my_realname
export REALNAME=$my_realname
echo "Email: "
read my_email
export EMAIL=$my_email

# github ssh
rm -rf ~/.ssh
mkdir ~/.ssh
passphrase2pgp -f ssh > ~/.ssh/github
chmod 400 ~/.ssh/github
ssh-keygen -y -f ~/.ssh/github > ~/.ssh/github.pub
ssh-add ~/.ssh/github
cat >> ~/.ssh/config << EOD
Host github.com
  HostName github.com
  User andersmurphy
  IdentitiesOnly yes
  IdentityFile ~/.ssh/github
EOD

# Switch .emacs.d from https to ssh
cd .emacs.d
git remote rm origin
git remote add origin git@github.com:andersmurphy/.emacs.d.git
cd ..

# emacs-sync
printf "\nInstalling emacs-sync...\n"
cd .emacs.d
git clone git@github.com:andersmurphy/emacs-sync.git
cd ..

# gpg
printf "\nGenerating gpg...\n"
passphrase2pgp -s -a -f pgp | gpg --import
printf "\ntrust 5 quit...\n"
gpg --edit-key $REALNAME
