# passphrase2pgp
go install nullprogram.com/x/passphrase2pgp@latest
wait

# set realname and email
echo "Real Name: "
read my_realname
export REALNAME=$my_realname
echo "Email: "
read my_email
export EMAIL=$my_email

# github ssh
passphrase2pgp -f ssh > ~/.ssh/github
wait
passphrase2pgp -f ssh -p > ~/.ssh/github.pub
wait
chmod 400 ~/.ssh/github
ssh-add ~/.ssh/github
cat >> ~/.ssh/config << EOD
Host github.com
  HostName github.com
  User andersmurphy
  IdentitiesOnly yeso
  IdentityFile ~/.ssh/github
EOD

# Switch .emacs.d from https to ssh
cd .emacs.d
git remote rm
git remote add origin git@github.com:andersmurphy/.emacs.d.git
cd ..

# emacs-sync
cd .emacs.d
git clone git@github.com:andersmurphy/emacs-sync.git
wait
cd ..

# gpg
passphrase2pgp -s -a -f pgp | gpg --import
gpg --edit-key $REALNAME
