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
echo "ssh name: "
read ssh_name

# github ssh
passphrase2pgp -f ssh > ~/.ssh/$ssh_name
chmod 400 ~/.ssh/$ssh_name
ssh-keygen -y -f ~/.ssh/$ssh_name > ~/.ssh/$ssh_name.pub
ssh-add ~/.ssh/$ssh_name 
