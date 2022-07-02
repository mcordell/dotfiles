# Other things to do:

Setup gpg. Get the public keys from USB or 1password.

```shell
gpg --import <PATH TO PUBLIC KEY>
```

command will output the import key id, store the KEYID

```
KEYID=<KEY ID>
gpg --edit-key $KEYID
gpg> trust
5, y
gpg> quit
```
setup git:

```
sed -e "s/KEYID/$KEYID/" .computer_gitconfig.template > ~/.computer_gitconfig
```

Setup ssh

```
ssh-add -L | grep "cardno:" > ~/.ssh/id_rsa_yubikey.pub
```
