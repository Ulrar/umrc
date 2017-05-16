umrc is a bot to allow using a Mastodon account from IRC.
It will display on the configured channel incomming notifications (follows, mentions ..)

Available commands are :

```
|toot <text>
|replytoot <id> <text>
|delete <id>
|boost <id>
|favorite <id>
|unfavorite <id>
|follow <text>
|unfollow <text>
```

You can also set the toot visibility like that, if you want :
```
|toot -private <text>
|replytoot -direct <text>
```

Thanks to @syucream for the Hastodon library, I'm using an (included) slightly
modified version of it here. See https://github.com/syucream/hastodon for the original.

# Compilation


Unfortunatly you'll need cabal >= 1.22, which means you'll need backports on debian jessie.
See https://backports.debian.org/Instructions/ for instructions on enabling backports.
Then you'll need to install packages :
```bash
apt-get -t jessie-backports install cabal-install ghc
```

And finally, compile umrc :
```bash
$ cabal update
$ cabal install --only-dependencies
$ cabal build
```

That should give you the final binary in dist/build/umrc/umrc.

# Configuration

You need to register the app on mastodon and get a client id, secret and a token.
You can use https://tinysubversions.com/notes/mastodon-bot/ for that, and use those ids
in the config file :

```
[DEFAULT]
clientId = ID
clientSecret = Another ID
token = The token
domain = mastodon.tld

server = chat.freenode.net
nick = umrc
chan = #umrc
admins = Ulrar, Someone else
```

See the included config.ini for an (inactive) example.
Save that somewhere as config.ini

# Running

Copy the binary and the config somewhere, and you can just run it with the config as a parameter.
For example, if you copied the binary in /usr/local/bin and the config in /etc :
```sh
/usr/local/bin/umrc /etc/umrc.ini
```

You might want to write an init file to start it automatically at boot.
I included a SysV init script in the init_scripts directory, you just need to copy the init.d
content to your /etc/init.d and the default content to your /etc/default/ and adjust the values
in there.
