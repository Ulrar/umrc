umrc is a bot to allow using a Mastodon account from IRC.
You can use !toot <text> to publish a new status, and it will display
on the configured channel incomming notifications (follows, mentions ..)

Thanks to @syucream for the Hastodon library, I'm using an (included) slightly
modified version of it here. See https://github.com/syucream/hastodon for the original.

# Compilation

You need to install ghc and cabal-install first, then

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

Save that somewhere as config.ini

# Running

Copy the binary and the config somewhere, and you can just run it with the config as a parameter.
For example, if you copies the binary in /usr/local/bin and the config in /etc :
```sh
/usr/local/bin/umrc /etc/umrc.ini
```

You might want to write an init file to start it automatically at boot.