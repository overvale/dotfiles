# Bootstrap

## Introduction

This document describes, and includes some code for, how I like to set up my Mac. I bought a new laptop in the summer of 2020 and as I set it up I documented pretty much everything I did. I'm doing my best to remember to keep this document up to date.


## Philosophy

1. Use default apps and services first.
1. Don't install Chrome or Electron apps.
1. New software should never slow the system, ideally only make it faster.
1. Don't use apps that install background update deamons, if you can help it.


## Settings

- Sign in with AppleID.
- Turn off automatic installation of system updates.
- Check for software updates, install if you want.
- Enable trackpad tap-to-click.
- Enable three-finger-drag in `System Prefs > Accessibility`.
- Run `defaults write -g NSWindowShouldDragOnGesture yes` in the terminal, then you can drag windows around with `ctrl+cmd` mouse-drag from anywhere in the window.
- Change caps-lock to Control.
- Set computer name in `System Prefs > Sharing`.
- Require password immediately.
- Enable FileVault encryption.
- Turn on firewall.
- Verify `SIP` is enabled with `csrutil status`.
- Disable Spotlight Suggestions in Spotlight preferences.
- Give Terminal full disk access.
- Change finder pref: search current folder.
- Add lock-screen message.
- Disable "Safari Suggestions" and "Show Favorites" in `Safari Prefs > Search`.
- Turn off Safari's "Open 'safe' files after downloading" setting.
- On your iPhone, enable text message forwarded to your new computer.
- Verify you're not sending analytics here: `System Preferences > Security & Privacy > Analytics & Improvements`.
- Add unix welcome message `sudo echo "Welcome to UNIX!" > /etc/motd`.
- Set up ssh keys - use [GitHub instructions](https://help.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh) - don't forget to set a password, and store the password in your keychain.
- Run `defaults write com.apple.dock autohide-time-modifier -float 0.4; killall Dock` in the terminal to speed up revealing the dock when you move your mouse to the bottom of the screen.
- Run `defaults write com.apple.TextEdit NSShowAppCentricOpenPanelInsteadOfUntitledFile -bool false` to make TextEdit create Untitled documents when it launches. [Source](https://mjtsai.com/blog/2020/12/10/making-textedit-create-an-untitled-document-at-launch/)
- Run `defaults write com.apple.TextEdit "TabWidth" '4'`
- Run `defaults -currentHost write -g NSTextKillRingSize -string 6` to expand the kill ring to 6 entries. See [here](https://www.hogbaysoftware.com/posts/mac-text-editing-mark-kill-yank/).


## Encryption

I try to use encryption wherever I can.

- Encrypt the hard drive with FileVault.
- Use encrypted methods for transfers/sync.
- Protect secrets with additional encryption via disk images and GPG encrypted files.

SSH and GPG work like this:

- You create a pair of public and private keys.
	- SSH in `~/.ssh/` and GPG in `~/.gnupg/`. Don't forget to back these up!
- You protect the private key with a password.
- You place the public keys on the servers/services you want connect to.
- A file encrypted with my public key can only be decrypted using my private key.

You can copy your key to a remote server with `ssh-copy-id user@server`.


## Install

While anyone could, in theory, adopt the settings above, the utilities and applications I need are unique to me. So, the below is simply a window into my specific workflow. Lucky you.


### Utilities

- Install command-line dev tools with `xcode-select --install`.
- Install [princeXML](https://www.princexml.com)
- Install [Homebrew](https://brew.sh)
- Install [Pro Video Formats 2.2](https://support.apple.com/downloads/pro%2520video%2520formats) (work-related)
- Install Safari Extensions
	- AdBlock
	- [Hush](https://oblador.github.io/hush/)

Install brew packages (you can run `brew leaves` to find out what you have installed right now):

```
brew install \
     aspell \
     fzf \
     fd  \
     git \
     gpg \
     markdown \
     pandoc \
     tldr \
     tree \
     vim \
     zsh
```

### Applications

Install as many app as you can into `~/Applications`, most work just fine from there, and apps in that directory run with the user's permissions, not admin permissions. Some tools, like Excel, require installation to `/Applications` so they can run background processes, others complain every launch but work just fine.

- Apps from App Store account
- BBEdit
- Emacs (the [mac port](https://github.com/railwaycat/homebrew-emacsmacport), [more info](https://bitbucket.org/mituharu/emacs-mac/src/master/README-mac))
- Firefox
- Hammerspoon
- Monodraw
- Transmit
- Tailscale


## My Stuff

- Copy files from old machine / backup.
- Copy fonts from backup.
- Install [textplay](http://git.io/textplay) with `cd ~/home/src/textplay && make install`.


### Home Dir Layout

Unfortunately, The Mac home folder is kind of a dumping ground. There are a bunch of folders you can't delete, even if you don't use them, a few have very strict security permissions (Documents, Desktop, Downloads), and seemingly every command-line app [wants to install a dotfile there](https://0x46.net/thoughts/2019/02/01/dotfile-madness/) without so much as asking. It very much feels like a place that's *for your computer's use*, not yours. Which is fine, I get why it is that way, but there's something to be said for a true home that you can do anything with. And backup (almost) the entirety of without worrying that you're backing-up a bunch of garbage.

I pretty much organize everything under `~/home/`.

- `~/home/`
	- `dot` - My dotfiles.
	- `src` - Source code, and source code related things.
	- `opt` - Basically a place for software installed via a `git clone`.


### Symlinks

Run from `$HOME`:

```
ln -s ~/home/dot/emacs/init.el ~/.emacs.d/init.el
ln -s ~/home/dot/emacs/early-init.el ~/.emacs.d/early-init.el
ln -s ~/home/dot/git/gitconfig .gitconfig
ln -s ~/home/dot/git/gitignore_global .gitignore_global
ln -s ~/home/dot/zsh/zshrc ~/.zshrc
ln -s ~/home/dot/zsh/zshenv ~/.zshenv
ln -s ~/home/dot/hammerspoon ~/.hammerspoon
ln -s ~/home/dot/vim ~/.vim
```

- Open emacs, zsh, etc. and verify everything is functioning correctly (plugins and whatnot).


### Dictionaries

This installs Webster's 1913 dictionary. Which is awesome.

1. Download the newest release [here](https://github.com/cmod/websters-1913).
2. Move it to `~/Library/Dictionaries`.


## Backups

My setup is basically:

1. Laptop -- for anything I'm working on, or want quick access to.
2. Local NAS (with RAID backup) -- for large media and archives.
3. Cloud -- for versioned backups of laptop data.

I've settled on using an `rsync` backup script to a server that creates regular snapshots. This allows me to keep the backups very simple and rely on the server’s snapshots for versioning.


## Synology Setup

I have a Synology NAS that I use for backups. Mostly I connect through the Finder via SMB, or through Transmit via SFTP, but I occasionally use tools like rsync, etc. that need ssh access.

There are a few pain-in-the-ass details I've picks up along the way.


### Keypair Authentication

I really wish this was officially documented somewhere, so I don't have to rely on blog posts, but the below seems pretty simple. That said, a good set of instructions are [here](https://silica.io/using-ssh-key-authentification-on-a-synology-nas-for-remote-rsync-backups/).

- Enable 'User Homes' in `User > Advanced > User Home`.
- `sudo vim /etc/ssh/sshd_config` and uncomment `PubkeyAuthentication yes`.
- Restart ssh service (turn it off/on).
- Copy your rsa keys to the NAS with `ssh-copy-id user@nas-ip`.
- Change some folder permissions. This is required because the default permissions are wide-open and ssh doesn't allow that (apparently).
	- `chmod 0711 ~`
	- `chmod 0711 ~/.ssh`
	- `chmod 0600 ~/.ssh/authorized_keys`
- You can now push files using a command like: `scp -r ~/home/dot/zsh user@server:/volume1/share/folder`
- You can also use rsync if you enable it on the NAS in `File Services > rsync`.


### SSH vs SFTP Paths

Something else to keep in mind with a Synology is that where you land when you log in via SSH is not the same place you land when you connect via SFTP. Don't assume the paths will be the same.

I learned this on the [restic FAQs](https://restic.readthedocs.io/en/stable/faq.html#creating-new-repo-on-a-synology-nas-via-sftp-fails).
