![dotfiles](dotfiles.png)

Install manually.

- [Docker](https://store.docker.com/search?type=edition&offering=community)
- [Google 日本語入力](https://www.google.co.jp/ime/)
- [ImageOptim](https://imageoptim.com/mac)
- [Karabiner Elements](https://github.com/tekezo/Karabiner-Elements)
- [LastPass](https://www.lastpass.com)
- [LessPass](https://lesspass.com/)
- [Kinza](https://www.kinza.jp)
- [Mono](http://www.mono-project.com/)
- [Spotify](https://www.spotify.com/jp/)
- [Pocket](https://getpocket.com/)
- [uBlock Origin](https://github.com/gorhill/uBlock/)
- [Vivaldi](https://vivaldi.com/blog/)

Setup.

```sh
curl https://raw.githubusercontent.com/ne-sachirou/dotfiles/master/init.sh | bash
cd ~/dotfiles
ansible-playbook -i hosts playbooks/〜.yml
```

Update.

```sh
cd ~/dotfiles
make install
```
