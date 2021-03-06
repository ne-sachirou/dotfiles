![dotfiles](dotfiles.png)

Install manually.

- [1Password](https://1password.com/jp/) for Firefox
- [Docker](https://store.docker.com/search?type=edition&offering=community)
- [Firefox Developer Edition](https://www.mozilla.org/ja/firefox/developer/)
- [Google 日本語入力](https://www.google.co.jp/ime/)
- [HTTPS Everywhere | Electronic Frontier Foundation](https://www.eff.org/https-everywhere)
- [LastPass](https://www.lastpass.com) for Vivaldi
- [LessPass](https://lesspass.com/)
- [Privacy Badger | Electronic Frontier Foundation](https://www.eff.org/privacybadger)
- [Vivaldi](https://vivaldi.com/blog/)

Setup.

```sh
curl https://raw.githubusercontent.com/ne-sachirou/dotfiles/master/init/macos.sh | bash
curl https://raw.githubusercontent.com/ne-sachirou/dotfiles/master/init/ubuntu.sh | bash
cd ~/dotfiles
ansible-playbook -v -i hosts 〜.yml
```

Update.

```sh
cd ~/dotfiles
make install
```
