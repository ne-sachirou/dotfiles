![dotfiles](dotfiles.png)

Install manually.

- [Docker](https://store.docker.com/search?type=edition&offering=community)
- [Firefox Developer Edition](https://www.mozilla.org/ja/firefox/developer/)
- [Google 日本語入力](https://www.google.co.jp/ime/)
- [LastPass](https://www.lastpass.com)
- [LessPass](https://lesspass.com/)
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
