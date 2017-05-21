![dotfiles](dotfiles.png)

Install manually.

- [Docker](https://store.docker.com/search?type=edition&offering=community)
- [Google 日本語入力](https://www.google.co.jp/ime/)
- [Karabiner Elements](https://github.com/tekezo/Karabiner-Elements)
- [LastPass](https://www.lastpass.com)
- [Kinza](https://www.kinza.jp)
- [uBlock Origin](https://github.com/gorhill/uBlock/)
- [Vivaldi](https://vivaldi.com/blog/)

Setup & update.

```sh
curl https://raw.githubusercontent.com/ne-sachirou/dotfiles/master/init.sh | bash
cd ~/dotfiles
ansible-playbook -i hosts playbooks/〜.yml
```
