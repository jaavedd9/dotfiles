# Description
Dotfiles with ansible playbooks for the debian(11) OS.

managed packages and configs inclucde
1. emacs
2. zsh
3. python # development packages
4. compton # compositor
5. nodejs
6. snap
7. tmux
8. kubectl
9. apt-packages
10. docker
11. xmonad
12. xmobar
13. rofi
14. copyq


# Installation
1. install debian(11)
2. install ansible
```
pip3 install ansible
```
3. clone the repo of dotfiles
```
git clone git@bitbucket.org:jaavedd9/dotfiles.git
```
4. run playbook

from the `deploy` directory

```
ansible-playbook -i inventory local.yml --ask-become-pass

```
