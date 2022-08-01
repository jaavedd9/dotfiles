# Description
Dotfiles with ansible playbooks for the Debian(11) OS.

managed packages and configs inclucde
1. emacs # extensible editor
2. zsh # shell language
3. python # development packages
4. java # openjdk
4. compton # compositor for transparency 
5. nodejs
6. snap
7. tmux # terminal session manager
8. kubectl # kubernetes
9. apt-packages
10. docker # containers
11. xmonad # tiling windows manager
12. xmobar # task bar for xmonad
13. rofi # application launcher and window switcher
14. copyq # paste manager with suport for storing encrypted passwords
15. rescuetime # to track time on OS
16. wakatime # to track time in Editor(emacs) 
17. hugo # static site generator
18. vim 

# emacs

# Installation
1. install Debian(11)
2. install ansible with pip
```
pip3 install ansible
```
3. clone this repository in "~/Code/personal/configs" directory 
```
git clone git@bitbucket.org:jaavedd9/dotfiles.git
```
4. Install ansible roles, from 'deploy' directory
```
ansible-galaxy install -r requirements.yml
```

5. make sure "$HOME/.emacs.d" directory is removed, because this playbook will create a sysmlink from this repo's "emacs.d" directory to "$HOME/.emacs.d". 

Reason for creating symlink instead of copying files to "$HOME/.emacs.d" directory is to avoid
running ansible playbook after every change in emacs config - which I do frequently.

6. Change the ansible variables
   1. `workstation_user`
   2. `workstation_home`
   3. `tmux_plugin_manager_path`
   4. `workstation_user_personal_private_key_path`
   5. `workstation_user_work_private_key_path`
   and other variables which involves `workstation_user`

6. run playbook

from the `deploy` directory

```
ansible-playbook -i inventory local.yml --ask-become-pass

```
