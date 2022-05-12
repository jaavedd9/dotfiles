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
```
ansible-playbook -i inventory local.yml --ask-become-pass

```
