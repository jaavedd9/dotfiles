;; yaml mode
;; yaml mode


;; (add-to-list 'auto-mode-alist
;;              '("\\.yml\\'" . yaml-mode)
;;              )
(use-package ansible 
  :ensure t 
  :after yaml-mode 
  :custom (ansible-vault-password-file "~/.emacs.d/.ansible-vault") 
  :hook (yaml-mode . ansible) 
  :bind (:map ansible-key-map
              ;; ("C-c v" . rr/set-ansible-vault-mimipass-pwd)
              ("C-c C-d" . ansible-doc)) 
  :config (add-hook 'ansible-hook 'lsp)
  ;;  (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
  ;; (defun rr/write-string (string file)
  ;;   (with-temp-buffer
  ;;     (insert string)
  ;;     (write-region (point-min) (point-max) file)))

  ;; (defun rr/set-ansible-vault-mimipass-pwd ()
  ;;   "Choose which mimipass password to be used for ansible vault."
  ;;   (interactive)
  ;;   (rr/write-string (format "#!/bin/bash\nmimipass get %s"
  ;;                            (rr/helm-mimipass))
  ;;                    ansible-vault-password-file)
  ;;   (chmod ansible-vault-password-file
  ;;          (string-to-number "700" 8)))
  ;; :ensure-system-package
  ;; ((ansible-lint . "sudo apt install ansible-lint"))
  )

(use-package company-ansible 
  :ensure t 
  :hook ansible-mode)

;; (use-package ansible-doc
;;   :ensure t
;;   :hook ansible
;;   ;; :bind
;;   ;; (:map ansible-doc-module-mode-map
;;   ;;       ("C-x C-s" . ignore))
;;   )

(use-package ansible-doc 
  :ensure t 
  :config (add-hook 'yaml-mode-hook #'ansible-doc-mode))


(defun toggle-show-trailing-whitespace () 
  "Toggle `show-trailing-whitespace'." 
  (interactive) 
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


;; sudo apt install yamllint
(defun yamllint-current-buffer () 
  "run a command on the current file and revert the buffer" 
  (interactive) 
  (shell-command (format "yamllint %s" (shell-quote-argument (buffer-file-name)))) 
  (revert-buffer t t t))

(defun ansible-syntax-check-current-buffer () 
  "run a command on the current file and revert the buffer" 
  (interactive) 
  (shell-command (format "ansible-playbook %s --syntax-check" (shell-quote-argument
                                                               (buffer-file-name)))) 
  (revert-buffer t t t))
;;(global-set-key (kbd "C-S-e") 'ansible-lint-current-buffer)


(defun ansible-playbook-check-current-buffer () 
  "run a command on the current buffer to check ansible syntax" 
  (interactive) 
  (shell-command (format "ansible-playbook %s --check" (shell-quote-argument (buffer-file-name)))) 
  (revert-buffer t t t))
;;(global-set-key (kbd "C-S-e") 'ansible-lint-current-buffer)

;; apt install ansible-lint
(defun ansible-lint-current-buffer () 
  "run a command on the current file and revert the buffer" 
  (interactive) 
  (shell-command (format "ansible-lint %s" (shell-quote-argument (buffer-file-name)))) 
  (revert-buffer t t t))
(global-set-key (kbd "C-S-e") 'ansible-lint-current-buffer)


;; ansible-vault
(use-package ansible-vault 
  :ensure t 
  :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe
                  (add-hook 'yaml-mode-hook (lambda () 
                                              (and (string= (file-name-base) "encrypted") 
                                                   (ansible-vault-mode 1))))) 
  :config
  (add-to-list 'auto-mode-alist '("/encrypted$" . yaml-mode)))

  
(provide 'init-ansible)
;;; init-ansible.el ends here
