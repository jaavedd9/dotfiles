;; replace modeline with doom-modeline

;; (use-package doom-modeline
;;       :ensure t
;;       :hook (after-init . doom-modeline-mode))

;; replace modeline with smart mode line

;; (use-package smart-mode-line-powerline-theme
;;   :ensure t)
;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   ;; (setq sml/theme 'light-powerline) ;; option are "powerline"(for dark modeline theme) and "light-powerline"
;;   (setq sml/theme 'powerline) ;; option are "powerline"(for dark modeline theme) and "light-powerline"
;;   (add-hook 'after-init-hook 'sml/setup))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t)
  (setq doom-modeline-modal-icon t)
;;  (setq doom-modeline-mu4e t)
  ;; (mu4e-alert-enable-mode-line-display)
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-buffer-encoding t)

  )


;; (use-package doom-modeline
;;   :ensure t
;;   ;; :init
;;   ;; ;; show doom-modeline at the same time with dashboard
;;   ;; (add-hook 'emacs-startup-hook 'doom-modeline-mode -100)
;;   ;; :custom
;;   ;; (doom-modeline-buffer-encoding nil)
;;   ;; (doom-modeline-vcs-max-length 20)
;;   ;; (doom-modeline-bar-width 1)
;;   ;; :custom-face
;;   ;; (mode-line ((t (:background ,fk/dark-color))))
;;   ;; (mode-line-inactive ((t (:background ,fk/dark-color5))))
;;   ;; (mode-line-highlight ((t (:inherit cursor :foreground "black"))))
;;   ;; (doom-modeline-bar ((t (:background ,fk/dark-color))))
;;   ;; (doom-modeline-buffer-path ((t (:inherit font-lock-comment-face :slant normal))))
;;   ;; :hook
;;   ;; (dashboard-after-initialize . column-number-mode)
;;   )



(provide 'init-modeline)
