;; for projectile
;;; Code:

;; recommended linux dependencies
;; https://docs.projectile.mx/projectile/configuration.html
;; It’s a great idea to install fd which is much faster than find. If fd is found, projectile will use as a replacement for find.
;; sudo apt install fd-find
;; sudo apt install ripgrep
;; ln -s $(which fdfind) ~/.local/bin/fd

;; (define-key projectile-mode-map (kbd "C-c C-p C-p") 'projectile-switch-project)
;; (define-key projectile-mode-map (kbd "C-c C-p C-f") 'projectile-find-file)
;; (define-key projectile-mode-map (kbd "C-c C-p C-d") 'projectile-find-dir)
;; (define-key projectile-mode-map (kbd "C-c C-p C-i") 'projectile-invalidate-cache)
;; (define-key projectile-mode-map (kbd "C-c C-p C-r") 'projectile-replace)
;; (define-key projectile-mode-map (kbd "C-c C-p C-s C-r") 'projectile-ripgrep)
;; (define-key projectile-mode-map (kbd "C-c C-p C-s C-s") 'projectile-ag)
;; (define-key projectile-mode-map (kbd "C-c C-p C-s M-g") 'projectile-grep)
;; (define-key projectile-mode-map (kbd "C-c C-p C-x C-s") 'projectile-run-shell)

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :bind
    ( :map projectile-mode-map
   ("C-c C-p C-p" . projectile-switch-project)
  ("C-c C-p C-f" . projectile-find-file)
  ("C-c C-p C-d" . projectile-find-dir)
  ("C-c C-p C-i" . projectile-invalidate-cache)
  ("C-c C-p C-r" . projectile-replace)
  ("C-c C-p C-s C-r" . projectile-ripgrep)
  ("C-c C-p C-s C-s" . projectile-ag)
  ("C-c C-p C-s C-g" . projectile-grep)
  ("C-c C-p C-x C-s" . projectile-run-shell))
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '(
                                         ;; "~/code/kfupm_projects/configs"
                                         ;; "~/code/kfupm_projects/apps"
                                         ;; "~/code/personal_projects"
                                         ;; "~/code/learning_projects"
                                         "~/Code/personal/apps"
                                         "~/Code/personal/configs"
                                         "~/Code/work/apps"
                                         "~/Code/work/configs"
                                         "~/Code/work/cmss"
                                         "~/Code/work/docs"
                                         ))
  ;; https://docs.projectile.mx/projectile/configuration.html
  ;; (setq projectile-indexing-method 'hybrid)
  (setq projectile-enable-caching t)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  ;;(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  ;; (setq projectile-keymap-prefix (kbd "C-c p"))
  ;;(define-key projectile-mode-map (kbd "M-p y") 'projectile-previous-project-buffer)
  ;;(define-key projectile-mode-map (kbd "M-p n") 'projectile-next-project-buffer)
  ;; projectile with helm
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  ;;(setq projectile-switch-project-action 'projectile-dired)
  ;;(setq projectile-switch-project-action #'projectile-dired)
  ;;(setq projectile-switch-project-action 'treemacs-add-and-display-current-project
  (helm-projectile-on)
  ;;(setq projectile-sort-order 'modification-time)
  (setq projectile-sort-order 'recently-active)
  ;;(setq projectile-sort-order 'access-time)
  ;;(setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'alien)
  )


(use-package treemacs-projectile
  :ensure t)


(use-package helm-projectile
  :custom
  (helm-projectile-sources-list '(helm-source-projectile-buffers-list
                                  helm-source-projectile-recentf-list
                                  helm-source-projectile-files-list
                                  helm-source-projectile-projects))
  ;; :bind
  ;; ("C-x f" . helm-projectile)
  ;; :hook
  ;; (projectile-mode . helm-projectile-on)
  :config
  (defun fk/projectile-recentf-files-first-five (original-function)
    "Return a list of five recently visited files in a project."
    (let ((files (funcall original-function)))
      (if (> (length files) 5)
          (seq-subseq files 0 5)
        files)))
  (advice-add 'projectile-recentf-files :around 'fk/projectile-recentf-files-first-five))


;; intergrating tab-bar with projectile
(use-package tab-bar
  :bind (:map tab-prefix-map ("p" . my/new-project-tab))
  :init
  (defun my/new-project-tab ()
    (interactive)
    (other-tab-prefix)
    (projectile-switch-project)
    (tab-rename (projectile-project-name))))

(provide 'init-projectile)
;;; init-projectile.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
