;;; Commentary
;; ref: https://github.com/daviwil/emacs-from-scratch/blob/8c302a79bf5700f6ef0279a3daeeb4123ae8bd59/Emacs.org#dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :custom ((dired-listing-switches "-aho --group-directories-first"))
  ;; :custom
  ;; ((setq dired-listing-switches "-lXGh --group-directories-first"))
  :init
  (dired-async-mode 1)
  :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;                             "h" 'dired-single-up-directory
  ;;                             "l" 'dired-single-buffer)
  (setq
   delete-by-moving-to-trash 1
   )
  )

(use-package dired-single
  :ensure t
  )

;; (require 'font-lock)
;; (require 'font-lock+)
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; (use-package dired-hide-dotfiles
;;   :ensure t
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   ;; (evil-collection-define-key 'normal 'dired-mode-map
;;   ;;                             "H" 'dired-hide-dotfiles-mode)
;;   )

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1)
  )

(use-package dired-subtree
  :ensure t
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  ( :map dired-mode-map
         ("TAB" . dired-subtree-toggle)
         ("<tab>" . dired-subtree-toggle))
  :config
  ;; Fix "no icons in subtree" issue.
  (defadvice dired-subtree-toggle
      (after add-icons activate) (revert-buffer)))

(use-package dired-sidebar
  :ensure t
  :commands dired-sidebar-toggle-sidebar
  :bind*
  ( :map windows
    ("t" . dired-sidebar-toggle-sidebar))
  :hook
  (dired-sidebar-mode . fk/darken-background)
  :config
  (defun fk/sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (dired-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar))
  )
(global-set-key (kbd "C-x C-,") `dired-sidebar-toggle-sidebar)


(use-package ibuffer-sidebar
  :ensure t
  :commands ibuffer-sidebar-toggle-sidebar
  :bind
  ( :map ibuffer-mode-map
    ("M-o" . nil)))

(use-package dired-show-readme
  :straight (:host gitlab :repo "kisaragi-hiu/dired-show-readme")
  :commands dired-show-readme-mode
  ;; :hook
  ;; (dired-mode . dired-show-readme-mode)
  )

;; (use-package dired-posframe
;;   :ensure t
;;   :straight (:host github :repo "conao3/dired-posframe.el")
;;   :commands dired-posframe-mode)

(use-package dired-recent
  :ensure t
  :after dired  ; TODO: is bind still defer?
  :bind
  ( :map files
    ("d" . dired-recent-open))
  :config
  (dired-recent-mode))

(use-package dired-show-readme
  :straight (:host gitlab :repo "kisaragi-hiu/dired-show-readme")
  :commands dired-show-readme-mode
  :hook
  (dired-mode . dired-show-readme-mode)
  )


(use-package dired-recent
  :after dired  ; TODO: is bind still defer?
  :bind
  ( :map files
    ("d" . dired-recent-open))
  :config
  (dired-recent-mode))


;; to enable unzip action from dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(provide 'init-dired)
