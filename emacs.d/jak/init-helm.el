
(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :requires helm-config
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-full-frame nil;; to select candidates in the same window
        helm-split-window-default-side 'same
        helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
        helm-recentf-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        )
  (global-set-key (kbd "C-x C-i") 'helm-resume)
  ;;(global-set-key (kbd "C-c C-i") 'helm-multi-swoop)
  ;;(global-set-key (kbd "C-x C-i") 'helm-multi-swoop-all)
  (global-set-key (kbd "C-c C-b") 'helm-bookmarks)
  ;; (require 'helm-config)
  (helm-mode 1)
  ;;  (setq helm-split-window-inside-p t
  ;;  helm-move-to-line-cycle-in-source t)
  ;;  (setq helm-autoresize-max-height 0)
  ;;  (setq helm-autoresize-min-height 100)
  ;;  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-c C-a")  'helm-select-action) ; list actions using C-z
  (define-key helm-map (kbd "C-c C-m")  'helm-select-action) ; list actions using C-z
  ;; to make helm navigation compatible with evil
  ;;https://emacs.stackexchange.com/a/18872/26147
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-c C-k") nil)

  ;; (define-key helm-find-files-map (kbd "<right>") 'helm-ff-run-open-file-externally) ; open files externally; works for helm-find-files

  ;; (setq helm-external-programs-associations
  ;; '(("pdf" ."evince")
  ;;   ("html" . "chrome")
  ;;   ("mp3" . "vlc")
  ;;   ("mp4" . "vlc")
  ;;   ("jpeg" . "feh")
  ;;   ("png" . "feh"))
  ;; )

  (setq
      helm-M-x-fuzzy-match                  t
      helm-bookmark-show-location           t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-mode-fuzzy-match                 t
      helm-locate-fuzzy-match               t 
      helm-quick-update                     t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t)
  )


;; (use-package helm-ag
  ;; :ensure t
  ;; )

;; (use-package helm-xref
;;   :ensure t
;;   )

;;  Flx-based fuzzy intelligent sorting for helm
;; (use-package helm-flx
;;   :ensure t
;;   :config
;;   (helm-flx-mode +1)
;;   (setq
;;       helm-flx-for-helm-find-files t ;; t by default
;;       helm-flx-for-helm-locate t) ;; nil by default
;;   )

;; (use-package helm-fuzzier
;;   :ensure t 
;;   :config
;;   (helm-fuzzier-mode 1)
;;   )
  
;; (use-package fussy
;;   :ensure t
;;   :config
;;   (push 'fussy completion-styles)
;;   (setq
;;    ;; For example, project-find-file uses 'project-files which uses
;;    ;; substring completion by default. Set to nil to make sure it's using
;;    ;; flx.
;;    completion-category-defaults nil
;;    completion-category-overrides nil))

(setq helm-completion-style 'emacs)


(setq helm-ag-insert-at-point 'symbol)
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; ;; to disable at point search
;; (setq helm-swoop-pre-input-function (lambda () ""))
;; ;; C-w if at point search is need

;; C-s in a buffer: open helm-swoop with empty search field
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (with-eval-after-load 'helm-swoop
;;   (setq helm-swoop-pre-input-function
;;         (lambda () nil)))

;; ;; C-s in helm-swoop with empty search field: activate previous search.
;; ;; C-s in helm-swoop with non-empty search field: go to next match.
;; (with-eval-after-load 'helm-swoop
;;   (define-key helm-swoop-map (kbd "C-s") 'tl/helm-swoop-C-s))

;; (defun tl/helm-swoop-C-s ()
;;   (interactive)
;;   (if (boundp 'helm-swoop-pattern)
;;       (if (equal helm-swoop-pattern "")
;;           (previous-history-element 1)
;;         (helm-next-line))
;;     (helm-next-line)
;;     ))


;; (use-package helm-descbinds
;;   :init
;;   (helm-descbinds-mode 1)
;;   :bind
;;   (("C-c h h" . helm-descbinds))
;;   :config
;;   (setq helm-descbinds-window-style 'split-window))


(use-package helm-swoop
  :ensure t
  :config
  (setq helm-swoop-use-fuzzy-match t
        helm-swoop-use-line-number-face t
        ;; to get color in seach candidates at the expense of speed
        helm-swoop-speed-or-color t
        ;; Go to the opposite side of line from the end or beginning of line
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t
        ;; helm-multi-swoop-edit-save t
        )
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  )



(global-unset-key (kbd "C-x c b"))



(helm-adaptive-mode 1)


;; ref: https://www.reddit.com/r/emacs/comments/kl0grk/question_helm_search_with_combination_of_first/
;; the important part is "flex"
;; (setq completion-styles '(basic partial-completion emacs22 flex))

;; ;; use the helm-flx package for better fuzzy sorting
;; (use-package helm-flx
;;   :ensure t
;;   :after helm
;;   :init
;;   (setq helm-flx-for-helm-find-files nil)
;;   :config
;;   (helm-flx-mode +1)
;;   (setq helm-flx-for-helm-find-files t ;; t by default
;;         helm-flx-for-helm-locate t) ;; nil by default
;;   )

;; (defun spacemacs//helm-make-source (f &rest args)
;;   "Function to be used as advice to activate fuzzy matching for all sources."
;;   (let ((source-type (cadr args))
;;         (props (cddr args)))
;;     ;; fuzzy matching is not supported in async sources
;;     (unless (child-of-class-p source-type helm-source-async)
;;       (plist-put props :fuzzy-match t)))
;;   (apply f args))

;; ;; spacemacs adds this advice to all sync helm sources.  I don't really understand its purpose, but it does make a difference.  Without this, fuzzy matching works differently.
;; (advice-add 'helm-make-source :around #'spacemacs//helm-make-source)

;; (use-package helm-tramp
;;   :ensure t
;;   :config
;;   (setq tramp-default-method "ssh")
;;   (define-key global-map (kbd "C-c t") 'helm-tramp)
;;   )

(require 'helm-rg)

(use-package helm-rg
  :custom
  (helm-rg--extra-args '("--max-columns" "400"))
  (fk/helm-rg-fuzzy-match t)  ; I may wanna disable helm-rg's transform functionality
  :custom-face
  (helm-rg-file-match-face ((t (:inherit font-lock-type-face :weight bold :underline nil))))
  (helm-rg-line-number-match-face ((t (:inherit line-number))))
  :bind
  ("C-M-s" . fk/helm-rg-dwim)
  :config
  (defun fk/helm-rg-dwim (&optional query)
    "Smarter version of helm-rg.
- Search in project if in a project else search in default (current) directory.
- Start search with selected text if region is active or empty string.
- Escape special characters when searching with selected text."
    (interactive)
    (let ((helm-rg-default-directory (or (projectile-project-root) default-directory))
          (query (or query (fk/convert-string-to-rg-compatible (or (fk/get-selected-text) "")))))
      (cl-letf (((symbol-function 'helm-rg--get-thing-at-pt) (lambda () query)))
        (if fk/helm-rg-fuzzy-match
            (call-interactively 'helm-rg)
          (cl-letf (((symbol-function 'helm-rg--helm-pattern-to-ripgrep-regexp) (lambda (_) _)))
            (call-interactively 'helm-rg)))
        )))

  ;; Use a simpler header in the helm buffer.
  (fset 'helm-rg--header-name (lambda (_) (concat "Search at " helm-rg--current-dir)))

  (defun fk/helm-rg-dwim-with-glob (glob &optional query)
    (interactive)
    (let ((helm-rg-default-glob-string glob))
      (fk/helm-rg-dwim query))))


(use-package helm-icons
   :straight (:host github :repo "yyoncho/helm-icons")
   :after helm
   :config
   (treemacs-resize-icons fk/default-icon-size)
   (helm-icons-enable))


(setq helm-grep-ag-command (concat "rg"
                                   ;;" --color=never"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s %s %s")
      helm-grep-file-path-style 'relative
      )

(defun mu-helm-rg (directory &optional with-types)
  "Search in DIRECTORY with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (helm-grep-ag-1 (expand-file-name directory)
                  (helm-aif (and with-types
                                 (helm-grep-ag-get-types))
                      (helm-comp-read
                       "RG type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm rg types*"))))


(defun mu-helm-project-search (&optional with-types)
  "Search in current project with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg (mu--project-root) with-types))

(defun mu-helm-file-search (&optional with-types)
  "Search in `default-directory' with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg default-directory with-types))


;; (use-package helm-ls-git
;;   :ensure t)

(defun mu--project-root ()
  "Return the project root directory or `helm-current-directory'."
  (require 'helm-ls-git)
  (require 'helm-ls-hg)
  (if-let (dir (helm-ls-git-root-dir))
      dir
    (helm-current-directory)))

;; (use-package helm-posframe
;;   :straight (:host github :repo "KaratasFurkan/helm-posframe")
;;   :after helm
;;   :custom
;;   (helm-display-header-line nil)
;;   (helm-echo-input-in-header-line t)
;;   (helm-posframe-border-color "gray")
;;   (helm-posframe-parameters '((left-fringe . 5)
;;                               (right-fringe . 5)))
;;   :config
;;   (helm-posframe-enable)
;;   ;; Remove annoying error message that displayed everytime after closing
;;   ;; helm-posframe. The message is:
;;   ;; Error during redisplay: (run-hook-with-args helm--delete-frame-function
;;   ;; #<frame 0x5586330a1f90>) signaled (user-error "No recursive edit is in
;;   ;; progress")
;;   (remove-hook 'delete-frame-functions 'helm--delete-frame-function))

;; (use-package helm-c-yasnippet
;;   :ensure t
;;   )


;; (use-package 
;;   helm-shell-history 
;;   :ensure t 
;;   :config (add-hook 'eshell-mode-hook (lambda () 
;;                                         (eshell-cmpl-initialize) 
;;                                         (define-key eshell-mode-map [remap eshell-pcomplete]
;;                                                     'helm-esh-pcomplete) 
;;                                         (define-key eshell-mode-map (kbd "M-p")
;;                                                     'helm-eshell-history))))


;; (use-package helm-systemd
;;   :ensure t)

(provide 'init-helm)
