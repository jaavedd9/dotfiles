
;; (defun disable-key-binding-in-magitmode ()
;;   ;; (define-key reftex-mode-map "\C-c/" nil)
;;   (define-key magit-section-cycle (kbd "C-<tab>") nil)
;;   )

(use-package magit
  :ensure t
  :commands magit
  :custom
  (magit-section-initial-visibility-alist '((stashes . show)
                                            (unpushed . show)
                                            (pullreqs . show)
                                            (issues . show)))
  :bind*
  ( :map version-control
    ("v" . magit-status)
    ("s" . magit-status)
    :map magit-mode-map
    ("o" . (lambda () (interactive)
             (call-interactively 'magit-diff-visit-file-other-window)
             (recenter-top-bottom)))
    ("C-c C-f" . magit-find-file))
  :hook
  ;;(magit-mode . magit-toggle-margin) FIXME: does not work
  ;;(magit-mode . magit-toggle-margin-details)
  (git-commit-setup . git-commit-turn-on-flyspell)
  ;; (magit-mode . disable-key-binding-in-magitmode)
  :config
  (setq magit-save-repository-buffers 'dontask)
  )

;; magit
(global-set-key (kbd "s-g") 'magit-status)
;; (global-set-key (kbd "C-c C-v") 'magit-status)
;; (with-eval-after-load 'magit-log
;;   ;;(define-key magit-log-mode-map (kbd "<C-return>") nil))
;;   (define-key magit-section-cycle (kbd "C-<tab>") nil))

;; (define-key magit-section-cycle (kbd "C-<tab>") nil)

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-global-modes '(not org-mode))
  (diff-hl-ask-before-revert-hunk nil)
  :custom-face
  (diff-hl-insert ((t (:background "#224022"))))
  (diff-hl-change ((t (:background "#492949" :foreground "mediumpurple1"))))
  (diff-hl-delete ((t (:background "#492929" :foreground "orangered2"))))
  :bind
  ( :map version-control
    ("n" . diff-hl-next-hunk)
    ("p" . diff-hl-previous-hunk)
    ("r" . diff-hl-revert-hunk))
  :hook
  (progn
    (dashboard-after-initialize . global-diff-hl-mode)
    (diff-hl-mode . diff-hl-flydiff-mode)
    (magit-pre-refresh . diff-hl-magit-pre-refresh)
    (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)


(provide 'init-vc)
