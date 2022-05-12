;; todo: refactor
;; https://github.com/nehcuh/.emacs.d-1
(use-package treemacs
  :ensure t
  :bind
  (
   ("s-f" . treemacs)
   ("s-t" . treemacs-select-window)
   ))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map global-map
              ("s-p t" . treemacs-projectile))
  )


(use-package treemacs-magit
  :defer t
  :after (treemacs magit)
  ;; :bind (:map global-map
  ;;             ("s-g t" . treemacs-magit))

  )

(provide 'init-treemacs)
