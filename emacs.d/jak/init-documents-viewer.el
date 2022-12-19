;;

;; install apt dependencies from here https://github.com/politza/pdf-tools
;; or
;; M-x install pdf tool

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                           :height 1.0))


(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;;(add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;;  (setq nov-text-width 90)
  (setq nov-text-width 60)
  ;;(setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  )


(use-package org-noter
  :after org
  :ensure t
  :config (
           ;; setq org-noter-default-notes-file-names '("notes.org")
                ;; org-noter-notes-search-path '("/mnt/data/Dropbox/emacs/org_files/doc-notes")
                ;;org-noter-separate-notes-from-heading t
                ))

;; (use-package org-noter-pdftools
;;   :ensure t
;;   )

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  ;; :bind
  ;; ( :map pdf-view-mode-map
  ;;   ("O" . pdf-occur)
  ;;   ("d" . pdf-view-midnight-minor-mode)
  ;;   ("s a" . pdf-view-auto-slice-minor-mode)
  ;;   ("t" . (lambda (beg end) (interactive "r") (go-translate))))
  ;; :hook
  ;; (pdf-view-mode . pdf-links-minor-mode)
  ;; (pdf-view-mode . pdf-isearch-minor-mode)
  ;; (pdf-view-mode . pdf-outline-minor-mode)
  ;; (pdf-view-mode . pdf-history-minor-mode)
  (with-eval-after-load 'pdf-links
    (define-key pdf-links-minor-mode-map (kbd "f") 'pdf-links-action-perform))
  :config
  (pdf-tools-install)
  )


;; (use-package pdf-continuous-scroll-mode
;;   :straight (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
;;   ;; M-x pdf-view-fit-width-to-window and disable olivetti before run this
;;   :commands pdf-continuous-scroll-mode)





(provide 'init-documents-viewer)
