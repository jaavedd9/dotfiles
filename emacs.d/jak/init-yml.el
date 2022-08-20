;;; dependencies of yaml-mode
(use-package outline-magic
  :ensure t)

;;; yaml configs
(use-package yaml-mode 
  :ensure t 
  :mode "\\.vault" 
  :config
  ;;  (add-hook 'yaml-mode-hook (lambda () (auto-fill-mode -1)))
  (progn
  (add-hook 'yaml-mode-hook 'flyspell-mode-off) 
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (setq auto-mode-alist (append '(("\\.yml\\'" . yaml-mode)
        ; note these are encapsulated in a '() list
                                ("\\.yaml\\'" . yaml-mode) 
                                ("\\.yaml.j2\\'" . yaml-mode) 
                                ("\\.yml.j2\\'" . yaml-mode)) auto-mode-alist))
;; references
  ;; https://github.com/yoshiki/yaml-mode/issues/25
;; https://gist.github.com/leoc/f8c0868051003c4ea6eff638bc614575
  (add-hook 'yaml-mode-hook 'leoc/yaml-outline-hook)
      ;; Customize folding markers
  ;; (set-display-table-slot
  ;;      standard-display-table
  ;;      'selective-display
  ;;      (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
  ;;        (vconcat (mapcar (lambda (c) (+ face-offset c)) " [+]"))))

      (defun leoc/yaml-outline-level ()
        (s-count-matches "\\([ ]\\{2\\}\\)" (match-string 0)))

      (defun leoc/yaml-outline-hook ()
        (interactive)
        (setq outline-regexp
              (rx
               (seq
                bol
                (group (zero-or-more "  ")
                       (or (group
                            (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                     (seq "'" (*? (not (in "'" "\n"))) "'")
                                     (*? (not (in ":" "\n"))))
                                 ":"
                                 (?? (seq
                                      (*? " ")
                                      (or (seq "&" (one-or-more nonl))
                                          (seq ">-")
                                          (seq "|"))
                                      eol))))
                           (group (seq
                                   "- "
                                   (+ (not (in ":" "\n")))
                                   ":"
                                   (+ nonl)
                                   eol)))))
               ))
    (use-package outline-magic :ensure t)
    (setq outline-level 'leoc/yaml-outline-level)
    (outline-minor-mode t)
    (hide-body)
    (show-paren-mode 1)
    (define-key yaml-mode-map [tab] 'outline-cycle)
    (define-key outline-minor-mode-map [M-S-tab] 'indent-for-tab-command)
    (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
    (define-key outline-minor-mode-map [M-down] 'outline-move-subtree-down)
    (define-key outline-minor-mode-map [M-up] 'outline-move-subtree-up))))


(use-package jinja2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . yaml-mode)))


(use-package flycheck-yamllint 
  :ensure t 
  :defer t 
  :init (progn (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))


(use-package yaml-imenu
  :ensure t
  )

(defun which-function-from-imenu-index ()
  "Call the imenu-index part in `which-function'.

It is a fallback for when which-func-functions and `add-log-current-defun' return nil."
  (let (which-func-functions)
    (letf (((symbol-function 'add-log-current-defun)
            (lambda () nil)))
      (which-function))))

;; `add-log-current-defun' returns a not so meaningful result in some
;; major modes when the default `add-log-current-defun-function'
;; happens to match a random line that is not really a function
;; definition.  It is often much more desirable to find a function
;; name from an imenu index in those modes.  Results are also used by
;; `which-function-mode'.
(defun enable-add-log-current-defun-using-which-function ()
  (setq-local add-log-current-defun-function 'which-function-from-imenu-index))

(add-hook 'yaml-mode-hook
          'enable-add-log-current-defun-using-which-function)


(provide 'init-yml)
