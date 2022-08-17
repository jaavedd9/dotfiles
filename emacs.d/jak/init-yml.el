
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
