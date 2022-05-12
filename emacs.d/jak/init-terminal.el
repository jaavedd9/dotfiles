;;
;; installations required on linux
;; https://github.com/akermu/emacs-libvterm#2004
;; https://github.com/akermu/emacs-libvterm#install-cmake-and-libtool
;; for me worked with emacs 27
;; it is mentioned to remove emacs 26 completly
;; apt-get install libtool-bin
(use-package
  vterm
  :ensure t
  :commands vterm
  :config (add-hook 'vterm-mode-hook (lambda ()
                                       (ctrlf-local-mode -1)))
  (defun vterm-send-C-c()
    (interactive)
    (vterm-send-key "c" nil nil t))
  (defun vterm-send-M-left()
    (interactive)
    (vterm-send-key "<left>" nil t))
  (defun vterm-send-M-right()
    (interactive)
    (vterm-send-key "<right>" nil t))
  (defun vterm-send-M-up()
    (interactive)
    (vterm-send-key "<up>" nil t))
  (defun vterm-send-M-down()
    (interactive)
    (vterm-send-key "<down>" nil t))
  (defun vterm-send-C-left()
    (interactive)
    (vterm-send-key "<left>" nil nil t))
  (defun vterm-send-C-right()
    (interactive)
    (vterm-send-key "<right>" nil nil t))
  (defun vterm-send-C-up()
    (interactive)
    (vterm-send-key "<up>" nil nil t))
  (defun vterm-send-C-down()
    (interactive)
    (vterm-send-key "<down>" nil nil t))
  (defun vterm-send-C-M-e()
    (interactive)
    (vterm-send-key "e" nil t t))
  (defun vterm-send-C-x-C-e()
    (interactive)
    (vterm-send-key "x" nil nil t)
    (vterm-send-key "e" nil nil t))
  (defun vterm-copy-mode-and-page-up()
    (interactive)
    (vterm-copy-mode)
    (scroll-down-command))
  :bind (:map vterm-mode-map
              ("M-<left>" . vterm-send-M-left)
              ("M-<right>" . vterm-send-M-right)
              ("M-<up>" . vterm-send-M-up)
              ("M-<down>" . vterm-send-M-down)
              ("C-<left>" . vterm-send-C-left)
              ("C-<right>" . vterm-send-C-right)
              ("C-<up>" . vterm-send-C-up)
              ("C-<down>" . vterm-send-C-down)
              ("C-M-e"  . vterm-send-C-M-e)
              ("C-x C-e" . vterm-send-C-x-C-e)
              ("M-<prior>" . vterm-copy-mode-and-page-up))
  ("C-<backspace>" . vterm-send-meta-backspace)
  (:map vterm-copy-mode-map
        ("C-c C-c" . vterm-copy-mode))
  :custom (vterm-max-scrollback 10000))

(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm))))
  (shell-pop-term-shell "/usr/bin/zsh")
  ;;(shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-window-size 20)
  (shell-pop-restore-window-configuration t)
  :bind*
  ;; (("C-SPC z" . shell-pop))
  )

(provide 'init-terminal)
