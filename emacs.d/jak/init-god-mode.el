;; to disabl function key translation in god-mode
(setq god-mode-enable-function-key-translation nil)

(use-package god-mode
  :ensure t
  :init
  (god-mode-all)
  :config
  (progn
    (global-set-key (kbd "<escape>") #'god-local-mode)
    (add-to-list 'god-exempt-major-modes 'dired-mode)
    (add-to-list 'god-exempt-major-modes 'magit-mode)
    ;; If God mode is activated through god-mode or god-mode-all, you might want to ensure that no buffers are skipped, as follows:
    ;;(setq god-exempt-major-modes nil)
    ;;(setq god-exempt-predicates nil)
  ))

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;;(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

;; (defun my-god-mode-update-mode-line ()
;;   (cond
;;    (god-local-mode
;;     (set-face-attribute 'mode-line nil
;;                         :foreground "#604000"
;;                         :background "#fff29a")
;;     (set-face-attribute 'mode-line-inactive nil
;;                         :foreground "#3f3000"
;;                         :background "#fff3da"))
;;    (t
;;     (set-face-attribute 'mode-line nil
;; 			            :foreground "#0a0a0a"
;; 			            :background "#d7d7d7")
;;     (set-face-attribute 'mode-line-inactive nil
;; 			            :foreground "#404148"
;; 			            :background "#efefef"))))


(defun my-god-mode-update-mode-line ()
  (cond (god-local-mode
         (progn
           (set-cursor-color "red4")
           (set-face-background 'mode-line "red4")
           ;; (set-face-foreground 'mode-line "gray")
           (set-face-background 'mode-line-inactive "gray30")
           (set-face-foreground 'mode-line-inactive "red")))
        (t
         (progn
           (set-cursor-color "green4")
           (set-face-background 'mode-line-inactive "#4EB8CA  ")
           ;;(set-face-foreground 'mode-line-inactive "red")
           (set-face-background 'mode-line "gray30")
           (set-face-foreground 'mode-line "black")))))

(add-hook 'post-command-hook 'my-god-mode-update-mode-line)


(define-key god-local-mode-map (kbd "r") #'repeat)
;;(define-key god-local-mode-map (kbd "i") #'god-local-mode)
(define-key god-local-mode-map (kbd "j") #'god-local-mode)
;;(define-key god-local-mode-map (kbd ".") #'repeat)

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)


(define-key god-local-mode-map (kbd "[") #'backward-paragraph)
(define-key god-local-mode-map (kbd "]") #'forward-paragraph)


(provide 'init-god-mode)
