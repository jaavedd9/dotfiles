;; next keys suggestions
(use-package doom-themes
  :ensure t
  :custom-face
  (font-lock-comment-face ((t (:slant italic))))
  (font-lock-string-face ((t (:foreground "PeachPuff3"))))
  (font-lock-function-name-face ((t (:foreground "LightGoldenrod"))))
  (highlight ((t (:underline t :background nil :foreground nil))))
  (lazy-highlight ((t (:background nil :foreground nil :box (:line-width -1)))))
  (fixed-pitch ((t (:family "Noto Sans Mono"))))
  :config
  ;; (load-theme 'doom-spacegrey t)
  (defconst fk/font-color (face-attribute 'default :foreground))
  (defconst fk/background-color (face-attribute 'default :background))
  (defconst fk/dark-color (doom-darken fk/background-color 0.15))
  (defconst fk/dark-color1 (doom-darken fk/background-color 0.01))
  (defconst fk/dark-color2 (doom-darken fk/background-color 0.02))
  (defconst fk/dark-color3 (doom-darken fk/background-color 0.03))
  (defconst fk/dark-color4 (doom-darken fk/background-color 0.04))
  (defconst fk/dark-color5 (doom-darken fk/background-color 0.05))
  (defconst fk/dark-color6 (doom-darken fk/background-color 0.06))
  (defconst fk/dark-color7 (doom-darken fk/background-color 0.07))
  (defconst fk/dark-color8 (doom-darken fk/background-color 0.08))
  (defconst fk/dark-color9 (doom-darken fk/background-color 0.09))
  (defconst fk/light-color (doom-lighten fk/background-color 0.15))
  (defconst fk/light-color1 (doom-lighten fk/background-color 0.09))
  (defconst fk/light-color2 (doom-lighten fk/background-color 0.08))
  (defconst fk/light-color3 (doom-lighten fk/background-color 0.07))
  (defconst fk/light-color4 (doom-lighten fk/background-color 0.06))
  (defconst fk/light-color5 (doom-lighten fk/background-color 0.05))
  (defconst fk/light-color6 (doom-lighten fk/background-color 0.04))
  (defconst fk/light-color7 (doom-lighten fk/background-color 0.03))
  (defconst fk/light-color8 (doom-lighten fk/background-color 0.02))
  (defconst fk/light-color9 (doom-lighten fk/background-color 0.01)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; load a theme
;;(load-theme 'sanityinc-tomorrow-eighties t)
;;(load-theme 'zenburn t)
;;(load-theme 'doom-moonlight t)
;; kaoline theme
(use-package kaolin-themes
  :ensure t
  :config
  ;; (load-theme 'kaolin-temple t)
  )

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;; (load-theme 'gruvbox-dark-medium t)
;;   )

;; (use-package leuven-theme
;;   :ensure t
;;   )

;; (load-theme 'moe-light t)
;; (load-theme 'leuven t)
;; disable tool and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)

;; line and column number
;; (global-hl-line-mode +1)
;; (line-number-mode +1)
;; (global-display-line-numbers-mode 1)
;; (column-number-mode t)
;; (size-indication-mode t)


;; filename in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq inhibit-startup-message t)    ;; Hide the startup message

;; dashboards
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   )

(use-package dashboard
  :ensure t
  :custom
  ;; Source for logo: https://github.com/tecosaur/emacs-config/blob/master/config.org#splash-screen
  ;; (dashboard-startup-banner (fk/expand-static-file-name "logos/emacs-e-small.png"))
  ;; Do not show package count, it is meaningless because of lazy loading.
  (dashboard-init-info (format "Emacs started in %s" (fk/time-since-startup)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  ;;(dashboard-week-agenda t)
  (dashboard-agenda-time-string-format "%d/%m/%Y %A %H:%M")
  ;; (dashboard-agenda-release-buffers t) ; Has bugs
  (dashboard-item-shortcuts '((recents . "r")
                              (bookmarks . "b")
                              (projects . "p")
                              (agenda . "a")))
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     ;;(bookmarks . 5)
                     ;;(agenda . 10) ;; I load agenda in :hook section
                     ))
  (dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (dashboard-navigator-buttons
   `((;; Github
      (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Github"
       "Browse github"
       (lambda (&rest _) (browse-url "https://github.com/")))
      ;; Codebase
      (,(all-the-icons-faicon "briefcase" :height 1.1 :v-adjust -0.1)
       "Codebase"
       "My assigned tickets"
       (lambda (&rest _) (browse-url "https://hipo.codebasehq.com/tickets")))
      ;; Perspective
      (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
       "Reload last session"
       "Reload last session"
       (lambda (&rest _) (persp-state-load persp-state-default-file))))))
  :custom-face
  (dashboard-heading-face ((t (:weight bold))))
  (dashboard-items-face ((t (:weight normal))))
  :hook
  (dashboard-mode . (lambda () (setq-local cursor-type nil)))
  (dashboard-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  ;; Load agenda after showing dashboard to decrease waiting time to see the
  ;; initial screen (dashboard). TODO: try to make this asynchronously to not
  ;; block Emacs.
  (dashboard-after-initialize . (lambda ()
                                  ;;(add-to-list 'dashboard-items '(agenda . 10) t)
                                  (dashboard-refresh-buffer)))
  :config
  (dashboard-setup-startup-hook)

  ;; Run the hooks even if dashboard initialization is skipped
  (when (> (length command-line-args) 1)
    (add-hook 'emacs-startup-hook (lambda () (run-hooks 'dashboard-after-initialize-hook))))

  (defun fk/home ()
    "Switch to home (dashboard) buffer."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (switch-to-buffer dashboard-buffer-name)
      (dashboard-refresh-buffer)))

  (defun fk/dashboard-get-agenda ()
    "Copy org-agenda (week) buffer"
    (save-window-excursion
      (org-agenda-list)
      (read-only-mode -1)
      (delete-matching-lines "...... now - - -")
      (delete-matching-lines "----------------")
      (let ((agenda (buffer-substring (point-min) (point-max))))
        (kill-buffer)
        agenda)))

  (defun fk/dashboard-insert-agenda (list-size)
    "Insert directly org-agenda buffer."
    (insert (fk/dashboard-get-agenda)))

  ;;(setcdr (assoc 'agenda dashboard-item-generators) 'fk/dashboard-insert-agenda)
  )

(setq dashboard-items '((recents  . 6)
                        (bookmarks . 7)
                        (projects . 10)
                        (registers . 5)))

;; beacon mode for cursor
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  )


;; dimmer for buffer highlight
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t)
  (setq dimmer-fraction 0.2)
  )

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))

;; (global-set-key (kbd "C-c t") 'toggle-transparency)


;; highlight line
(set-face-attribute 'hl-line nil :inherit nil :background "#3e4446")
(global-hl-line-mode 1)
;;https://emacs.stackexchange.com/a/47845/26147
;; (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)
;;(set-face-attribute 'highlight nil :background "gray6" :foreground 'unspecified)
;;(set-face-attribute 'highlight nil :background "#ffc000" :foreground 'unspecified)

;; make emacs faster
;;(setq display-line-numbers-type nil)

;; fonts
;; (use-package fira-code-mode
;;   :ensure t
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" ":")) ;; List of ligatures to turn off
;;   :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;;
(use-package alert
  :ensure t
  :commands alert
  :config
  (setq alert-default-style 'notifications))


(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")
  :commands good-scroll-mode
  :custom
  (good-scroll-duration 0.2)
  (good-scroll-point-jump 4)
  ;; :bind
  ;; ("C-v" . fk/smooth-scroll-up)
  ;; ("M-v" . fk/smooth-scroll-down)
  ;; ("C-l" . fk/smooth-recenter-top-bottom)
  ;; :hook
  ;; (dashboard-after-initialize . good-scroll-mode)
  :config
  (defun fk/smooth-scroll-down (&optional pixels)
    "Smooth alternative of M-v `scroll-down-command'."
    (interactive)
    (let ((good-scroll-step (or pixels 300)))
      (good-scroll-down)))

  (defun fk/smooth-scroll-up (&optional pixels)
    "Smooth alternative of C-v `scroll-up-command'."
    (interactive)
    (let ((good-scroll-step (or pixels 300)))
      (good-scroll-up)))

  (defun fk/smooth-recenter-top-bottom ()
    "docstring"
    (interactive)
    (let* ((current-row (cdr (nth 6 (posn-at-point))))
           (target-row (save-window-excursion
                         (recenter-top-bottom)
                         (cdr (nth 6 (posn-at-point)))))
           (distance-in-pixels (* (- target-row current-row) (line-pixel-height)))
           (good-scroll-step distance-in-pixels))
      (when (not (zerop distance-in-pixels))
        (good-scroll--update -1)))))


;; font
(defconst fk/default-font-family "Roboto Mono")
;; fk/default-font-size is calculated on start according to the primary screen
;; size. if screen-size is bigger than 16 inch: 9 else 15.
(defconst fk/default-font-size
  (let* ((command "xrandr | awk '/primary/{print sqrt( ($(NF-2)/10)^2 + ($NF/10)^2 )/2.54}'")
         (screen-size (string-to-number (shell-command-to-string command))))
    (if (or (> screen-size 16) (= screen-size 0)) 132 150)) ;; 132 the default fontsize size/10, I think
  )  ; screen-size=0 if command gives error
(defconst fk/default-icon-size 18)

(defconst fk/variable-pitch-font-family "Noto Serif")
(defconst fk/variable-pitch-font-size fk/default-font-size)  ; TODO: adjust this and use in org-mode

;; (custom-set-faces
;;  `(default ((t (:family ,fk/default-font-family :height ,fk/default-font-size))))
;;  `(variable-pitch ((t (:family ,fk/variable-pitch-font-family :height ,fk/variable-pitch-font-size))))
;;  ;; Characters with fixed pitch face do not shown when height is 90.
;;  `(fixed-pitch-serif ((t (:height 100)))))


(defun fk/adjust-font-size (height)
  "Adjust font size by given height. If height is '0', reset font
size. This function also handles icons and modeline font sizes."
  (interactive "nHeight ('0' to reset): ")
  (let ((new-height (if (zerop height)
                        fk/default-font-size
                      (+ height (face-attribute 'default :height)))))
    (set-face-attribute 'default nil :height new-height)
    (set-face-attribute 'mode-line nil :height new-height)
    (set-face-attribute 'mode-line-inactive nil :height new-height)
    (message "Font size: %s" new-height))
  (let ((new-size (if (zerop height)
                      fk/default-icon-size
                    (+ (/ height 5) treemacs--icon-size))))
    (when (fboundp 'treemacs-resize-icons)
      (treemacs-resize-icons new-size))
    (when (fboundp 'company-box-icons-resize)
      (company-box-icons-resize new-size)))
  (when diff-hl-mode
    (diff-hl-maybe-redefine-bitmaps)))

(defun fk/increase-font-size ()
  "Increase font size by 0.5 (5 in height)."
  (interactive)
  (fk/adjust-font-size 5))

(defun fk/decrease-font-size ()
  "Decrease font size by 0.5 (5 in height)."
  (interactive)
  (fk/adjust-font-size -5))

(defun fk/reset-font-size ()
  "Reset font size according to the `fk/default-font-size'."
  (interactive)
  (fk/adjust-font-size 0))


;; Page Break Lines
(use-package page-break-lines
  :ensure t
  :custom-face
  (page-break-lines ((t (:inherit font-lock-comment-face
                                  :foreground ,fk/light-color1
                                  :width expanded))))
  :hook
  (dashboard-after-initialize . global-page-break-lines-mode)
  :config
  (add-to-list 'page-break-lines-modes 'c-mode))


(use-package hl-todo
  :ensure t
  :custom
  ;; Better hl-todo colors, taken from spacemacs
  (hl-todo-keyword-faces '(("TODO" . "#dc752f")
                           ("NEXT" . "#dc752f")
                           ("THEM" . "#2d9574")
                           ("PROG" . "#4f97d7")
                           ("OKAY" . "#4f97d7")
                           ("DONT" . "#f2241f")
                           ("FAIL" . "#f2241f")
                           ("DONE" . "#86dc2f")
                           ("NOTE" . "#b1951d")
                           ("KLUDGE" . "#b1951d")
                           ("HACK" . "#b1951d")
                           ("TEMP" . "#b1951d")
                           ("QUESTION" . "#b1951d")
                           ("HOLD" . "#dc752f")
                           ("FIXME" . "#dc752f")
                           ("XXX+" . "#dc752f")))
  :hook
  (dashboard-after-initialize . global-hl-todo-mode))

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-responsive 'top)
;;   (highlight-indent-guides-auto-enabled nil)
;;   :custom-face
;;   ;; (highlight-indent-guides-character-face ((t (:foreground ,fk/light-color6))))
;;   ;; (highlight-indent-guides-top-character-face ((t (:foreground ,fk/light-color))))
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode))

;; TODO: Add a function to set window width to fill column width
;; according to current major mode
(use-package zoom
  :ensure t
  :commands zoom-mode
  :preface
  (defvar fk/zoom-default-size '(120 . 40))
  :custom
  (zoom-size fk/zoom-default-size)
  :bind*
  (("C-M-*" . fk/enlarge-window)
   ("C-M--" . fk/shrink-window)
   ("C-M-0" . balance-windows))
  :config
  ;; TODO: handle when zoom-mode active
  (defun fk/adjust-window-width (percentage)
    (let* ((new-width (round (* (window-width) percentage)))
           (zoom-size (cons new-width (cdr zoom-size))))
      (if (> percentage 1.0)  ; TODO: fk/smooth-zoom do not shrink
          (fk/smooth-zoom)
        (zoom))))

  (defun fk/enlarge-window ()
    (interactive)
    (fk/adjust-window-width 1.1))

  (defun fk/shrink-window ()
    (interactive)
    (fk/adjust-window-width 0.9))

  (defvar fk/smooth-zoom-steps 10)
  (defvar fk/smooth-zoom-period 0.01)

  (defun fk/floor (number)
    "Floor by absolute value."
    (if (< number 0)
        (ceiling number)
      (floor number)))

  (defun fk/smooth-zoom ()
    "Smooth (animated) version of `zoom'."
    (interactive)
    (cancel-function-timers 'fk/smooth-zoom--resize)
    (setq fk/smooth-zoom-sizes '())
    (setq fk/smooth-zoom-window (get-buffer-window))
    (let* ((current-size (cons (window-width) (window-height)))
           (desired-size zoom-size)
           (distances (cons (- (car desired-size) (car current-size))
                            (- (cdr desired-size) (cdr current-size))))
           (step-distance (cons (fk/floor (/ (car distances) (float fk/smooth-zoom-steps)))
                                (fk/floor (/ (cdr distances) (float fk/smooth-zoom-steps))))))
      (dotimes (i fk/smooth-zoom-steps)
        (let* ((zoom-size (if (< i (1- fk/smooth-zoom-steps))
                              (cons (+ (car step-distance) (car current-size))
                                    (+ (cdr step-distance) (cdr current-size)))
                            desired-size))
               (time (concat (number-to-string (round (* i fk/smooth-zoom-period 1000))) " millisec")))
          (setq current-size zoom-size)
          (add-to-list 'fk/smooth-zoom-sizes current-size t)
          (run-at-time time nil 'fk/smooth-zoom--resize)))))

  (defun fk/smooth-zoom--resize ()
    (with-selected-window fk/smooth-zoom-window
      (let ((zoom-size (pop fk/smooth-zoom-sizes)))
        (zoom--resize)))))


;; (use-package stripe-buffer
;;   :ensure t
;;   :custom-face
;;   (stripe-highlight ((t (:background ,fk/light-color7))))
;;   :config
;;   ;; hl-line (higher priority stripes) fix:
;;   (defadvice sb/redraw-region (after stripe-set-priority activate)
;;     (when (or stripe-buffer-mode stripe-table-mode)
;;       (dolist (overlay sb/overlays)
;;         (overlay-put overlay 'priority -100))))
;;   :hook
;;   (org-mode . turn-on-stripe-table-mode))

(use-package emojify
  :ensure t
  :commands emojify-mode)

(use-package emacs-everywhere
  :ensure t
  :commands emacs-everywhere
  :bind
  ( :map emacs-everywhere-mode-map
         ("C-x C-c" . emacs-everywhere-abort))
  :hook
  ;;(emacs-everywhere-mode . (lambda () (require 'turkish) (turkish-mode)))
  ;; Banish mouse to prevent focusing to company's childframe
  (emacs-everywhere-mode . (lambda () (set-mouse-position (selected-frame) 0 0)))
  :config
  ;; I disabled insert selection because it inserts from clipboard if there is
  ;; no selection.
  (advice-add 'emacs-everywhere-insert-selection :override 'ignore))

;; TODO: find mp3 file does not work with straight
(use-package fireplace
  :ensure t
  :straight (:files ("*"))  ; Fix fireplace.mp3 not found issue
  :commands fireplace
  :custom
  (fireplace-sound-on t))


(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :hook
  (visual-fill-column-mode . visual-line-mode))

;; (use-package which-key-posframe
;;   :ensure t
;;   :custom
;;   (which-key-idle-secondary-delay 0)
;;   ;;(which-key-side-window-max-height 0.99)
;;   :custom-face
;;   (which-key-posframe ((t (:background ,fk/dark-color))))
;;   (which-key-posframe-border ((t (:background "gray"))))
;;   :hook
;;   (dashboard-after-initialize . which-key-posframe-mode)
;;   (dashboard-after-initialize . which-key-mode)
;;   :config
;;   (which-key-posframe-mode)
;;   )

;; (use-package linum-relative
;;   :ensure
;;   :config
;;   (setq display-line-numbers-type "relative")
;;   (linum-relative-global-mode)
;;   (setq linum-relative-current-symbol "")
;;   )

;; (display-line-numbers-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
;;(setq display-line-numbers 'relative)
;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook
;;                 shell-mode-hook
;;                 eshell-mode-hook))
;;   (add-hook mode (lambda () (linum-relative-toggle))))

;; (use-package vimish-fold
;;   :ensure
;;   :after evil)

;; (use-package vimish-fold
;;   :ensure t
;;   :after evil)

;; (use-package evil-vimish-fold
;;   :ensure t
;;   :after vimish-fold
;;   :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;;
;;

;; (use-package highlight-indentation
;;   :ensure t
;;   :config
;;   (progn
;;   (setq highlight-indentation-mode 1)
;;   (setq highlight-indentation-current-column-mode 1)
;;   (add-hook 'prog-mode-hook 'highlight-indentation-mode)
;;   (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
;;   ))

(use-package hydra
  :ensure t
  )

;; fonts
(set-face-attribute 'default nil
                  :font "DejaVu Sans Mono")
;; (set-default-font "DejaVu Sans Mono-13")
(set-frame-font "DejaVu Sans Mono-13")
(set-fontset-font "fontset-default" '(#x0000 . #xFFFFF) '("DejaVu Sans Mono" . "unicode-bmp"))
;; for deamon mode
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))


(load-theme 'sanityinc-tomorrow-day t)
;; 
;; (set-face-attribute 'hl-line nil :inherit nil :background "#90ee90")
;; change the current line higlight color
;; https://emacs.stackexchange.com/a/41918/26147
(set-face-attribute 'hl-line nil :inherit nil :background "#e0ffff")


;; how to check current face
;; https://emacs.stackexchange.com/questions/40926/how-can-i-change-the-parentheses-highlight-style
;; show-paren-mode highlights the current brackets
;; change show-paren-mode mode face, hightlighted bracket

(set-face-attribute 'show-paren-match nil :inherit nil :background "#ff6347")
;;https://superuser.com/a/1181817/713461
;; setting the face directly was not working for some reason - my lack of understanding -  as suggested in the stackoverflow
;; adding it in the find file hook worked
(add-hook 'find-file-hook (lambda ()
                            (set-face-attribute 'show-paren-match nil :inherit nil :background "#ff6347")))

(provide 'init-ui)
;;; init-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
