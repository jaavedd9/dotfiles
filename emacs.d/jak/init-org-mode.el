;; org mode
(use-package org
  :pin gnu
  :config
  (define-key org-mode-map (kbd "C-c C-j") nil)
  (define-key org-mode-map (kbd "C-c C-k") nil)
  (setq org-id-link-to-org-use-id t) ;; to make org-store-link use id instead of file for org-roam backlinks to work
  )

(require 'org)


;; to update time stamps in org files before save
;; ref https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/10

(setq time-stamp-active t
      time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
;; (add-hook 'before-save-hook 'time-stamp nil)

(add-hook 'before-save-hook #'jak/before-saving-org-file)


  ;;--------------------------
  ;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
  ;;--------------------------

  (defun zp/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun zp/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (zp/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun zp/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (zp/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))


(defun jak/before-saving-org-file()
  (jak/replace-org-roam-links)
  (zp/org-set-last-modified)
  )

(defun jak/replace-org-roam-links ()
  (org-roam-link-replace-all)
  )

  (defun zp/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (zp/org-set-time-file-property "LAST_MODIFIED")))

;; end of updating modification time


;; to set sounds for org-timer
(setq org-clock-sound (file-truename(concat dropbox-path  "emacs/sounds/service-bell-ring.")))

;; from https://explog.in/notes/writingsetup.html
;; layout
(add-hook
 'text-mode-hook
 'auto-fill-mode)

;; from https://emacs.stackexchange.com/a/27577
(add-hook 'org-mode-hook '(lambda ()
                            (visual-line-mode)
                            (org-indent-mode)
                            (variable-pitch-mode)
                            (flyspell-mode)
                            (setq doom-modeline-enable-word-count t)
                            (display-line-numbers-mode 0)
                            ))

;; n characters per line
;;(setq-default fill-column 120)
(setq-default fill-column 100)

(setq org-indent-indentation-per-level 1)
(setq org-adapt-indentation nil)
(setq org-hide-leading-stars 't)

;; to preserve indentation in src blocks
;; https://emacs.stackexchange.com/a/9483
(setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)
;; Turn on indentation and auto-fill mode for Org files
;; (defun dw/org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode 1)
;;   (auto-fill-mode 0)
;;   (visual-line-mode 1)
;;   (setq evil-auto-indent nil)
;;   (diminish org-indent-mode))

(use-package org
  :defer t
  ;; :hook
  ;; (org-mode . dw/org-mode-setup)
  ;; daviwil
  ;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org#org-mode
  :config
  (setq
   org-ellipsis " ▾"
   org-hide-emphasis-markers t
   org-src-fontify-natively t
   org-fontify-quote-and-verse-blocks t
   org-src-tab-acts-natively t
   org-edit-src-content-indentation 2
   org-hide-block-startup nil
   org-src-preserve-indentation nil
   org-startup-folded 'content
   org-cycle-separator-lines 2)
  ;; (use-package org-superstar
  ;;   :ensure t
  ;;   ;; :if (not dw/is-termux)                
  ;;   :after org
  ;;   :hook (org-mode . org-superstar-mode)
  ;;   :custom
  ;;   (org-superstar-remove-leading-stars t)
  ;;   ;; (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  ;;   )
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Increase the size of various headings
  ;; from
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  ;; (let* ((variable-tuple
  ;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.0))))
  ;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
  ;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2 :foreground "#26A65B"))))
  ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.00)
                  (org-level-6 . 1.00)
                  (org-level-7 . 1.00)
                  (org-level-8 . 1.00)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
  (set-face-attribute 'org-level-1 nil :foreground "#006442" :weight 'bold)
  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  ;; TODO: Others to consider
  ;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; end daviwil
  ;;layout ends
  )


;; disable company completions in org mode
(defun jpk/org-mode-hook ()
  ;;(company-mode -1)
  )
(add-hook 'org-mode-hook 'jpk/org-mode-hook)


;; fix image size

(setq org-image-actual-width 700)

(use-package htmlize
  :ensure t
  )

(use-package ox-twbs
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown")
  :config
  (define-key markdown-mode-map (kbd "C-c C-j") nil)
  (define-key markdown-mode-map (kbd "C-c C-k") nil))

;; enable logging
(setq org-log-into-drawer t)
;; TODO states
;; ! record time stamp
;; @ create a log in logbook drawer
;; ref: https://www.gnu.org/software/emacs/manual/html_node/org/Tracking-TODO-state-changes.html
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "INPROGRESS(i!)" "WAITING(w@/!)" "|" "DONE(d!)"))
      )

;; literate programming
(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-default-exec-mode 'jar)  (setq org-confirm-babel-evaluate nil)
  (setq plantuml-jar-path  (concat dropbox-path "emacs/plantuml/plantuml.jar"))
  (setq org-plantuml-jar-path (expand-file-name (concat dropbox-path "emacs/plantuml/plantuml.jar")))
  (setq org-startup-with-inline-images t)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

(setq org-ditaa-jar-path  (concat dropbox-path "emacs/ditaa/ditaa.jar"))


(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (plantuml . t)
   (ditaa . t)
   (shell . t)
   (calc . t)
   ;; (literate-calc . t)
   ))

(global-set-key (kbd "<f5>") 'org-capture)
(global-set-key (kbd "<f6>") 'org-agenda)


;; tags
(setq org-tag-alist '(
                      ("family" . ?f)
                      ("someday" . ?s)
                      ))

;; storage
;;(setq org-default-notes-file "/mnt/data/Dropbox/emacs/org_files/org_mode/inbox.org")
(setq org-default-notes-file (concat dropbox-path "emacs/org_files/org_mode/inbox.org"))
(setq org-agenda-files (list (concat dropbox-path "emacs/org_files/org_mode/agenda/")))
(setq org-directory (concat dropbox-path "emacs/org_files"))

;; custom org templates
(setq org-capture-templates
      '(
        ;; tasks that take < 1 hour
        ("k" "Mini Todo" entry (file+headline  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/mini_tasks.org")) "Tasks")
         "* TODO mini %?\n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))  %i\n  %a\n")
        ;; take some action based on this note, either on the same day or soon
        ("f" "Fleeing Note" entry (file+headline  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/fleeting.org")) "Tasks")
         "* %(org-insert-time-stamp (org-read-date nil t \"+0d\")) %?\n  %i\n  %a\n\n")
        ;; personal tasks
        ("p" "Personal Todo" entry (file+headline (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/personal_tasks.org")) "Tasks")
         "* TODO %?  %i\n %a\n")
        ;; work tasks
        ("w" "Work Todo" entry (file+headline  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/work_tasks.org")) "Tasks")
         "* TODO %?  %i\n  %a\n")
        ;; meeting
        ("m" "Meeting" entry (file+headline  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/meetings.org")) "Tasks")
         "* Scheduled at: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))  %?  %i\n  %a\n")
        ;; to complete on someday
        ;; ("i" "Idea" entry (file+datetree  (lambda () (concat dropbox-path "emacs/org_files/org_mode/notes/ideas.org")))
        ;;  "* %?\n Idea: %U\n  %i\n  Link: %a")
        ("n" "Note" entry (file+datetree  (lambda () (concat dropbox-path "emacs/org_files/org_mode/notes/notes.org")))
         "* %?\n %U\n  %i\n  Link: %a")
        ("L" "Web" entry (file+datetree  (lambda () (concat dropbox-path "emacs/org_files/org_mode/notes/notes_from_web.org")))
         "* %?\n %U\n  %i\n  Link: %a")
        
        ("r" "Respond to email" entry (file  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/reply_later.org")))
         "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t
         :clock-resume t
         :immediate-finish t)
        ("e" "Email Workflow")
        ("ea" "Action" entry (file+olp  (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/mail.org")) "Action")
         "* TODO Action:  %?  :mail:\n %:fromname on %a\n%i")
        ("ef" "Follow Up" entry (file+olp (lambda ()(concat dropbox-path "emacs/org_files/org_mode/agenda/mail.org")) "Follow Up")
         "* TODO Follow Up: %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
        ("er" "Read Later" entry (file+olp (lambda () (concat dropbox-path "emacs/org_files/org_mode/agenda/mail.org")) "Read Later")
         "* TODO Read Later: %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)
        )
      )

;; to align auto added tags: https://www.reddit.com/r/emacs/comments/93990v/automatically_add_tag_to_capture_in_org_mode/
(add-hook 'org-capture-mode-hook #'org-align-all-tags)

;;(add-hook 'org-capture-before-finalize-hook 'org-trello/sync-card)

;; refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;org -bullet
;; (use-package org-bullets
;;       :ensure t
;;       :config
;;       (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(require 'org-habit)


;;to suppress warning
(eval-when-compile
  (defvar url-http-method ())
  (defvar url-http-data ())
  (defvar url-http-extra-headers ())
  (defvar oauth-token-data ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ())
  (defvar oauth--token-data ())
  )


(use-package calfw
  :ensure t)

(use-package calfw
  :init
  (progn
    (use-package calfw-cal :ensure t)
    (use-package calfw-org :ensure t)
    (use-package calfw-ical :ensure t))
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  (require 'calfw-ical)

  (defun mycalendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Orange")  ;; orgmode source
      (cfw:ical-create-source "KFUPM" "https://mail.kfupm.edu.sa/owa/calendar/1bc21ff61def45ce8e9b165c2fa08a5a@kfupm.edu.sa/150b1c7e169a47f7abd86b624623852e13646414716493109298/calendar.ics" "Green") ; devorah calender
      (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/embed?src=dsbiq03n1hhuudgu45hhndmusa14j68g%40import.calendar.google.com&ctz=Asia%2FRiyadh" "IndianRed") ; google calendar ICS
      )))
  (setq cfw:org-overwrite-default-keybinding t))


(use-package calfw-gcal
  :ensure t
  :config
  (require 'calfw-gcal))

;; org gtaks

;; (use-package org-gtasks
;;   :ensure t
;;   )
;; (org-gtasks-register-account :name "JAK"
;;                              :directory "/mnt/data/Dropbox/org_files/tasks/g_tasks.org"
;;                              :client-id "340422362289-qjso05l63gkovrjp741g0muut2u5dujn.apps.googleusercontent.com"
;;                             :client-secret "B70ft_qMhgdji0XeIAcVh4HC")
(use-package olivetti
  :ensure t)

;; slide presentation
(use-package org-tree-slide
  :ensure t
  :commands org-tree-slide-mode
  :custom
  (org-tree-slide-activate-message "")
  (org-tree-slide-deactivate-message "")
  (org-tree-slide-breadcrumbs "    >    ")
  (org-tree-slide-heading-emphasis t)
  (org-tree-slide-slide-in-waiting 0.025)
  (org-tree-slide-content-margin-top 4)
  ;; to be able to start presentation from the subtree where the cursor is at
  (org-tree-slide-cursor-init nil)
  :custom-face
  (org-tree-slide-heading-level-1 ((t (:height 1.8 :weight bold))))
  (org-tree-slide-heading-level-2 ((t (:height 1.5 :weight bold))))
  (org-tree-slide-heading-level-3 ((t (:height 1.3 :weight bold))))
  (org-tree-slide-heading-level-4 ((t (:height 1.1 :weight bold))))
  :bind
  ( :map org
         ("s" . org-tree-slide-mode)
         :map org-tree-slide-mode-map
         ("<f8>" . org-tree-slide-content)
         ("<f9>" . org-tree-slide-move-previous-tree)
         ("<f10>" . org-tree-slide-move-next-tree)
         ("C-n" . (lambda () (interactive) (if cursor-type
                                               (next-line)
                                             (setq-local cursor-type t)
                                             (next-line)))))
  :hook
  (org-tree-slide-play . (lambda () (setq-local beacon-mode nil)))
  (org-tree-slide-stop . (lambda () (setq-local beacon-mode t)))
  (org-tree-slide-before-narrow . (lambda () (setq-local cursor-type nil)))
  (org-tree-slide-stop . (lambda () (setq-local cursor-type t)))
  (org-tree-slide-play . variable-pitch-mode)
  (org-tree-slide-stop . (lambda () (variable-pitch-mode -1)))
  (org-tree-slide-play . fk/hide-org-metalines-toggle)
  (org-tree-slide-stop . fk/hide-org-metalines-toggle)
  (org-tree-slide-before-narrow . org-remove-inline-images)
  (org-tree-slide-after-narrow . org-display-inline-images)
  (org-tree-slide-play . fk/org-tree-slide-update-modeline)
  (org-tree-slide-stop . fk/org-tree-slide-update-modeline)
  (org-tree-slide-play . (lambda () (setq-local olivetti-body-width 95) (olivetti-mode 1)))
  (org-tree-slide-stop . (lambda () (setq-local olivetti-body-width 120) (olivetti-mode 1)))
  :config
  (defun fk/buffer-contains-substring (string)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (and-let* ((pos (search-forward string nil t))
                   (visible (not (outline-invisible-p pos))))))))

  (setq fk/org-meta-line-hide-p nil)
  (setq fk/org-meta-line-face-remap nil)

  (defun fk/hide-org-metalines-toggle ()
    "Hide or unhide meta lines starting with \"#+\" in org-mode."
    (interactive)
    (if fk/org-meta-line-hide-p
        (face-remap-remove-relative fk/org-meta-line-face-remap)
      (setq fk/org-meta-line-face-remap (face-remap-add-relative 'org-meta-line
                                                                 :foreground fk/background-color)))
    (setq fk/org-meta-line-hide-p (not fk/org-meta-line-hide-p)))

  (defun fk/org-tree-slide-update-modeline ()
    "Show current page in modeline."
    (let ((slide-position '(:eval (format " %s " (org-tree-slide--count-slide (point))))))
      (if (org-tree-slide--active-p)
          (setq-local global-mode-string (append global-mode-string (list slide-position)))
        (setq-local global-mode-string (delete slide-position global-mode-string))))))

;; Alternative
(use-package epresent
  :ensure t
  :commands epresent-run)

;; Another alternative
(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root (concat "file://" (expand-file-name "~/.emacs.d/jak/reveal.js")))
  (setq Org-Reveal-title-slide nil)
  (setq org-reveal-theme "blood") ;; blood, solarized, moon
  (setq org-reveal-transition "zoom")
  )
;; images from web into emacs
(use-package org-download
  :ensure t
  :after org
  :config 
  (custom-set-variables
   '(org-download-screenshot-method "flameshot gui --raw > %s"))
  (setq org-download-image-org-width 330)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))



;; writing
;; grammar using langtool
;; https://github.com/mhayashi1120/Emacs-langtool
(use-package langtool
  :ensure t
  )
;; https://languagetool.org/
;; downloaded from https://languagetool.org/download/
;; (setq langtool-language-tool-jar (concat dropbox-path "emacs/LanguageTool-5.4/languagetool-commandline.jar"))
(setq langtool-language-tool-jar (concat dropbox-path "emacs/LanguageTool-5.9/languagetool-commandline.jar"))
(require 'langtool)


;; score details are here
;; https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests
(use-package writegood-mode
  :ensure t
  )


(add-to-list 'org-emphasis-alist
             '(
               ;; "*" (:foreground "yellow") ;; bold
               "*" (:foreground "green") ;; bold
               "/" (:foreground "green") ;; italics
               "_" (:foreground "red") ;; underline
               ;;"=" (:foreground "orange") ;; verbatim
               ;;"~" (:foreground "purple") ;; code
               )
             )

(use-package org-pomodoro
  :ensure t
  :straight (:files ("*"))  ; For sound files
  :commands org-pomodoro
  :custom
  (org-pomodoro-audio-player "ffplay")
  :config
  ;; Apply args for all sounds
  (advice-add 'org-pomodoro-sound-args :override (lambda (_) "-volume 20 -nodisp -nostats -hide_banner")))


(use-package org-wild-notifier
  :init
  (setq org-wild-notifier-mode 1)
  :ensure t
  )

;; (use-package valign
;;   :ensure t
;;   :straight (:host github :repo "casouri/valign")
;;   :custom
;;   (valign-fancy-bar t)
;;   :hook
;;   (org-mode . valign-mode))

;; (use-package org-appear
;;   :ensure t
;;   :straight (:host github :repo "awth13/org-appear" :branch "feature/time-stamps")
;;   :custom
;;   (org-appear-autolinks t)
;;   :hook
;;   (org-mode . org-appear-mode))


;; to align table
(setq org-startup-align-all-tables t)


(use-package flyspell-popup
  :ensure t
  :after flyspell
  :custom
  (flyspell-popup-correct-delay 2)
  :config
  (flyspell-popup-auto-correct-mode))

(eval-after-load "org"
  '(require 'ox-md nil t))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-'" . flyspell-correct-wrapper))
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil) ;; unbind 
  )

(use-package flyspell-correct-helm
  :ensure t
  :after flyspell-correct
  )
;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-'" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-popup
;;   :ensure t
;;   :after flyspell-correct)

;; remap shortcuts of flyspell
;; by default c-; is flyspell-auto-correct-previous-word 
;; unset spell correct password
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

;; David Wilson
;; This is needed as of Org 9.2
(require 'org-tempo)
;; (add-to-list 'org-modules 'org-tempo t)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))
(add-to-list 'org-structure-template-alist '("calc" . "src calc"))
(add-to-list 'org-structure-template-alist '("lc" . "src literate-calc"))

;;We want the same syntax highlighting in source blocks as in the native language files.
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; bind to company completions
(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-c C-i") nil)
(define-key org-mode-map (kbd "C-c C-p C-p") nil)
;;(Define-key org-mode-map (kbd "C-,") nil)

(setq org-export-with-broken-links t)

(use-package 
  easy-hugo 
  :ensure t
  :init
  (setq easy-hugo-helm-ag t)
  (setq easy-hugo-basedir (concat dropbox-path "emacs/hugo/braindump/"))
  (setq easy-hugo-postdir "content/posts")
  (setq easy-hugo-bloglist
	;; blog2 setting
	    #'(((easy-hugo-basedir . "/mnt/data/Dropbox/emacs/hugo/ksaflyer-blog/")
           	(easy-hugo-url . "http://example2.com")
	        (easy-hugo-sshdomain . "myblogdomain")
	        ;; (easy-hugo-root . "/home/hugo/")
           (easy-hugo-postdir . "content"))
	;; (easy-hugo-url . "http://example2.com")
	;; (easy-hugo-sshdomain . "myblogdomain")
	;; (easy-hugo-root . "/home/hugo/")
    )
    )
	;; ;; blog3 setting for aws s3
	;; ((easy-hugo-basedir . "~/src/github.com/masasam/hugo3/")
	;; (easy-hugo-url . "http://example3.net")
	;; (easy-hugo-amazon-s3-bucket-name . "yourS3bucketname"))
	;; ;; blog4 setting for google cloud strage
	;; ((easy-hugo-basedir . "~/src/github.com/masasam/hugo4/")
	;; (easy-hugo-url . "http://example4.net")
	;; (easy-hugo-google-cloud-storage-bucket-name . "yourGCPbucketname")
	;; (easy-hugo-image-directory . "img"))
	;; ;; blog5 for github pages
	;; ((easy-hugo-basedir . "~/src/github.com/masasam/githubpages/")
	;; (easy-hugo-url . "https://yourid.github.io"))
	;; ;; blog6 for firebase hosting
	;; ((easy-hugo-basedir . "~/src/github.com/masasam/firebase/")
	;; (easy-hugo-url . "https://yourproject.firebaseapp.com"))))
  )


(defun org-capture-jak-work ()
  (interactive)
  (org-capture nil "w"))


(defun org-capture-jak-personal ()
  (interactive)
  (org-capture nil "p"))


(defun org-agenda-show-super-zaen-view (&optional arg)
  (interactive "P")
  (org-agenda arg "z"))

(defun org-agenda-show-jak-customized (&optional arg)
  (setq org-agenda-window-setup 'reorganize-frame)  
  (interactive "P")
  (org-agenda arg "j"))

(defun org-agenda-show-jak-customized-independent-window (&optional arg)
  (interactive "P")
  (setq org-agenda-window-setup 'only-window)  
  (org-agenda arg "j"))

(defun org-agenda-show-super-week-view (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; 0-6 is Sunday-Saturday 
(setq org-agenda-start-on-weekday 0)

(use-package  quelpa-use-package
  :ensure t)
(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))

(use-package helm-org-ql
  :quelpa (helm-org-ql :fetcher github :repo "alphapapa/org-ql"
                       :files ("helm-org-ql.el")))

;; custom date time format
(setq org-agenda-timegrid-use-ampm t)

;; custom time format
;; https://emacs.stackexchange.com/questions/19863/how-to-set-my-own-date-format-for-org
;; not working but
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

;; in org mode imenu index headlines of level 2 not below that
(setq org-imenu-depth 7)

;; more details of about this here
;;https://github.com/nobiot/org-transclusion
;; https://www.youtube.com/watch?v=KxDrE3spAL8

(use-package org-transclusion
              :ensure t
              :after org
              )
(defun org-table-transform-in-place ()
  "Just like `ORG-TABLE-EXPORT', but instead of exporting to a
  file, replace table with data formatted according to user's
  choice, where the format choices are the same as
  org-table-export."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)
  (let* ((format
      (completing-read "Transform table function: "
               '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
                 "orgtbl-to-html" "orgtbl-to-generic"
                 "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
                 "orgtbl-to-unicode")))
     (curr-point (point)))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
    (let ((transform (intern (match-string 1 format)))
          (params (and (match-end 2)
               (read (concat "(" (match-string 2 format) ")"))))
          (table (org-table-to-lisp
              (buffer-substring-no-properties
               (org-table-begin) (org-table-end)))))
      (unless (fboundp transform)
        (user-error "No such transformation function %s" transform))
      (save-restriction
        (with-output-to-string
          (delete-region (org-table-begin) (org-table-end))
          (insert (funcall transform table params) "\n")))
      (goto-char curr-point)
      (beginning-of-line)
      (message "Tranformation done."))
      (user-error "Table export format invalid"))))

(define-key org-mode-map (kbd "\C-x |") 'org-table-transform-in-place)

(provide 'init-org-mode)
;;; init-org.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

