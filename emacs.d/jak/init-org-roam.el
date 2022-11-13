(setq org-roam-v2-ack t)

;;(setq org-roam-directory  '/home/jaavedkhan/Dropbox/emacs/org_files/org_roam/files)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory  (file-truename(concat dropbox-path  "emacs/org_files/org_roam/files/")))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "meeting" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-file-exclude-regexp "org-roam/data/")
  (org-roam-setup)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-location
      (concat org-roam-directory "/.database/org-roam.db"))
  )

;; setting up of capture templates not working in :config
;; inserting property drawer in template
;; https://github.com/org-roam/org-roam/issues/1783#issuecomment-900452202
(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :if-new
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("p" "Permenant Note" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+FILETAGS: permanent\n")
         :unnarrowed t)
        ("f" "Fleeting Note" plain "%?"
         :if-new (file+head "fleeting/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+FILETAGS: fleeting\n")
         :unnarrowed t)
        ("9" "Project Note" plain "%?"
         :if-new (file+head "permenant/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+FILETAGS: project\n")
         :unnarrowed t)
        ("i" "Idea" plain "%?"
         :if-new (file+head "permanent/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+ROAM_TAGS: idea\n#+FILETAGS: idea\n")
         :unnarrowed t)
        ("m" "Meeting Note" plain "%?"
         :if-new (
                  file+head "meetings/${slug}--%<%Y%m%d-%H%M%S>.org"
                            ":PROPERTIES:
:ROAM_EXCLUDE: t
:END:
#+TITLE: ${title}\n#+ROAM_TAGS: :meeting:\n#+FILETAGS: :meeting:\n* Summary \n** SCHEDULED: <%<%Y-%m-%d %a>> \n\n* Present at meeting[0/0]\n - [X] [[id:b459290d-646e-4dfd-82b4-83235c2d63b3][Javeed Ali Khan]] \n\n* Agenda\n - \n\n* Notes\n\n* Actions\n")
         :unnarrowed t)
        ("h" "Person" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+ROAM_TAGS: person\n#+FILETAGS: person\n")
         :unnarrowed t)
        ("o" "Organization" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+ROAM_TAGS: organization\n#+FILETAGS: organization\n")
         :unnarrowed t)
        ("w" "Web" plain "%?"
         :if-new (file+head "internet/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}--%<%Y%m%d-%H%M%S>\n#+FILETAGS: web\n")
         :unnarrowed t)
        ))

(require 'org-protocol)
(require 'org-roam-protocol)

(setq org-roam-capture-ref-templates
      ;; '(("i" "internet" plain #'org-roam-capture--get-point "%?"
      ;;    :file-name "internet/%<%Y%m%d%H%M>-${slug}"
      ;;    :unnarrowed t))
      '(("i" "internet" plain "%?" :if-new
         (file+head "internet/%<%Y%m%d%H%M>-${slug}.org" 
                      ":PROPERTIES:
:ROAM_EXCLUDE: t
:END:
#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+fILETAGS: :bookmark:\n#+ROAM_TAGS: :bookmark:\n* Clipped text\n${body}")
         :unnarrowed t)
        ("r" "ref" plain "%?" :if-new
         (file+head "${slug}.org" "#+title: ${title}")
         :unnarrowed t)))

;; ref: https://www.orgroam.com/manual.html
;; to exclude certain tags from org-find

;; (setq org-roam-db-node-include-function
;;       (lambda ()
;;         (not
;;          (member "bookmark" (org-get-tags))
;;          )))


;; (use-package org-roam-ui
;;   :straight
;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   ;;  :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

(use-package org-roam-ui
  :ensure t)

(use-package org-roam-timestamps
  :ensure t)

;; org-protocol
(require 'org-protocol)
;; to enable org-protocol for global shortcuts
(require 'server)
(if (not (server-running-p)) (server-start))
;; (setq server-socket-dir "~/.emacs.d/server")
;; (setq server-name "emacs-26")


;; sudo apt install gawk
;; (use-package org-fc
;;   :straight
;;   (org-fc
;;    :type git :repo "https://git.sr.ht/~l3kn/org-fc"
;;    :files (:defaults "awk" "demo.org"))
;;   :custom
;;   (org-fc-directories (list (concat dropbox-path "emacs/org_files")))
;;   :config
;;   (require 'org-fc-hydra)
;;   (global-set-key (kbd "C-c f") 'org-fc-hydra/org-fc-update)
;;   )

;;(global-set-key (kbd "<f4>") 'org-roam-capture)

(setq org-roam-dailies-capture-templates
      '(
        ("d" "default" entry "* %?" :if-new
         (file+head "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>"))
        ))

(provide 'init-org-roam)
