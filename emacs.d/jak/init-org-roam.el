(setq org-roam-v2-ack t)

;; ref: https://jethrokuan.github.io/org-roam-guide/

;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;;       (file-name-nondirectory
;;        (directory-file-name
;;         (file-name-directory
;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))

(setq org-roam-node-display-template
      ;; (concat "${title:*} " (propertize "${tags:10} " 'face 'org-tag) "${type:15}" )
      (concat "${title:*} " (propertize "${tags:15} " 'face 'org-tag))
      )

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
      '(
        ;; ("d" "default" plain "%?" :if-new
        ;;  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
        ;;  :unnarrowed t)
       ("d" "Permenant Note" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+FILETAGS: permanent\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n - tags :: ")
         :unnarrowed t)
       ("p" "project" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+FILETAGS: :permanent:project:\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n* Project\n - tags :: \n")
         :unnarrowed t)

        ("f" "Fleeting Note" plain "%?"
         :if-new (file+head "fleeting/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+FILETAGS: fleeting\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n - tags :: ")
         :unnarrowed t)
        ("9" "Project Note" plain "%?"
         :if-new (file+head "permenant/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+FILETAGS: project\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n* Project\n - tags :: \n")
         :unnarrowed t)
        ("i" "Idea" plain "%?"
         :if-new (file+head "permanent/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+ROAM_TAGS: idea\n#+FILETAGS: idea\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n* Idea \n - tags :: ")
         :unnarrowed t)
        ("m" "Meeting Note" plain "%?"
         :if-new (
                  file+head "meetings/${slug}--%<%Y%m%d-%H%M%S>.org"
                            ;; ":PROPERTIES
;; :ROAM_EXCLUDE: t
;; :END:
"#+TITLE: ${title}\n#+ROAM_TAGS: :meeting:\n#+FILETAGS: :meeting:\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n* Meeting\n - tags :: \n** SCHEDULED: <%<%Y-%m-%d %a>> \n\n* Present at meeting[0/0]\n - [X] [[id:b459290d-646e-4dfd-82b4-83235c2d63b3][Javeed Ali Khan]] \n\n* Agenda\n - \n\n* Notes\n\n* Questions during the meeting\n\n* Actions \n** TODO")
         :unnarrowed t)
        ("h" "Person" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+ROAM_TAGS: person\n#+FILETAGS: person\n\n* Person\n - tags :: \n")
         :unnarrowed t)
        ("o" "Organization" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+ROAM_TAGS: organization\n#+FILETAGS: organization\n\n* Organization\n - tags :: \n")
         :unnarrowed t)
        ("w" "Web" plain "%?"
         :if-new (file+head "internet/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+FILETAGS: web\n\n - tags :: \n")
         :unnarrowed t)
        ("a" "Area" plain "%?"
         ;; Area of responsiblity or interest where some standard needs to be maintained
         :if-new (file+head "permanent/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+FILETAGS: area\n\n* Area\n - tags :: \n")
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
                      ;; ":PROPERTIES:
;; :ROAM_EXCLUDE: t
;; :END:
"#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+fILETAGS: :bookmark:\n#+ROAM_TAGS: :bookmark:\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n* Notes or comments\n\n* [[${ref}][URL]]\n\n* Clipped text\n${body}\n")
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

(defun jak/org-roam-node-find-meeting()
  (interactive)
  (org-roam-node-find  :key "meeting ")
  )


(defun jak/org-roam-node-find-project()
  (interactive)
  (org-roam-node-find  :key "project ")
  )

(defun jak/org-roam-node-find-person()
  (interactive)
  (org-roam-node-find  :key "person ")
  )

(defun jak/org-roam-node-find-organization()
  (interactive)
  (org-roam-node-find  :key "organization ")
  )

(defun jak/org-roam-node-find-web()
  (interactive)
  (org-roam-node-find  :key "web ")
  )

;; (use-package consult-notes
;;   :ensure t
;;   :straight (:type git :host github :repo "mclear-tools/consult-notes")
;;   :commands (consult-notes
;;              consult-notes-search-in-all-notes
;;              ;; if using org-roam 
;;              consult-notes-org-roam-find-node
;;              consult-notes-org-roam-find-node-relation)
;;   :config
;;   ;; (setq consult-notes-sources '("Name"  ?key  "path/to/dir")) ;; Set notes dir(s), see below
;;   (setq consult-notes-sources
;;       '(("Roam"             ?o "/mnt/data/Dropbox/emacs/org_files/org_roam")
;;         ("Org"      ?r "/mnt/data/Dropbox/emacs/org_files/org_mode")))
;;   ;; Set org-roam integration OR denote integration
;;    ;; (when (locate-library "denote")
;;     (when (locate-library "org-roam")
;;   (consult-notes-org-roam-mode)))

;; (defun bms/org-roam-rg-search ()
;;   "Search org-roam directory using consult-ripgrep. With live-preview."
;;   (interactive)
;;   (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
;;     (consult-ripgrep org-roam-directory)))

;; ;;(global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)

(provide 'init-org-roam)
