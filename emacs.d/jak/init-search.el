(use-package consult
  :ensure t)


(defun bms/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))
(global-set-key (kbd "C-c <f1>") 'bms/org-roam-rg-search)


(use-package helm-rg
  :ensure t
  :init
  (defun helm-all-roam-files (&optional query)
    "Search with rg in your roam directory, QUERY."
    (interactive)
    (let ((helm-rg-default-directory org-roam-directory))
      (helm-rg query nil)))
  (defun helm-all-org-files (&optional query)
    "Search with rg in your roam directory, QUERY."
    (interactive)
    (let ((helm-rg-default-directory "/mnt/data/Dropbox/emacs/org_files/org_mode"))
      (helm-rg query nil)))
  (defun helm-all-meetings (&optional query)
    "Search with rg in your roam directory, QUERY."
    (interactive)
    (let ((helm-rg-default-directory "/mnt/data/Dropbox/emacs/org_files/org_roam/files/meetings"))
      (helm-rg query nil)))
  (defun helm-rg-all-repos (&optional query)
    "Search with rg all repos, QUERY."
    (interactive)
    (let ((helm-rg-default-directory "/home/jaavedkhan/Code/"))
      (helm-rg query nil)))
  :bind
  (("<f12>" . helm-all-roam-files)
   ("<f11>" . helm-rg-all-repos)))


;; ;; deft
;; ;; https://github.com/jrblevin/deft
;; (use-package deft
;;   :ensure t
;;   :bind ("<f8>" . deft)
;;   :commands (deft)
;;   :config
;;   (setq deft-directory (concat dropbox-path "emacs/org_files")
;;         deft-extensions '("md" "org" "txt")
;;         deft-recursive t
;;         ;; org roam migration issue to version 2
;;         ;; https://github.com/jrblevin/deft/issues/75
;;         deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
;;         deft-use-filename-as-title 't
;;         )
;;   )


;; (use-package deft
;;   :after org
;;   ;; :bind
;;   ;; ("<f8>" . deft)
;;   :config
;;   (setq deft-directory (concat dropbox-path "emacs/org_files")
;;         deft-extensions '("md" "org" "txt"))
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory org-roam-directory))

;; apt dependencies are
;; libxapian-dev
;; libtclap-dev
;; then from
;; ~/.emacs.d/straight/repos/notdeft/xapian -> make
(use-package notdeft
  :straight (:host github :repo "hasu/notdeft" :files ("*.el" "xapian"))
  :bind
  ("<f8>" . notdeft)
  :config
  (setq notdeft-directories (list(concat dropbox-path "emacs/org_files")))
  (setq notdeft-xapian-program "/home/jaavedkhan/.emacs.d/straight/repos/notdeft/xapian/notdeft-xapian")
  (setq notdeft-allow-org-property-drawers t)
  :config
  (progn
    (add-hook 'org-mode-hook 'notdeft-note-mode)
  (require 'notdeft-global)
  ;; (global-set-key [f8] 'notdeft-global-map)
  )
  (define-key notdeft-mode-map (kbd "C-c C-j") nil)
  )

(defun notdeft-search-meetings ()
  (interactive)
  (notdeft-open-query "tag:meeting")
  )

(defun notdeft-search-bookmarks ()
  (interactive)
  (notdeft-open-query "tag:bookmark")
  )


;;; copied

                                        ; refined init.el, gradually will mode all of my config here
                                        ; loaded in dotspacemacs/user-config

;;; random helpers

                                        ; TODO should this be macro??
(cl-defun with-error-on-user-prompt (body)
  "Suppress user prompt and throw error instead. Useful when we really want to avoid prompts, e.g. in non-interactive functions"
  (interactive)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (arg) (error "IGNORING PROMPT %s" arg))))
    (eval body)))
;;;

;;; searching for things
(cl-defun my/files-in (path &key (exts nil) (follow nil))
  "Search for files with certail extensions and potentially following symlinks.
   None of standard Elisp functions or popular libs support following symlink :(
   In addition, rg is ridiculously fast."
  (let* ((patterns (s-join " " (-map (lambda (i) (format "-g '*.%s'" i)) exts)))
         (follows (if follow "--follow" ""))
         ;; TODO use fd maybe?
         (rg-command (format "rg --files %s -0 %s %s" follows patterns path)))
    (-map #'file-truename (s-split "\0" (shell-command-to-string rg-command) t))))

(cl-defun my/org-files-in (path &key (archive nil) (follow nil))
  (my/files-in path :exts (if archive '("org" "org_archive") '("org")) :follow follow))


(cl-defun --my/helm-files-do-rg (dir &key (targets nil) (rg-opts nil))
  "Helper function to aid with passing extra arguments to ripgrep"
  (require 'helm-ag)
  ;; TODO need to ignore # files?
  (let ((helm-ag-command-option (s-join " " rg-opts)))
    ;; NOTE: spacemacs/helm-files-do-rg is patched to support second argument with multiple directories
    ;; (see patch-helm.el)
    (helm-rg dir targets)
    )
  )

(defun --my/find-file-defensive (f)
  "Open file, ignoring lock files, various IO race conditions and user prompts.
   Returns filename if successful, othewise nil"
  (ignore-errors (with-error-on-user-prompt `(find-file-read-only f)) f))


;; TODO FIXME fucking hell, elisp doesn't seem to have anything similar to e.g. check_call in python
;; also no simple way to pass set -eu -o pipefail
;; so, if find or xargs fails, you'd get with garbage in the variable

;; really wish there was some sort of bridge for configuring emacs on other programming languages
;; there is zero benefit of using Elisp for most of typical emacs configs; only obstacles.
;; can't say about other lisps, but very likely it's not very beneficial either


(setq my/git-repos-search-root "/home/jaavedkhan/cdrive/code")

(defun --my/git-repos-refresh ()
  (let ((search-git-repos-cmd (s-join " "
                                      `(
                                        "fdfind"
                                        "--follow" ; follow symlink
                                        "--hidden" "--type d" "'.git$'" ; match git dirs
                                        ,(format "'%s'" my/git-repos-search-root)
                                        "-x" "readlink" "-f" "'{//}'")))) ; resolve symlinks
    (progn
      (message "refreshing git repos...")
      (defconst *--my/git-repos*
        (-distinct (s-split "\n" ; remove duplicates due to symlinking
                            (shell-command-to-string search-git-repos-cmd) t)))))) ; t for omit-nulls


(defun my/code-targets ()
  "Collects repositories across the filesystem and bootstraps the timer to update them"
                                        ; TODO there mustbe some generic caching mechanism for that in elisp?
  (let ((refresh-interval-seconds (* 60 5)))
    (progn
      (unless (boundp '*--my/git-repos*)
        (--my/git-repos-refresh)
        (run-with-idle-timer refresh-interval-seconds t '--my/git-repos-refresh))
      *--my/git-repos*)))



(defun --my/one-off-helm-follow-mode ()
  ;; I only want helm follow when I run helm-ag against my notes,
  ;; but not all the time, in particular when I'm running my/search-code because it
  ;; triggers loading LSP etc
  ;; Problem is helm-follow-mode seems to be handled on per-source basis
  ;; and there is some logic that tries to persist it in customize-variables
  ;; for future emacs sessions.
  ;; helm-ag on one hand seems to use since source (helm-ag-source) for all searches
  ;; on the orther hand it does some sort of dynamic renaming and messing with source names
  ;; (e.g. search by "helm-attrset 'name")
  ;; As a result it's very unclear what's actually happening even after few hours of debugging.
  ;; also see https://github.com/emacs-helm/helm/issues/2006,

  ;; other things I tried (apart from completely random desperate attempts)
  ;; - setting
  ;;   (setq helm-follow-mode-persistent t)
  ;;   (setq helm-source-names-using-follow `(,(helm-ag--helm-header my/search-targets))))
  ;; - using different source similar to helm-ag-source, but with :follow t -- doesn't work :shrug:

  ;; in the end I ended up hacking hooks to enable follow mode for specific helm call
  ;; and disabling on closing the buffer. Can't say I like it at all and looks sort of flaky.

  (defun --my/helm-follow-mode-set (arg)
    "Ugh fucking hell. Need this because helm-follow-mode works as a toggle :eyeroll:"
    (unless (eq (helm-follow-mode-p) arg)
      (helm-follow-mode)))

  (defun --my/enable-helm-follow-mode ()
    (--my/helm-follow-mode-set t))

  (defun --my/disable-helm-follow-mode ()
    (--my/helm-follow-mode-set nil)
    (remove-hook 'helm-move-selection-before-hook '--my/enable-helm-follow-mode)
    (remove-hook 'helm-cleanup-hook '--my/disable-helm-follow-mode))

  ;; ugh, helm-move-selection-before-hook doesn seem like the right one frankly
  ;; but I haven't found anything better, e.g. helm-after-initialize-hook seems too early
  ;; helm-after-update-hook kinda worked, but immediately dropped after presenting results
  ;; as helm complains at 'Not enough candidates' :(
  (add-hook 'helm-move-selection-before-hook '--my/enable-helm-follow-mode)
  (add-hook 'helm-cleanup-hook '--my/disable-helm-follow-mode))


(defun my/search ()
  (interactive)
  (--my/one-off-helm-follow-mode)
  (--my/helm-files-do-rg my/search-targets
                         :rg-opts '("--follow")))

(defun my/search-code ()
  (interactive)
  (--my/helm-files-do-rg "/"
                         :targets (my/code-targets)
                         :rg-opts '("-T" "txt" "-T" "md" "-T" "html" "-T" "org" "-g" "!*.org_archive")))


(with-eval-after-load 'helm-ag
  ;; see helm-ag--construct-command. Not configurable otherwise ATM
  (defun helm-ag--construct-ignore-option (pattern)
    (concat "--glob=!" pattern)))
;;;

;;; org-drill
(defun --my/drill-with-tag (tag)
  (require 'org-drill)
  (let ((org-drill-question-tag tag))
    (org-drill (my/org-files-in my/drill-targets :follow t))))

(defun my/habits ()
  (interactive)
  (--my/drill-with-tag "habit"))


(defun my/drill ()
  (interactive)
  (--my/drill-with-tag "drill"))

;;; org-agenda

(defun get-org-agenda-files ()
  (my/org-files-in my/agenda-targets :follow t))

                                        ; TODO hotkey to toggle private/non private?
(defun my/agenda (&optional arg)
  (interactive "P")
  (require 'org-agenda)
  (let ((org-agenda-tag-filter-preset '("-prv"))
        (org-agenda-window-setup 'only-window))
    (org-agenda arg "a")))

(defun my/switch-to-agenda ()
  "launch agenda unless it's already running"
  (interactive)
  (if (get-buffer "*Org Agenda*") (switch-to-buffer "*Org Agenda*") (my/agenda)))

;;;


;;; org-refile

(defun get-org-refile-targets ()
  (my/org-files-in my/refile-targets :follow t))

;;;

;;;

;;; misc stuff
(defun my/now ()
  "Insert current timestamp in org-mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
;;;



;; (global-set-key (kbd "<f1>") #'my/search)
;; (global-set-key (kbd "<f3>") #'my/search-code)
;;;

(use-package org-ql
  :ensure t)

(use-package helm-org-ql
  :ensure t)

(use-package helm-recoll
  :ensure t
  :commands helm-recoll
  :init (setq helm-recoll-directories
              '(
                ("books" . "~/.recoll-dropbox-documents")
                ("dropbox" . "~/.recoll-dropbox")
                ))
  :config
  
  )

(defun helm-ff-recoll-index-directory (directory)
  "Create a recoll index directory from DIRECTORY.
Add the new created directory to `helm-recoll-directories' using the
basename of DIRECTORY as name.
By using `customize-set-variable', a new source is created for this
new directory."
  (cl-assert (boundp 'helm-recoll-directories) nil
             "Package helm-recoll not installed or configured")
  (let* ((bn (helm-basename (expand-file-name directory)))
         (index-dir (format "~/.recoll-%s" bn))
         (conf-file (expand-file-name "recoll.conf" index-dir))) 
    (mkdir index-dir)
    (with-current-buffer (find-file-noselect conf-file)
      (insert (format "topdirs = %s" (expand-file-name directory)))
      (save-buffer)
      (kill-buffer))
    (customize-set-variable 'helm-recoll-directories
                            (append `((,bn . ,index-dir)) helm-recoll-directories))
    (message "Don't forget to index config directory with 'recollindex -c %s'" index-dir)))

(defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Recoll index directory"
   'helm-ff-recoll-index-directory
   source
   'file-directory-p
   3))


(provide 'init-search)
