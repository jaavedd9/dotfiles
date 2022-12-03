;; Export org-roam files
(use-package ox-hugo
  :ensure t            ;Auto-install thepackage from Melpa (optional)
  :after ox
  :config
  (setq org-hugo-base-dir (concat dropbox-path "emacs/hugo/braindump"))
  ;;  (setq org-hugo-section "braindump")
  ;;  (setq org-hugo-base-dir (concat (dropbox-path )))
  )

;; from : https://sidhartharya.me/exporting-org-roam-notes-to-hugo/
;; (defun org-hugo--tag-processing-fn-roam-tags(tag-list info)
;;   "Process org roam tags for org hugo"
;;   (if (org-roam-file-p)
;;       (append tag-list
;;               '("braindump")
;;               (mapcar #'downcase (org-roam--extract-tags)))
;;     (mapcar #'downcase tag-list)
;;     ))

;; (add-to-list 'org-hugo-tag-processing-functions 'org-hugo--tag-processing-fn-roam-tags)

(defun org-hugo--org-roam-backlinks (backend)
  (when (equal backend 'hugo)
    (when (org-roam-file-p)
      (beginning-of-buffer)
      (replace-string "{" "")
      (beginning-of-buffer)
      (replace-string "}" "")
      (end-of-buffer)
      ;;(org-roam-buffer--insert-backlinks)
      )))
(add-hook 'org-export-before-processing-hook #'org-hugo--org-roam-backlinks)

(setq org-export-exclude-tags '("noexport" "private" "pain"))
(setq org-hugo-auto-set-lastmod t)

(defun org-hugo--org-roam-save-buffer(&optional no-trace-links)
  "On save export to hugo"
  (when (org-roam-file-p)
    (org-hugo-export-wim-to-md)))

(add-to-list 'after-save-hook #'org-hugo--org-roam-save-buffer)

;; to fix the problem of explorting to hugo
;; with borken links in markdown
;; https://github.com/kaushalmodi/ox-hugo/issues/500#issuecomment-1006674469
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun zeeros/fix-doc-path (path)
  (replace-in-string "permanent/" "" (replace-in-string "../" "" path)))

(advice-add 'org-export-resolve-id-link :filter-return #'zeeros/fix-doc-path)


(defun my-org-hugo-org-roam-sync-all()
  ""
  (interactive)
  (dolist (fil (org-roam--list-files org-roam-directory))
    (with-current-buffer (find-file-noselect fil)
      (org-hugo-export-wim-to-md)
      (kill-buffer))))


;; (defun org-hugo--org-roam-save-buffer()
;;   ""
;;   (when (org-roam-file-p)
;;     (when (<= (length
;;                (split-string
;;                 (replace-regexp-in-string (expand-file-name org-roam-directory) ""
;;                                           (expand-file-name (buffer-file-name org-roam-buffer--current))) "/")) 2
;;                                           )
;;       (unless no-trace-links
;;         (dolist (links (org-roam--extract-links))
;;           (with-current-buffer (find-file-noselect (aref links 1))
;;             (org-hugo--org-roam-save-buffer t)
;;             (kill-buffer))))
;;       (org-hugo-export-wim-to-md)))
;;   (org-hugo-export-wim-to-md)
;;   )

;;(add-to-list 'after-save-hook #'org-hugo--org-roam-save-buffer)

;; (defun my-org-roam-buffer--insert-backlinks ()
;;   "Insert the org-roam-buffer backlinks string for the current buffer."
;;   (let (props file-from)
;;     (if-let* ((file-path (buffer-file-name org-roam-buffer--current))
;;               (titles (with-current-buffer (find-file-noselect file-path)
;;                         (org-roam--extract-titles)))
;;               (backlinks (delete 'nil
;;                                  (mapcar
;;                                   #'(lambda (a)
;;                                       (if (<= (length
;;                                                (split-string
;;                                                 (replace-regexp-in-string
;;                                                  (concat
;;                                                   (expand-file-name org-roam-directory)) "" (car a)) "/")) 2) a))
;;                                   (org-roam--get-backlinks (push file-path titles)))))
;;               (grouped-backlinks (--group-by (nth 0 it) backlinks)))
;;         (progn
;;           (insert (let ((l (length backlinks)))
;;                     (format "\n\n* %s\n"
;;                             (org-roam-buffer--pluralize "Backlink" l))))
;;           (dolist (group grouped-backlinks)
;;             (setq file-from (car group))
;;             (setq props (mapcar (lambda (row) (nth 2 row)) (cdr group)))
;;             (setq props (seq-sort-by (lambda (p) (plist-get p :point)) #'< props))
;;             (insert (format "** %s\n"
;;                             (org-roam-format-link file-from
;;                                                   (org-roam-db--get-title file-from)
;;                                                   "file")))
;;             (dolist (prop props)
;;               (insert "*** "
;;                       (if-let ((outline (plist-get prop :outline)))
;;                           (-> outline
;;                               (string-join " > ")
;;                               (org-roam-buffer-expand-links file-from))
;;                         "Top")
;;                       "\n"
;;                       (if-let ((content (funcall org-roam-buffer-preview-function file-from (plist-get prop :point))))
;;                           (propertize
;;                            (s-trim (s-replace "\n" " " (org-roam-buffer-expand-links content file-from)))
;;                            'help-echo "mouse-1: visit backlinked note"
;;                            'file-from file-from
;;                            'file-from-point (plist-get prop :point))
;;                         "")
;;                       "\n\n"))))
;;       (insert "\n\n* No backlinks!"))))

;; (defun my-org-hugo-org-roam-sync-all()
;;   ""
;;   (interactive)
;;   (dolist (fil (split-string (string-trim (shell-command-to-string (concat "ls " org-roam-directory "/*.org")))))
;;     (with-current-buffer (find-file-noselect fil)
;;       (org-hugo-export-wim-to-md)
;;       (kill-buffer))))


;; have ox-hugo remove DONE tags
(defun ajb-ox-hugo-filter-done-tag (text backend info)
  "Remove DONE tag from task while exporting"
  (when (org-export-derived-backend-p backend 'hugo)
    (replace-regexp-in-string "\"DONE\"" "" text)))

(add-to-list 'org-export-filter-final-output-functions
             'ajb-ox-hugo-filter-done-tag)

(provide 'init-export)
