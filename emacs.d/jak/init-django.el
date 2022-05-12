;; Grep functions for Django
(setq fk/helm-rg-fuzzy-match t)  ; I may wanna disable helm-rg's transform functionality

;; requires package https://github.com/BurntSushi/ripgrep
;; sudo apt install ripgrep (rg)
(use-package helm-rg
  :custom
  (helm-rg--extra-args '("--max-columns" "400"))
  :custom-face
  (helm-rg-file-match-face ((t (:inherit font-lock-type-face :weight bold :underline nil))))
  (helm-rg-line-number-match-face ((t (:inherit line-number))))
  :bind
  ("C-M-s" . fk/helm-rg-dwim)
  :config
  (defun fk/helm-rg-dwim (&optional query)
    "Smarter version of helm-rg.
- Search in project if in a project else search in default (current) directory.
- Start search with selected text if region is active or empty string.
- Escape special characters when searching with selected text."
    (interactive)
    (let ((helm-rg-default-directory (or (projectile-project-root) default-directory))
          (query (or query (fk/convert-string-to-rg-compatible (or (fk/get-selected-text) "")))))
      (cl-letf (((symbol-function 'helm-rg--get-thing-at-pt) (lambda () query)))
        (if fk/helm-rg-fuzzy-match
            (call-interactively 'helm-rg)
          (cl-letf (((symbol-function 'helm-rg--helm-pattern-to-ripgrep-regexp) (lambda (_) _)))
            (call-interactively 'helm-rg))))))

  ;; Use a simpler header in the helm buffer.
  (fset 'helm-rg--header-name (lambda (_) (concat "Search at " helm-rg--current-dir)))

  (defun fk/helm-rg-dwim-with-glob (glob &optional query)
    (interactive)
    (let ((helm-rg-default-glob-string glob))
      (fk/helm-rg-dwim query))))


(defun fk/django-search-models ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "models.py" "models/" "^class "))

(defun fk/django-search-views ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "views*.py" "^class "))

(defun fk/django-search-serializers ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "*serializers*.py" "^class "))

(defun fk/django-search-forms ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "forms*.py" "^class "))

(defun fk/django-search-tests ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "*test*.py" "^class "))

(defun fk/django-search-settings ()
  (interactive)
  ;; TODO: this glob does not work
  (fk/helm-rg-dwim-with-glob "settings*.py" ""))

(defun fk/django-search-admins ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "admin.py" ""))

(defun fk/django-search-permissions ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "permissions.py" "^class "))

(defun fk/django-search-mixins ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "mixins.py" "^class "))

(defun fk/django-search-urls ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "*.py" "path\\( "))

(defun jak/all-classes ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "*.*" "^class "))

(defun jak/all-methods ()
  (interactive)
  (fk/helm-rg-dwim-with-glob "*.*" "def "))

(bind-keys*
 :map django
 ("m" . fk/django-search-models)
 ("v" . fk/django-search-views)
 ("f" . fk/django-search-forms)
 ("s" . fk/django-search-serializers)
 ("t" . fk/django-search-tests)
 ("S" . fk/django-search-settings)
 ("a" . fk/django-search-admins)
 ("p" . fk/django-search-permissions)
 ("x" . fk/django-search-mixins)
 ("u" . fk/django-search-urls)
 ;; generic
 ("c" . jak/all-classes)
 ("b" . jak/all-methods)
 )


;; Utility functions for Django

(defun fk/django-copy-path-of-test-at-point ()
  "Add path of the test at point to kill-ring. Returns the path."
  (interactive)
  (require 'which-func)
  (let* ((defuns (seq-subseq (split-string (which-function) "\\.") 0 2))
         (class (car defuns))
         (func (let ((f (-second-item defuns))) (and f (string-match "^test" f) f)))
         (module (fk/django-get-module))
         (path (concat module (and module class ".") class (and class func ".") func)))
    (kill-new path)))

(defun fk/django-get-module ()
  "pony-get-module originally."
  (let* ((root (projectile-project-root))
         (path (file-name-sans-extension (or buffer-file-name (expand-file-name default-directory)))))
    (when (string-match (projectile-project-root) path)
      (let ((path-to-class (substring path (match-end 0))))
        (mapconcat 'identity (split-string path-to-class "/") ".")))))

;; (bind-keys*
;;  :map django
;;  ("c" . fk/django-copy-path-of-test-at-point))


(provide 'init-django)
