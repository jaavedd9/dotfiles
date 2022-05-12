(prodigy-define-service
  :name "OverGrive"
  :command "python3 /opt/thefanclub/overgrive/overgrive"
  ;;  :args '("-m" "SimpleHTTPServer" "6001")
  :tags '(clound)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


;; django
(prodigy-define-service
  :name "Kombare 2019"
  :command (format "%s/kombare_2019_env/bin/python" (getenv "VENVS"))
  ;;  :args '("-m" "SimpleHTTPServer" "6001")
  :args '("manage.py" "runserver" "8009")
  :cwd  (format "%s/repo_kombare_2019/kombare" (getenv "JAK"))
  :tags '(personal)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)



(prodigy-define-service
  :name "DSR incentives"
  ;;  :command (format "%s/dsr_incentives_env/bin/python" (getenv "VENVS"))
  :command (format "%s/dsr_incentives_env/bin/uwsgi --http :8000 \
                   --chdir  %s/dsr-incentives/dsr_incentives \
                    -w dsr_incentives.wsgi --workers 10"
                   (getenv "VENVS")
                   (getenv "KFUPM")
                   )
  ;;  :args '("-m" "SimpleHTTPServer" "6001")
  ;;:args '("manage.py" "runserver" "8000")
  ;; :cwd  (format "%s/dsr-incentives/dsr_incentives" (getenv "KFUPM"))
  :tags '(work)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'init-prodigy)
