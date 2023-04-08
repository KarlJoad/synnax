(define-module (synnax services utils)
  #:export (get-log-file-path))

(define (get-log-file-path daemon-name)
  "Given DAEMON-NAME, return file path for the daemon's log file as a string."
  (format #f "~a/~a.log"
          (or (getenv "XDG_LOG_HOME")
              (format #f "~a/.local/var/log" (getenv "HOME")))
          daemon-name))
