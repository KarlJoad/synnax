(define-module (synnax services home utils)
  #:export (home-service-log-file-path))

(define (home-service-log-file-path daemon-name)
  "Given DAEMON-NAME as a string, return file path for the daemon's log file as
a string. @env{XDG_LOG_HOME} is attempted first, and if it is not set, then the
same path is constructed relative to @env{HOME}."
  (format #f "~a/~a.log"
          (or (getenv "XDG_LOG_HOME")
              (format #f "~a/.local/var/log" (getenv "HOME")))
          daemon-name))
