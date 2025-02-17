(define-module (synnax packages web)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages web))

(define-public nginx-headers-more-module
  (package
    (inherit nginx)
    (name "nginx-headers-more-module")
    (version "0.38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openresty/headers-more-nginx-module")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbgwzkpni616nawjkrq0xid60wdgab3vciy7nr966ac6rjyiliy"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx))
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules") ;Only build this module not all of nginx.
           ,@(package-arguments nginx))
       ((#:configure-flags flags)
        #~(cons "--add-dynamic-module=." #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda _
                (begin
                  ;; The nginx source code is needed to compile the module.
                  (format #t "decompressing nginx source code~%")
                  (invoke "tar" "xvf" #$(this-package-input "nginx-sources")
                          ;; This package's LICENSE file would be
                          ;; overwritten with the one from nginx when
                          ;; unpacking the nginx source, so rename the nginx
                          ;; one when unpacking.
                          "--transform=s,/LICENSE$,/LICENSE.nginx,"
                          "--strip-components=1"))))
            (replace 'install
              (lambda _
                (let ((modules-dir (string-append #$output
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_http_headers_more_filter_module.so"
                                modules-dir))))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (home-page "https://github.com/openresty/headers-more-nginx-module")
    (synopsis "Set, add, and clear input and output headers in NGINX http servers")
    (description "Allows adding, setting, or clearing any output or input header
specified.

This is an enhanced version of the standard headers module because it provides
more utilities like resetting or clearing \"builtin headers\" like @code{Content-Type},
@code{Content-Length}, and @code{Server}.

It also allows you to specify an optional HTTP status code criteria using the
@code{-s} option and an optional content type criteria using the @code{-t}
option while modifying the output headers with the more_set_headers and
more_clear_headers directives.")
    (license license:bsd-2)))

nginx-headers-more-module
