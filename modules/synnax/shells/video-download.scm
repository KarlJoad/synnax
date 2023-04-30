(define-module (synnax shells video-download)
  #:use-module (guix profiles)
  #:use-module (gnu packages video)
  #:use-module (gnu packages attr))

(packages->manifest
 (list yt-dlp
       ffmpeg
       attr))

;; I typically use the following command to download a video with its audio in
;; the highest quality for each:
;; yt-dlp --concurrent-fragments 50 --restrict-filenames \
;; --write-description --write-info-json --write-subs \
;; --embed-subs --embed-thumbnail --embed-metadata --embed-info-json \
;; --xattrs
