;;; (www session fs)

;; Copyright (C) 2011 Thien-Thi Nguyen
;; Copyright (C) 2011 gregory benison
;;
;; This file is part of Guile-WWW.
;;
;; Guile-WWW is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; Guile-WWW is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Guile-WWW; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA  02110-1301  USA

;;; Code:

(define-module (www session fs)
  #:export (session:fs)
  #:re-export (session-get-user
               session-set-user!
               session-propagate)
  #:use-module (www session base))

;;; ---------- utilities ----------

(define (rewind port)
  ;; FIXME: ‘file-set-position’ is not available on Guile 1.4.x.
  ;; How about ‘(seek port 0 SEEK_SET)’?  --ttn
  (file-set-position port 0))

;;; ------- filesystem-based session management ----------

;; Resume (or initiate, if none exists) a session using the filesystem for
;; server-side state management.  @var{basedir} is the name of the directory
;; where session files will be stored (e.g., @file{/tmp}).
;;
;; Return a session object suitable as the argument to
;; @code{session-get-user}, @code{session-set-user!}, and
;; @code{session-propagate}.
;;
(define (session:fs basedir)
  (make-session
   (fs:allocate basedir)
   (fs:lookup basedir)
   fs:get-id
   fs:get-token
   fs:get-user
   (fs:set-user! basedir)))

(define (fs:allocate dir)
  ;; A "session object" here consists of a file port.
  (lambda (token)
    ;; FIXME: ‘mkstemp!’ is not available on Guile 1.4.x.
    ;; Also, who is responsible for deleting this file?  --ttn
    (let ((port (mkstemp! (in-vicinity dir "session.XXXXXX"))))
      (write `((token . ,token)) port)
      port)))

(define (fs:lookup session-dir)
  (lambda (session-id)
    (let ((fname (in-vicinity session-dir session-id)))
      (and (file-exists? fname)
           (open-file fname "r+")))))

(define (fs:get-id session)
  (basename (port-filename session)))

(define (--get symbol)
  (lambda (session)
    (rewind session)
    (let ((rv (assoc-ref (read session) symbol)))
      (rewind session)
      rv)))

(define fs:get-token (--get 'token))

(define fs:get-user (--get 'user))

(define (fs:set-user! session-dir)
  (lambda (session user-name)
    (rewind session)
    (let ((contents (assoc-set! (read session) 'user user-name)))
      (rewind session)
      (write contents session)
      (display (make-string 256 #\space) session)
      (rewind session)
      session)))

;;; (www session fs) ends here
