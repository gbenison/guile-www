;;; (www session base)

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

(define-module (www session base)
  #:export (make-session
            session-get-user
            session-set-user!
            session-propagate)
  #:use-module ((www cgi) #:select (cgi:init
                                    cgi:cookie))
  #:use-module ((www server-utils cookies) #:select (rfc2109-set-cookie-string))
  #:use-module ((srfi srfi-13) #:select (string-tokenize))
  #:use-module ((srfi srfi-14) #:select (char-set-complement
                                         char-set))
  #:use-module ((ice-9 receive) #:select (receive)))

;;; ---------- utilities ----------

;; Return a random number from the kernel, of @var{n} bytes.
;; FIXME: We just need a good, well-seeded source of random bits.
;; This works but is platform-specific.
;;
(define random-bytes
  (let ((port (open-input-file "/dev/urandom")))
    ;; random-bytes
    (lambda (n)
      (let loop ((n n) (rv 0))
        (if (zero? n)
            rv
            (loop (1- n) (+ (char->integer (read-char port))
                            (ash rv 8))))))))

(define (fs s . args)
  (apply simple-format #f s args))

(define (->string x)
  (if (string? x)
      x
      (fs "~A" x)))

(define (->non-empty-string x)
  (and x (let ((str (fs "~A" x)))
           (and (not (string-null? str))
                str))))

(define (strequal? a b)
  (string=? (->string a)
            (->string b)))

(define cookie->key
  (let ((on-colon (char-set-complement (char-set #\:))))
    ;; cookie->key
    (lambda (cookie)
      (apply values (string-tokenize cookie on-colon)))))

(define (key->cookie id token)
  (fs "~A:~A" id token))

;; {session abstraction}
;;
;; Each session handler must provide a mechanism to allocate a new
;; session with a unique ID.  If the handler allocates IDs in a
;; predictable way (for example, using successive integers), this
;; presents a security risk, because one could hijack a session by
;; obtaining a session ID and then guessing at closely related IDs.
;; To help guard against this, the "token" mechanism is provided: the
;; @code{(www session base)} module generates a random token for every
;; newly allocated session, and the handler must provide a way of
;; associating the token with the session.  The combination of session
;; ID + token is hard to guess correctly, even if session IDs are
;; closely related.  (Note this mechanism does not address the
;; ``cookie-theft'' class of attacks.)
;;
;; Each handler must also provide a way to associate a session
;; with a user name via @code{(@var{method}:get-user)} and
;; @code{(@var{method}:set-user!)}.
;; Note that authentication is up to the calling application:
;; before calling (session 'set-user!), the program should verify
;; that the incoming HTTP request legitimately originated from that
;; user.

(define (make-session
         allocate   ;; token      -> session
         lookup     ;; session-id -> session
         get-id     ;; session    -> session-id
         get-token  ;; session    -> token
         get-user   ;; session    -> user-name
         set-user!) ;; session user-name -> user-name

  (define (allocate-session!)
    (allocate (random-bytes 8)))

  (define (validate session id token)
    (and session
         (or (and (strequal? id    (get-id session))
                  (strequal? token (get-token session)))
             (throw 'invalid-session id token))
         session))

  ;; FIXME: Perhaps this should be left to caller.  --ttn
  (cgi:init)
  (let* ((cookie (cgi:cookie "sessionID"))
         (session (or (and cookie
                           (receive (key token)
                               (cookie->key cookie)
                             (validate (lookup key) key token)))
                      (allocate-session!))))
    ;; rv
    (lambda (request)
      (case request
        ((propagate)
         (rfc2109-set-cookie-string 'sessionID
                                    (key->cookie (get-id session)
                                                 (get-token session))))
        ((set-user!)
         (lambda (user-name)
           (set! session (set-user! session user-name))
           user-name))
        ((get-user)
         (->non-empty-string (get-user session)))
        (else
         (throw 'invalid-request request))))))

;; Generate an HTTP header ensuring that @var{session} will be
;; propagated in subsequent requests.
;;
(define (session-propagate session)
  (session 'propagate))

;; Associate user @var{name} with a session.  @var{session} is a session
;; object returned by, for example, @code{session:fs} (or any other
;; constructor provided by a session handler).
;;
(define (session-set-user! session name)
  ((session 'set-user!) name))

;; Return the user name previously provided to @code{session-set-user!}.
;;
(define (session-get-user session)
  (session 'get-user))

;;; (www session base) ends here
