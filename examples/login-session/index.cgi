#!/bin/sh
#-*- scheme -*-
export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:`pwd`/../../install/share/guile/site;
exec ${GUILE-guile} -s $0
!#
;;; login-session/index.cgi

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

(use-modules
 (sxml simple)
 ((www cgi) #:select (cgi:values))
 ((www server-utils answer) #:select (mouthpiece)))

;; FIXME: Should do ‘cgi:init’ somewhere, maybe here. --ttn

;; The following uses filesystem-based session management.
(use-modules (www session fs))
(define session (session:fs "/tmp"))

;; As an alternative - uncomment the following for
;; database-based session management.
; (use-modules (dbi dbi)
; 	     (www session db))
; 
; (define dbh (dbi-open "mysql" "::test:socket:/var/run/mysqld/mysqld.sock"))
; (define session
;   (session:db
;     (lambda (query)
;       (dbi-query dbh query)
;       (dbi-get_row dbh))
;     "sessions"))

;; See if there is a login request.
(define login-status-message
  (if (cgi:values "uname")
      (let ((uname  (car (cgi:values "uname")))
            (passwd (car (cgi:values "password"))))
        (if (equal? passwd "123456")
            (and (session-set-user! session uname)
                 "")
            '(p (@ (class "warning")) "** Invalid login **")))
      ""))

(define welcome-line
  (let ((user (session-get-user session)))
    (if user
        `("Welcome " ,user ". "
          (a (@ (href "login.html")) "Log in as a different user"))
        '("You are not logged in. " (a (@ (href "login.html")) "Log in")))))

(define content
  `(html
    (head (title "(www session fs) login demo"))
    (body
     ,login-status-message
     ,welcome-line)))

(define mp (mouthpiece (current-output-port)))
(mp #:add-header #f (session-propagate session))
(mp #:add-content (with-output-to-string
                    (lambda ()
                      (sxml->xml content))))
(mp #:set-reply-status:success)
(mp #:send-reply)

;;; login-session/index.cgi ends here
