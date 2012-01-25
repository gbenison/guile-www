;;; (www session db)

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

(define-module (www session db)
  #:export (session:db)
  #:re-export (session-get-user
               session-set-user!
               session-propagate)
  #:use-module (www session base)
  #:use-module ((srfi srfi-1) #:select (filter))
  #:use-module ((srfi srfi-13) #:select (string-join)))

;;; ------ utilities -------

(define (fs s . args)
  (apply simple-format #f s args))

(define (sql-fmt x)
  (object->string x))

;; Remove entries matching @var{keys} from @var{alist}.

(define (alist-scrub alist keys)
  (filter (lambda (entry)
            (not (member (car entry) keys)))
          alist))

;;; ---------- database-based session management ----------

(define (session-from-db query-op table-name session-id)
  (query-op
   (fs "SELECT * from ~A WHERE session_id=~A"
       table-name
       session-id)))

(define (db:allocate-session query-op table-name)
  (lambda (token)
    (query-op (fs "INSERT INTO ~A (token) values (~A)"
		  table-name
		  token))
    (let ((session-id (cdar (query-op "SELECT last_insert_id ()"))))
      (session-from-db query-op table-name session-id))))

(define (db:sync-database query-op table-name session)
  (let* ((session-id (assoc-ref session "session_id"))
         (session* (alist-scrub session '("session_id"
                                          "token")))
         ;; FIXME: Handle empty string case better.
         (fields (map (lambda (x)
                        (fs "~A=~A" (car x) (sql-fmt (cdr x))))
                      session*)))
    (query-op
     (fs "UPDATE ~A set ~A WHERE session_id=~A"
	 table-name
	 (string-join fields ", ")
	 session-id))))

;; Resume (or initiate, if none exists) a session using a database for
;; server-side state management.  @var{query-op} is a function
;; @code{lambda(query)} where @var{query} is an SQL statement to pass
;; to a database, and where the return value is an alist of the form
;; ((column_name . value) ... ) containing the first row of the result
;; of the query.
;; @var{table-name} is the name of a table in the database which must
;; contain columns @code{session_id} (of type @code{int}) and @code{token}
;; (sufficient to hold 8-byte integers).
;;
;; Return a session object suitable as the argument to
;; @code{session-get-user}, @code{session-set-user!}, and
;; @code{session-propagate}.
;;
(define (session:db query-op table-name)

  (define (lookup session-id)
    (session-from-db query-op table-name session-id))

  (define (get-id session)
    (assoc-ref session "session_id"))

  (define (get-token session)
    (assoc-ref session "token"))

  (define (get-user session)
    (assoc-ref session "user_name"))

  (define (set-user! session user-name)
    (let ((new-session (assoc-set! session "user_name" user-name)))
      (db:sync-database query-op table-name new-session)
      new-session))

  ;; Do it!
  (make-session (db:allocate-session query-op table-name)
                lookup
                get-id
                get-token
                get-user
                set-user!))

;;; (www session db) ends here
