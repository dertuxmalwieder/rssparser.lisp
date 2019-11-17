;;; Web to RSS Parser
;;; by tux. [ http://tuxproject.de ]
;;;
;;; Licensed under the terms of the WTFPL.
;;; http://wtfpl.net/txt/copying/


(in-package #:rssparser)


(defun get-args ()
  "Returns the list of command-line parameters. Don't touch this."
  (or
   #+sbcl sb-ext:*posix-argv*
   #+ccl *command-line-argument-list*
   nil))


;;; -------- SET YOUR OPTIONS BELOW --------


;;; Store the script's command mode in a parameter
;;; so we won't have to fetch it again and again.
;;; Also store the arguments.
(defparameter *script-mode* (cadr (get-args)))
(defparameter *script-arguments* (cddr (get-args)))


;;; Store the database file name and the folder
;;; for our feed files so we can easily change
;;; them later if we want to ...
(defconstant +database-file+ "/home/tux/hg/rssparser.lisp/feeds.db")
(defconstant +feed-folder+ "/home/tux/hg/rssparser.lisp/feeds/")


;;; Store the maximum number of entries per feed.
(defconstant +max-items-per-feed+ 50)


;;; Set up a feed cleaner: If this constant is not NIL,
;;; the feed parser will remove old entries from the
;;; database automatically.
(defconstant +feed-cleanup+ t)


;;; If a website is dead, it could automatically be
;;; removed from the feed list.
(defconstant +remove-dead-feeds+ t)


;; By default, the webserver listens on this port.
(defconstant +webserver-port+ 5000)
