;;; Web to RSS Parser
;;; by tux. [ http://tuxproject.de ]
;;;
;;; Licensed under the terms of the WTFPL.
;;; http://wtfpl.net/txt/copying/


(load "~/quicklisp/setup.lisp")

(ql:quickload '(:datafly        ;; for database access

                ;; WEB SERVER:
                :hunchentoot    ;; for providing a web server
                :cl-who         ;; for building the HTML output
                :parenscript    ;; for the avoiding of the horrible JS syntax
                :smackjack      ;; for AJAX requests
                :lass           ;; for building the (S)CSS styles

                ;; WEB/RSS CLIENT:
                :cl-ppcre       ;; for regex
                :dexador        ;; for using the web
                :clss           ;; for CSS selecting
                :plump          ;; for parsing XML/DOM
                :plump-sexp     ;; for converting DOM to S-exps
                :local-time     ;; for time conversion
                :xml-emitter)   ;; for creating the RSS files
              :silent t)

(defpackage #:rssparser
  (:use :cl :sxql :datafly :xml-emitter :local-time :hunchentoot :cl-who :lass :smackjack :sb-int)
  (:import-from :parenscript :ps :chain))
