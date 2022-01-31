;;; Web to RSS Parser
;;; by tux0r [ https://tuxproject.de ]
;;;
;;; The contents of this file are subject to the terms of the
;;; Common Development and Distribution License, Version 1.1 only
;;; (the "License").  You may not use this file except in compliance
;;; with the License.
;;;
;;; See the file LICENSE in this distribution for details.
;;; A copy of the CDDL is also available via the Internet at
;;; https://spdx.org/licenses/CDDL-1.1.html
;;;
;;; When distributing Covered Code, include this CDDL HEADER in each
;;; file and include the contents of the LICENSE file from this
;;; distribution.


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
