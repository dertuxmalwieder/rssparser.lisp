#!/opt/local/bin/sbcl --script

;;; Web to RSS Parser
;;; by tux. [ https://tuxproject.de ]
;;;
;;; Licensed under the terms of the WTFPL.
;;; http://wtfpl.net/txt/copying/


;;; valid syntax:
;;;  * ./rssparser add <Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]
;;;  * ./rssparser.lisp del(ete) <ID>
;;;  * ./rssparser.lisp list
;;;  * ./rssparser.lisp export <ID>
;;;  * ./rssparser.lisp webserver
;;;  * ./rssparser.lisp parse


;;; SETUP

(load (merge-pathnames "package.lisp" *load-truename*))
(load (merge-pathnames "config.lisp" *load-truename*))

(in-package #:rssparser)



;;; HELPER FUNCTIONS


(defun show-syntax ()
  "Prints the command-line syntax for the RSS parser."
  (format t "Syntax:~% * rssparser.lisp add <Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]~% * rssparser.lisp delete <ID>~% * rssparser.lisp list~% * rssparser.lisp export <ID>~% * rssparser.lisp webserver~%~%If you're a bot:~% * rssparser.lisp parse"))


(defun print-feed-list (list)
  "Makes the feed list look nicer."
  (loop for feed in list do
       (let ((id-pair (first feed))
             (title-pair (second feed))
             (url-pair (third feed))
             (lastsuccess-pair (fourth feed)))

         (format t "~%ID: ~a  Title:        ~a~%        URL:          ~a~%        Last success: ~a~%"
                 (cdr id-pair)
                 (cdr title-pair)
                 (cdr url-pair)
                 (if (cdr lastsuccess-pair)
                     (timestamp-to-rssdate (cdr lastsuccess-pair))
                     ;; New feeds don't have a "last success" yet:
                     "-")))))


(clss:define-pseudo-selector external-link (node)
  "CLSS pseudo-selector for external links, shamelessly borrowed from the CLSS manual."
  (let ((href (plump:attribute node "href")))
    (and href (cl-ppcre:scan "^(http|https)://" href))))


(defun extract-urls (node)
  "Tries to find an external href in <node>. Returns a list of all found URLs."
  (let ((link-list (clss:select "a:external-link" node)))
    (loop for found-link being the elements of link-list
       collect (list (plump:attribute found-link "href")))))


(defun timestamp-to-rssdate (timestamp)
  "Returns a readable date from a given timestamp."
  (format-timestring nil
                     (unix-to-timestamp timestamp)
                     :format +rfc-1123-format+))



;;; WEB SERVER


;; Define a parameter to determine if a method has been called via web interface.
(defparameter *is-web* nil)

;; Make Parenscript coexist with CL-WHO
(setf parenscript:*js-string-delimiter* #\")

;; Set the output doctype
(setf (html-mode) :html5)

;; Instantiate an AJAX listener for Smackjack
(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

;; Define the routes for Hunchentoot
(setq *dispatch-table*
      (list 'dispatch-easy-handlers
            (create-ajax-dispatcher *ajax-processor*)))


;; Define the HTML output when accessing the web UI
(define-easy-handler (rssweb :uri "/" :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (with-html-output-to-string
      (s nil :indent t)

    (:html
     (:head
      (:title "RSSParser Web Control Center")
      (str (generate-prologue *ajax-processor*))
      (:style :type "text/css"
              (str (compile-and-write
                    '(body
                      :margin 0
                      :padding 0
                      :font-family monospace
                      :font-size 12px)
                    '(h1
                      :margin 5px)
                    '(input
                      :width 335px)
                    '(div#controls
                      :display block
                      :width "calc (100% - 20px)"
                      :padding 10px
                      :background-color lightgrey)
                    '(div#addfeed
                      :display none
                      :position absolute
                      :font-family sans-serif
                      :top 5px
                      :right 5px
                      :border 1px solid darkgray
                      :padding 8px
                      :background-color white
                      (b
                       :margin-bottom 6px)
                      (div
                       :display inline-block)
                      (div.label
                       :width 100px)
                      (div.explanation
                       :width 440px
                       :margin-bottom 4px
                       :font-size 11px))
                    '(table
                      :margin 10px
                      :border-collapse collapse
                      (tr
                       (td
                        :border 1px solid darkgray
                        :padding 4px)
                       (td.centered
                        :text-align center)))
                    '(div#footer
                      :position fixed
                      :width 100%
                      :padding 5px
                      :display block
                      :bottom 0px
                      :left 0px))))
      (:script :type "text/javascript"
               (str (ps
                     (defun print-response-callback (response)
                       (alert response))

                     (defun add-feed ()
                       (setf (chain document (get-element-by-id "addfeed") style display) "inline-block"))

                     (defun add-this-feed ()
                       (let ((title (chain document (get-element-by-id "TitleToAdd") value))
                             (url (chain document (get-element-by-id "URLToAdd") value))
                             (entry-selector (chain document (get-element-by-id "EntrySelectorToAdd") value))
                             (title-selector (chain document (get-element-by-id "TitleSelectorToAdd") value))
                             (content-selector (chain document (get-element-by-id "ContentSelectorToAdd") value)))
                         (if (or (not title) (not url) (not entry-selector) (not title-selector))
                             (alert "You should enter valid feed data before we can process your request.")
                             (progn
                               (chain smackjack (ajax-add-feed (list title url entry-selector title-selector content-selector) print-response-callback))
                               (close-add-feed-dlg)
                               (request-table)))))

                     (defun close-add-feed-dlg ()
                       (loop for inputid in (list "TitleToAdd" "URLToAdd" "EntrySelectorToAdd" "TitleSelectorToAdd" "ContentSelectorToAdd") do
                            (setf (chain document (get-element-by-id inputid) value) nil))
                       (setf (chain document (get-element-by-id "addfeed") style display) "none"))

                     (defun parse-all ()
                       (chain smackjack (ajax-parse-feeds print-response-callback))
                       (request-table))

                     (defun update-table (htmltext)
                       (setf (chain document (get-element-by-id "ajaxtable") inner-h-t-m-l) htmltext))

                     (defun request-table ()
                       (chain smackjack (htmltable update-table))
                       nil)

                     (defun zap-feed (feedid)
                       (chain smackjack (ajax-delete-feed feedid))
                       (request-table)
                       nil)))))
     (:body
      (:h1 "RSSParser Web Control Center")
      (:div :id "controls"
            (:button :type "button"
                     :onclick (ps (parse-all))
                     "Parse All Feeds")
            (:button :type "button"
                     :onclick (ps (add-feed))
                     "Add a Feed"))
      (:div :id "addfeed"
            (:div :class "explanation" "Please type the title of the feed you'd like to add.") (:br)
            (:div :class "label" "Title:") (:input :type "text" :id "TitleToAdd" :placeholder "e.g. KiTTY") (:br) (:br)
            (:div :class "explanation" "Please type the URL of the website you want to make an RSS feed of.") (:br)
            (:div :class "label" "URL:") (:input :type "text" :id "URLToAdd" :placeholder "e.g. http://www.9bis.net/kitty/?action=news&zone=en") (:br) (:br)
            (:b "CSS Selectors:") (:a :href "http://www.w3schools.com/cssref/trysel.asp" :target "_blank" "[?]") (:br)
            (:div :class "explanation" (str (concatenate 'string "Please type the single entry selector of the feed. For a blog, this is usually a DIV named \"article\" or similar. RSSParser.lisp will use this selector as the container selector for the following elements."))) (:br)
            (:div :class "label" "Selector:") (:input :type "text" :id "EntrySelectorToAdd" :placeholder "e.g. .news") (:br) (:br)
            (:div :class "explanation" "Please type the title selector of the feed. This is usually the headline inside the entry selector.") (:br)
            (:div :class "label" "Selector:") (:input :type "text" :id "TitleSelectorToAdd" :placeholder "e.g. h1") (:br) (:br)
            (:div :class "explanation" (str (concatenate 'string (htm (:em "(Optional)")) " Please type the content selector of the feed. This is usually the text part of the entry selector. If this is left blank, RSSParser.lisp will use the complete entry selector for the contents of your feed items."))) (:br)
            (:div :class "label" "Selector:") (:input :type "text" :id "ContentSelectorToAdd" :placeholder "e.g. \"\"") (:br) (:br)
            (:button :type "button"
                     :onclick (ps (add-this-feed))
                     "Add this feed")
            (:button :type "button"
                     :onclick (ps (close-add-feed-dlg))
                     "Cancel"))
      (:div :id "ajaxtable")
      (:div :id "footer"
            (:a :href "http://bitbucket.org/tux_/rssparser.lisp" :target "_blank" "Powered by RSSParser.lisp"))
      (:script :type "text/javascript"
               (str (ps (request-table))))))))


;; Define an AJAX handler which returns the updated feeds list (as a table)
(defun-ajax htmltable ()
  (*ajax-processor* :callback-data :response-text)
  (with-html-output-to-string
      (s nil :indent t)
    (:table
     (:tr
      (:th "ID")
      (:th "Title")
      (:th "Original URL")
      (:th "Last success")
      (:th "Delete"))
     (loop for feed in (list-all-feeds) do
          (let ((id-pair (first feed))
                (title-pair (second feed))
                (url-pair (third feed))
                (lastsuccess-pair (fourth feed)))
            (htm (:tr
                  (:td (str (cdr id-pair)))
                  (:td (str (cdr title-pair)))
                  (:td
                   (:a :href (cdr url-pair) :target "_blank" (str (cdr url-pair))))
                  (:td (if (cdr lastsuccess-pair)
                           (str (timestamp-to-rssdate (cdr lastsuccess-pair)))
                           "-"))
                  (:td :class "centered"
                       (:button :type "button"
                                :title (concatenate 'string "Delete the " (write-to-string (cdr title-pair)) " feed")
                                :onclick (concatenate 'string "javascript:zapFeed(\"" (write-to-string (cdr id-pair)) "\")")
                                "x")))))))))


;;; MAIN APPLICATION


;; Force the charset to be Unicode
(setf sb-impl::*default-external-format* :UTF-8)

(connect-toplevel :sqlite3 :database-name +database-file+)

(defun add-new-feed (params)
  "Adds a new feed to the database."
  ;; Params: Feed title, URL, entry selector, title sel., [ content sel. ]
  (if
   (and
    ;; URL given?
    (cl-ppcre:scan "^https?://" (second params))
    ;; Are the necessary params given at all?
    (>= (list-length params) 4)
    (<= (list-length params) 5))
   (progn
     (if
      (and
       ;; Everything but the title must not be a number.
       (not (numberp (parse-integer (second params) :junk-allowed t)))
       (not (numberp (parse-integer (third params) :junk-allowed t)))
       (not (numberp (parse-integer (fourth params) :junk-allowed t))))
      (progn
        (let ((content
               (if (fifth params)
                   (princ-to-string (fifth params))
                   "")))
          (execute
           ;; all arguments are set (probably even correctly).
           (insert-into :feeds
                        (set= :feedtitle (princ-to-string (first params))
                              :url (princ-to-string (second params))
                              :entryselector (princ-to-string (third params))
                              :titleselector (princ-to-string (fourth params))
                              :contentselector content))))
        t)
      nil))
   (progn
     ;; invalid number of arguments
     (show-syntax)
     nil)))


;; AJAX handler for feed addition:
(defun-ajax ajax-add-feed (params)
  (*ajax-processor* :callback-data :response-text)
  (setf *is-web* t)
  (if
   (add-new-feed params)
   "We successfully added your feed. :-)"
   "Sorry, something went wrong. :-(")
  (setf *is-web* nil))


(defun delete-feed (id)
  "Deletes a feed and all of its references."
  (if
   (and
    (eql 1 (list-length id))
    (numberp (parse-integer (car id) :junk-allowed t)))
   (let ((id-to-delete (car id)))
     ;; The <id-to-delete> is the number of the feed to delete.
     (if
      (retrieve-one (select :id (from :feeds) (where (:= :id id-to-delete))))
      (progn
        ;; This feed exists. Remove it.
        (execute (delete-from :feeds
                              (where (:= :id id-to-delete))))
        (execute (delete-from :entries
                              (where (:= :feedid id-to-delete))))
        ;; Now that the database entry is gone, we don't
        ;; need the XML file anymore.
        (let
            ;; The file is probably "feeds/feed<ID>.xml".
            ((feed-file (probe-file (concatenate 'string +feed-folder+ "feed" id-to-delete ".xml"))))
          (if feed-file
              ;; We have an XML file. Yet.
              (delete-file feed-file)))
        t)
      ;; No such feed in the database
      nil))
   (progn
     ;; invalid number of arguments or ID is NaN
     (show-syntax)
     nil)))


(defun export-feed (id)
  "Exports a feed's INSERT command for transferring it into another database."
  (if
   (and
    (eql 1 (list-length id))
    (numberp (parse-integer (car id) :junk-allowed t)))
   (progn
     (let* ((id-to-export (car id))
            (feed (retrieve-one (select
                                 (:feedtitle :url :entryselector :titleselector :contentselector)
                                 (from :feeds)
                                 (where (:= :id id-to-export))))))

       ;; The <id-to-export> is the number of the feed to export.
       (if feed
           (progn
             ;; This feed exists. Export it.
             (let
                 ((feed-title (second feed))
                  (feed-url (fourth feed))
                  (feed-entries (sixth feed))
                  (feed-entry-titles (eighth feed))
                  (feed-entry-contents (tenth feed)))
               (concatenate 'string
			    "INSERT INTO feeds ('feedtitle', 'url', 'entryselector', 'titleselector', 'contentselector') "
			    "VALUES ('" feed-title "', '" feed-url "', '" feed-entries "', '" feed-entry-titles "', '" feed-entry-contents "');")))
           ;; No such feed in the database
           nil)))
   (progn
     ;; invalid number of arguments or ID is NaN
     (show-syntax)
     nil)))


;; AJAX handler for feed deletion:
(defun-ajax ajax-delete-feed (feed-id)
  (*ajax-processor* :callback-data :response-text)
  (if
   (delete-feed (list feed-id))
   (concatenate 'string "Deleted feed " feed-id)
   "Failed to delete the feed, sorry."))


(defun list-all-feeds ()
  "Lists all known feeds."
  (retrieve-all
   (select (:id :feedtitle :url :lastsuccess)
           (from :feeds))
   :as 'trivial-types:association-list))


(defun parse-all-feeds ()
  "Loops over the known feeds and generates the XML files."
  (loop for feed in
       (retrieve-all
        (select (:feedtitle :url :entryselector :titleselector :contentselector :id)
                (from :feeds)))
     do
       (let
           ((feed-title (second feed))
            (feed-url (fourth feed))
            (feed-entries (sixth feed))
            (feed-entry-titles (eighth feed))
            (feed-entry-contents (tenth feed))
            (feed-id (last feed)))
         (handler-case
             (let ((the-dom (plump:parse (dex:get feed-url))))
               ;; the-dom is the DOM of our feed URL now.
               ;; We can process it normally.
               (let ((box-elements (clss:select feed-entries the-dom))
                     (feed-file (concatenate 'string +feed-folder+ "feed" (princ-to-string (car feed-id)) ".xml")))
                 (with-open-file (stream feed-file
                                         :direction :output
                                         :if-exists :overwrite
                                         :if-does-not-exist :create)
                   (with-rss2 (stream :encoding "utf-8")
                     (rss-channel-header feed-title feed-url)
                     (loop for feed-entry being the elements of box-elements do
                          (let
                              ((contents
                                ;; If the feed has an "empty" contents selector, take the
                                ;; whole box element's content as the entry contents
                                (if (string= feed-entry-contents "")
                                    (list feed-entry)
                                    (clss:select feed-entry-contents feed-entry)))

                               ;; The title elements are always stored with the feed.
                               (titles (clss:select feed-entry-titles feed-entry)))

                            ;; Loop over all titles/contents and add an RSS item for each of them
                            (loop for single-title being the elements of titles
                               for single-contents being the elements of contents
                               do
                                 (let
                                     ;; Grab the plain text from the title:
                                     ((our-title (plump:text single-title))
                                      ;; Try to find the entry URL:
                                      (our-url (extract-urls single-title))
                                      ;; Serialize the contents to NIL (return a string):
                                      (our-contents (plump:serialize (plump-sexp:parse (plump-sexp:serialize single-contents)) nil)))
                                   ;; Write the data to the database:
                                   (when
                                       (and
                                        our-title
                                        our-contents

                                        ;; Only do it if we don't have this item yet :-)
                                        (not (retrieve-all
                                              (select :*
                                                      (from :entries)
                                                      (where (:and
                                                              (:= :feedid (car feed-id))
                                                              (:= :title our-title)))))))

                                     (execute
                                      (insert-into :entries
                                                   (set= :feedid (princ-to-string (car feed-id))
                                                         :title (princ-to-string our-title)
                                                         :contents (princ-to-string our-contents)
                                                         :url (if our-url (princ-to-string (caar our-url)) feed-url)
                                                         :timestamp (timestamp-to-unix (now)))))))

                               ;; Update the success timestamp in the database.
                                 (execute
                                  (update :feeds
                                          (set= :lastsuccess (timestamp-to-unix (now)))
                                          (where (:= :id (car feed-id))))))))

                     ;; Clean up if requested.
                     (when +feed-cleanup+
                       (execute
                        (delete-from :entries
                                     (where (:in :id
                                                 (select :id
                                                         (from :entries)
                                                         (where (:= :feedid (car feed-id)))
                                                         (order-by (:desc :timestamp))
                                                         (limit -1)
                                                         (offset (* 2 +max-items-per-feed+))))))))

                     ;; We have a nicely filled database now.
                     ;; Grab the newest entries and put them into our feed.
                     (loop for item-list in
                          (retrieve-all
                           (select (:title :contents :url :timestamp)
                                   (from :entries)
                                   (where (:= :feedid (car feed-id)))
                                   (order-by (:asc :timestamp))
                                   (limit +max-items-per-feed+))
                           :as 'trivial-types:association-list)
                        do
                        ;; Our list should look as follows now:
                        ;; ((TITLE) (CONTENTS) (URL) (TIMESTAMP))
                          (let ((this-title (cdr (first item-list)))
                                (this-contents (cdr (second item-list)))
                                (this-url (cdr (third item-list)))
                                (this-timestamp (cdr (fourth item-list))))

                            ;; Write each fetched item to the RSS file.
                            (rss-item (princ-to-string this-title)
                                      :link this-url
                                      :description (princ-to-string this-contents)
                                      :pubDate (princ-to-string (timestamp-to-rssdate this-timestamp)))))))))

           (simple-file-error ()
             ;; The feed folder is not writeable.
             (unless *is-web*
               (format t "Please fix the access rights for ~a for this script to work.~%" +feed-folder+))
             (return nil))

           (simple-stream-error ()
             ;; Connection reset by peer.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                   "temporarily unavailable. We'll try later.")))

           (sb-bsd-sockets:connection-refused-error ()
             ;; Connection refused.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                   "temporarily unavailable. We'll try later.")))

           (sb-bsd-sockets:interrupted-error ()
             ;; Connection interrupted.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                   "temporarily unavailable. We'll try later.")))

           (dex:http-request-service-unavailable ()
             ;; Temporary website error. Retry later.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                   "temporarily unavailable. We'll try later.")))

           (dex:http-request-forbidden ()
             ;; Probably also temporary website error. Retry later.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                   "temporarily unavailable. We'll try later.")))

           (dex:http-request-failed (e)
             ;; Page not found. Assume it is gone.
             (if +remove-dead-feeds+
                 (progn
                   ;; Remove the page from the feeds.
                   (unless *is-web*
                     (format t (concatenate 'string
                                            "~%Feed " (prin1-to-string (car feed-id)) " seems to have a broken website: "
                                            feed-url " could not be reached (" e "). We'll better remove it.")))
                   (force-output nil)
                   (delete-feed (list (write-to-string (car feed-id)))))
                 (progn
                   ;; Display a warning.
                   (unless *is-web*
                     (format t (concatenate 'string
                                            "~%Feed " (prin1-to-string (car feed-id)) " seems to have a broken website: "
                                            feed-url " could not be reached (" e ").")))
                   (force-output nil)))))))

  (setf *is-web* nil))


;; AJAX handler for feed parsing:
(defun-ajax ajax-parse-feeds ()
  (*ajax-processor* :callback-data :response-text)
  (setf *is-web* t)
  (parse-all-feeds)
  "Good news: We just refreshed all of your generated RSS feeds. :-)")


(defun start-webserver ()
  "Runs the RSS parser's built-in web server."
  (format t "Starting the web server on port ~d. Press Ctrl+C to quit.~%~%" +webserver-port+)
  (force-output nil)
  (defparameter *server*
    (start (make-instance 'easy-acceptor :port +webserver-port+))))


(defun rssparser ()
  "The main function, evaluating the command-line parameters."
  (cond
    ((string= *script-mode* "add")
     ;; add an entry
     (when
         (add-new-feed *script-arguments*)
       (format t "Success!~%")))
    ((or
      (string= *script-mode* "delete")
      (string= *script-mode* "del"))
     ;; remove an entry
     (if
      (delete-feed *script-arguments*)
      (format t "Deletion successful!~%")
      (format t "This feed could not be deleted.~%")))
    ((string= *script-mode* "list")
     ;; list all entries
     (let ((feedlist (list-all-feeds)))
       (if feedlist
           (progn
             ;; print a list of feeds
             (format t (concatenate 'string
                                    (prin1-to-string (list-length feedlist))
                                    (if (eql 1 (list-length feedlist))
                                        " feed is set up:~%"
                                        " feeds are set up:~%")))
             (force-output nil)
             (print-feed-list feedlist))
           (format t "You don't have any feeds yet."))))
    ((string= *script-mode* "export")
     ;; show an SQLite command to transfer this feed elsewhere
     (let ((result (export-feed *script-arguments*)))
      (if result
       (format t "Execute this SQL command to add this feed to a new database:~%  ~A~%~%" result)
       (format t "This feed could not be exported.~%"))))
    ((string= *script-mode* "parse")
     ;; the parser for existing sites
     (parse-all-feeds))
    ((string= *script-mode* "webserver")
     ;; start the web server
     (handler-case
         (start-webserver)
       (sb-sys:interactive-interrupt ()
         (format t "~%~%Quitting the web server. Goodbye.~%~%"))))
    (t
     ;; else ...
     (show-syntax))))



(rssparser)
