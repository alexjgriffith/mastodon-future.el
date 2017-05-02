;;; mastodon-render.el --- Rendering toots form mastodon.el

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.1
;; Homepage: https://github.com/jdenen/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-render.el provides an alternate rendering for mastodon.el
;; need to document

;;; Code:

(require 'time-date)

(defcustom mastodon-render-waring-string " ----cw---- \n"
  "The warning string for a toot.")

(defcustom mastodon-render-boosted-string "(B) "
  "Appears before toot when a user has boosted it.")

(defcustom mastodon-render-favourited-string "(F) "
  "Appears before toot when a user has favourited it.")

(defcustom mastodon-render-reblog-string "BOOSTED"
  "The string that appears between two users after boosting.")

(defvar mastodon-render-event-components '()
  "A list of event components that have a getter.

Getters are defined by mastodon-render--define-part")

(defmacro mastodon-render--define-part (name face test-fun
                                             true-value false-value &rest more-plist-values)
  "Create a getter for NAME from EVENT json strucutre. 

TEST-FUN, TRUE-VALUE, and FALSE-VALUE will be evaluated with a single
argument, event, if they are functions. If they are not functions their
literal values will be used in the output plist"
  (let* ((func (intern (format "mastodon-format--get-%s" name)))
         (docs (format "Get %s from EVENT json structure." name)))
    (add-to-list 'mastodon-render-event-components `(,name ,func))
   `(defun ,func (event) ,docs
      (let* ((state (mastodon-render--eval-if-funcp ,test-fun event))
            (true (mastodon-render--eval-if-funcp ,true-value event state))
            (false (mastodon-render--eval-if-funcp ,false-value event state)))
      (list 'type ',name
          'state state
          'true true
          'false false
          'face ',face
          ,@more-plist-values)))))

(defun mastodon-render--eval-if-funcp (func &rest vars)
    (if (functionp func)
        (apply func vars)
      func))

(defun mastodon-render--get-field (event field)
  (cdr (assoc field event)))

(defun mastodon-render--get-field-2 (event field-1 field-2)
  (mastodon-render--get-field (mastodon-render--get-field event field-1) field-2))

(defun mastodon-render--get-field-3 (event field-1 field-2 field-3)
  (mastodon-render--get-field (mastodon-render--get-field-2 event field-1 field-2) field-3))


(defun mastodon-render--html (string)
  (with-temp-buffer
    (insert (decode-coding-string string 'utf-8))
    (shr-render-region (point-min) (point-max))
    (goto-char (point-max))
    (delete-region (point) (progn (skip-chars-backward "\n") (point)))
    (insert "\n")
    (buffer-string)))


(mastodon-render--define-part
 spoiler-text
 default
 (not (equal spoiler-value ""))
 (mastodon-render--get-field event 'spoiler_text)
 "")

(mastodon-render--define-part
 cw
 success
 (not(equal "" (mastodon-render--get-field event 'spoiler_text)))  
 mastodon-render-waring-string
 ""
 'hidden '())


(mastodon-render--define-part
 context
 default t
 (lambda(event status)
   (mastodon-render--process-content
    (mastodon-render--get-field event 'content)))
 "")

;; This is the most complex of all the getters so far
(mastodon-render--define-part
 images ;; getter type
 default ;; the face of the output string
 (lambda(event state) ;; test wether the state should be true
   ;; lambda not neaded, but good for clarity
   (let ((media-list (append
                      ;; Attachments in unboosted toots
                      (mastodon-render--get-field
                       event
                       'media_attachments)
                      ;; Attachments in boosted toots
                      (mastodon-render--get-field-2
                       event
                       'reblog
                       'media_attachments)
                      ;; append [] nil :: Vector -> List
                      nil)))
   (when (> (length media-list) 0) 
     (mapcar
      (lambda(media)
        ;; extract the preview_url from the attachment
        ;; details
        (cdr(assoc 'preview_url media)))
      media-list ))))
 (concat (mapconcat ;; true case
          (lambda(media)
            (concat "Media_Link:: "
                     media))
          state "\n") "\n")
 "" ;; false case
 'attachments state ;; additional plist val 1
 'number (length state) ;; additional plist val 2)


(mastodon-render--define-part
 boosted
 success
 (equal (mastodon-render--get-field event 'reblogged) :json-true)
 mastodon-render-boosted-string
 "")

(mastodon-render--define-part
 favourited
 success
 (equal (mastodon-render--get-field event 'favourited) :json-true)
 mastodon-render-favourited-string
 "")

(mastodon-render--define-part
 display-name
 warning
 t
 (mastodon-render--process-display-name
  (mastodon-render--get-field-2 event 'account 'display_name))
 "")

(mastodon-render--define-part
 acct
 default
 t
 (concat "(@" (mastodon-render--get-field-2 event 'account 'acct) ")")
 "")


(mastodon-render--define-part
 acct
 success
 (mastodon-render--get-field event 'reblog)
 mastodon-render-reblog-string
 "")


(mastodon-render--define-part
 reblog-display-name
 warning
 (mastodon-render--get-field event 'reblog)
 (mastodon-render--process-display-name
  (mastodon-render--get-field-3 event
                                'reblog
                                'account
                                'display_name))
 "")

(mastodon-render--define-part
 reblog-display-name
 default
 (mastodon-render--get-field event 'reblog)
 (concat "(@"(mastodon-render--get-field-3 event 'reblog 'account 'acct)
        ")")
 "")


(mastodon-render--define-part
 time
 default
 t
 (mastodon-render--process-time
          (mastodon-render--get-field event 'created_at))
 "")


(defun mastodon-render--process-spoiler (string)
  (mastodon-render--html string))
 
(defun mastodon-render--process-content (string)
  (mastodon-render--html string))

(defun mastodon-render--process-time (string)
  (format-time-string
   mastodon-toot-timestamp-format (date-to-time string)))

(defun mastodon-render--process-display-name (string)
  (decode-coding-string string 'utf-8))

(defun mastodon-render--toot-string-layout (event)
  (let ((spoiler-text (mastodon-render--get-spoiler-text event))
        (cw (mastodon-render--get-cw event))
        (content (mastodon-render--get-content event))
        (images (mastodon-render--get-images event))
        (boosted (mastodon-render--get-boosted event))
        (favourited (mastodon-render--get-favourited event))
        (display-name (mastodon-render--get-display-name event))
        (acct (mastodon-render--get-acct event))
        (reblog (mastodon-render--get-reblog event))
        (reblog-display-name (mastodon-render--get-reblog-display-name event))
        (reblog-acct (mastodon-render--get-reblog-acct event))
        (time (mastodon-render--get-time event)))
    (mastodon-render--toot-add-default
     `(,spoiler-text
      ,cw 
      ,content 
      ,images 
      " | " ,boosted "" ,favourited ""
      ,display-name  ,acct 
      ,reblog " " ,reblog-display-name
      ,reblog-acct " " ,time "\n"
      " ----------\n"))))


(defun mastodon-render--toot-add-default (alist)
    (mapcar
      (lambda(x)
       (if (stringp x)
           (list x '(type :visual face default))
         x))
      alist))

(defun mastodon-render--toot-string-compose (alist)
  (let ((prev 0)
        (range-list '())
        (out-string "")
        (list alist))
    (message "in")
    (while (cadr list)
      (let* ((string (pop list))
             (start prev)
             (end (+ prev (length (car string))))             
             (range (list  (or (plist-get (second string) 'type)
                               :visual)
                           start end)))
        (push range range-list)
        (message (car string))
        (setq prev (+ prev (- end start)))
        (setq out-string (concat out-string (car string)))))
    (print out-string)
    (list out-string  (reverse range-list))))

(defun mastodon-render--propertized-toot (event compile-toot-layout)
  (let* ((toot-layout (mastodon-render--toot-string-compose
                       (funcall compile-toot-layout event)))
         (toot-string (car toot-layout))
         (ranges (cdr toot-layout)) 
        (boosted (< (length(mastodon-render--get-boosted event) )0))
        (favourited (< (length(mastodon-render--get-favourited event) )0))
        (cw (< (length(mastodon-render--get-cw event ) )0))
        (toot-id (mastodon-render--get-field event 'id)))
    (propertize toot-string                
                'toot-id toot-id                
                'boosted boosted
                'favourited favourited
                'cw cw
                'ranges ranges)))

(defun mastodon-render--toot (event)
  (insert (mastodon-render--propertized-toot
           event
           'mastodon-render--toot-string-layout)))

(defun mastodon-render--check-proporties ()
  (interactive )
  (let* ((props  (text-properties-at (point) ))
         (buffer (get-buffer-create "proporties")))
    (with-current-buffer buffer (insert(pp props))) (display-buffer buffer)))

(defun mastodon-render--goto-part (part fun)
  (let ((range (assoc
                part
                (car(plist-get
                     (text-properties-at (point))
                     'ranges)) )))
    (goto-char (funcall fun range ))))

;; example mastodon-render--toggle-value(:boosted )
(defun mastodon-render--toggle-value (part)
  ;; there will be a true and false rendering for each part
  ;; the state will be known, by toggling this you switch
  ;; the state
  ;; In addition for all regions that start after this point
  ;; their start and end values area djusted by the difference
  ;; in the toggle widths (eg "" "(B) " -> 0,4)
  ;; for now false is "" for all parts
  )

;; eg
(defun mastodon-render--get-content-mod (event)
  (let ((content (mastodon-render--process-content
                  (mastodon-render--get-field event 'content))))
    (list 'type :content
          'state t
          'true content
          'false ""
          'face 'default))))


;; (insert(default-toot *boost-buffer* 'default-compile-toot-string))
;;(setq debug-on-error 't)
(defun mastodon-tl--toot (event)
  (insert (mastodon-render--propertized-toot
           event
           'mastodon-render--toot-string-layout)))

(provide 'mastodon-render)
;;; mastodon-render.el ends here
