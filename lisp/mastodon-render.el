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

(defcustom mastodon-render-waring-string " ----cw---- \n"
  "The warning string for a toot.")

(defcustom mastodon-render-boosted-string "(B) "
  "Appears before toot when a user has boosted it.")

(defcustom mastodon-render-favourited-string "(F) "
  "Appears before toot when a user has favourited it.")

(defcustom mastodon-render-reblog-string "BOOSTED"
  "The string that appears between two users after boosting.")

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

(defun mastodon-render--get-spoiler-text (event)
  (let ((spoiler (mastodon-render--process-spoiler
                  (mastodon-render--get-field event 'spoiler_text))))
    (propertize spoiler
                'type :spoiler-text
                'length (length spoiler)
                'face 'default)))

(defun mastodon-render--get-cw (event)
 (let ((cw(if (equal "" (mastodon-render--get-field event 'spoiler_text))
              ""
            mastodon-render-waring-string)))
 (propertize cw
             'type :cw
             'hidden :false
             'length (length cw)
             'face 'success)))

(defun mastodon-render--get-content (event)
  (let ((content (mastodon-render--process-content
                  (mastodon-render--get-field event 'content))))
    (propertize content
                'type :content
                'length (length content)
                'face 'default)))

(defun mastodon-render--get-images (event)
  (let*((media-list (append
                     (mastodon-render--get-field
                      event
                      'media_attachments)
                     nil))
        (media-string (if (> (length media-list) 0) 
                          (concat (mapconcat
                           (lambda(media)
                             (concat "Media_Link:: "
                                     (cdr(assoc 'preview_url media))))
                           media-list "\n") "\n")
                        "" ;;else
                        )))
    (propertize media-string
                'type :media
                'length (length media-string)
                'number (length media-list)
                'attachments media-list
                'face 'default)))

(defun mastodon-render--get-boosted (event)
  (let((boost (if (equal (mastodon-render--get-field event 'reblogged)
                         :json-true)
                  mastodon-render-boosted-string
                "")))
        (propertize boost
                    'type :boosted
                    'length (length boost)
                    'face 'success)))

(defun mastodon-render--get-favourited (event)
  (let((fave (if (equal (mastodon-render--get-field event 'favourited)
                        :json-true)
                mastodon-render-favourited-string
                "")))
        (propertize fave
                    'type :favourited
                    'length (length fave)                    
                    'face 'success)))

(defun mastodon-render--get-display-name (event)
  (let ((user(mastodon-render--get-field-2 event 'account 'display_name)))
    (propertize user
                'type :display-name
                'length (length user)
                'face 'warning)))

(defun mastodon-render--get-acct (event)
  (let ((acct (concat "(@"
                      (mastodon-render--get-field-2 event 'account 'acct)
                      ")")))
    (propertize acct
                'type :acct
                'length (length acct)
                'face 'default)))

(defun mastodon-render--get-reblog (event)
  (let((rebloger(if (mastodon-render--get-field event 'reblog)
                    mastodon-render-reblog-string
                  "")))
    (propertize rebloger
                'type :reblog
                'length (length rebloger)
                'face 'success)))

(defun mastodon-render--get-reblog-display-name (event)
  (let((rebloger(if (mastodon-render--get-field event 'reblog)
                    (mastodon-render--get-field-3 event
                                 'reblog
                                 'account
                                 'display_name)
                  "")))
    (propertize rebloger
                'type :reblog-diplay-name
                'length (length rebloger)                
                'face 'warning)))

(defun mastodon-render--get-reblog-acct (event) 
  (let (( re-acct(if (mastodon-render--get-field event 'reblog)
                     (concat "(@"(mastodon-render--get-field-3 event 'reblog 'account 'acct)
                             ")")
                   "")))
    (propertize re-acct
                'type :reblog-acct
                'length (length re-acct)
                'face 'default)))

(defun mastodon-render--get-time (event)
  (let((time (mastodon-render--get-field event 'created_at)))
    (propertize time
                'type :time
                'length (length time)
                'face 'default)))
  
(defun mastodon-render--process-spoiler (string)
  (mastodon-render--html string))
 
(defun mastodon-render--process-content (string)
  (mastodon-render--html string))

(defun mastodon-render--process-time (string) string)

(defun mastodon-render--toot-string (event)
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
  (concat spoiler-text
          cw 
          content 
          images 
          " | " boosted "" favourited ""
          display-name  acct 
          reblog " " reblog-display-name
           reblog-acct " " time "\n"
          " ----------\n")))

(defun mastodon-render--propertized-toot (event compile-toot-string)
  (let ((toot-string (funcall compile-toot-string event))
        (boosted (< (length(mastodon-render--get-boosted event) )0))
        (favourited (< (length(mastodon-render--get-favourited event) )0))
        (cw (< (length(mastodon-render--get-cw event ) )0))
        (toot-id (mastodon-render--get-field event 'id)))
    (propertize toot-string                
                'toot-id toot-id                
                'boosted boosted
                'favourited favourited
                'cw cw)))

(defun mastodon-render--toot (event)
  (insert (mastodon-render--propertized-toot
           event
           'mastodon-render--toot-string)))

(defun mastodon-render--check-proporties ()
  (interactive )
  (let* ((props  (text-properties-at (point) ))
         (buffer (get-buffer-create "proporties")))
    (with-current-buffer buffer (insert(pp props))) (display-buffer buffer)))

;; (insert(default-toot *boost-buffer* 'default-compile-toot-string))

 (defun mastodon-tl--toot (event)
  (insert (mastodon-render--propertized-toot
           event
           'mastodon-render--toot-string)))


(provide 'mastodon-render)
;;; mastodon-render.el ends here
