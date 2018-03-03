;;; mastodon-profile.el --- Functions for inspecting Mastodon profiles -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;; mastodon-profile.el generates stream of users toots.
;; To fix
;; 1. Render Image at top of frame [x]
;; 2. Get toot author [x]
;; 3. Load more toots [x]
;; Later
;; 1. List followers [x]
;; 2. List people they follow [x]
;; 3. Option to follow
;; 4. wheather they follow you or not
;; 5. Show only Media

;;; Code:

(require 'mastodon-http  nil t)
(require 'mastodon-media  nil t)

(defgroup mastodon-profile nil
  "Inspect Mastadon profiles."
  :prefix "mastodon-profile-"
  :group 'mastodon)

(defun mastodon-profile--toot-proporties ()
  "Get the next by-line plist."
  (interactive)
  (let ((toot-proporties (plist-get (text-properties-at (point)) 'toot-json)))
    (if toot-proporties
        toot-proporties
      (plist-get (text-properties-at (mastodon-tl--goto-next-toot)) 'toot-json))))

;; TODO: split this function up so it can handle also showing
;; followers and those they follow
(defun mastodon-profile--make-author-buffer (status)
  "Takes a the STATUS (the by-line plist) and extracts account info."
  (let* ((id (mastodon-profile--field status 'id))
         (acct (mastodon-profile--field status 'acct))
                  accounts/id/statuses
                  (url (mastodon-http--api
                        (concat "accounts/"
                                (format "%s" id)
                                "/statuses" )))
         (buffer (concat "*mastodon-" acct  "*"))
         (account (cdr(assoc 'account status)))
         (note (mastodon-profile--field status 'note))
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/statuses" id)
                          update-function
                          ,'mastodon-tl--timeline json))
      (insert "\n")
      (insert (mastodon-profile--image-from-status account))
      (insert "\n  ")
      (insert (let ((mastodon-tl--display-media-p nil))
                (mastodon-tl--byline-author status)))
      (insert "\n ------------\n")
      (insert (mastodon-profile--process-text note))
      (insert (mastodon-tl--set-face
       (concat " ------------\n"
               "     TOOTS   \n"
               " ------------\n")
       'success 'nil))       
      (mastodon-tl--timeline json))
    (mastodon-tl--goto-next-toot)))

(defun mastodon-profile--process-text (text)
  "Retrieve TEXT content from TOOT."
    (propertize (with-temp-buffer
                  (insert (decode-coding-string text 'utf-8))
                  (shr-render-region (point-min) (point-max))
                  (buffer-string))
                'face 'default))

(defun mastodon-profile--get-next-author ()
  "Jump to the next author and generate a buffer."
  (interactive)
  (mastodon-profile--make-author-buffer(mastodon-profile--toot-proporties)))

(defun mastodon-profile--image-from-status (status)
  "Generate an image from a STATUS."
  (let ((url (cdr(assoc 'avatar_static status))))
    (unless (equal url "/avatars/original/missing.png")
      (mastodon-media--get-media-link-rendering url))))

(defun mastodon-profile--field (status field)
  "The STATUS is a nested alist.

FIELD is used to identify regions under 'account"
  (cdr (assoc field (assoc 'account status))))

(defun mastodon-profile--get-next-authour-id ()
  "Get the author id of the next toot."
  (interactive)
  (get-authour-id (toot-proporties)))

(defun mastodon-profile--add-author-bylines (tootv)
  "Covnvert TOOTV into an author-byline and insert."
      (mapc (lambda(toot)
              (insert (propertize (mastodon-tl--byline-author
                                   (list (append (list 'account) toot)))
                                  'toot-id (cdr(assoc 'id toot)) 'toot-json toot)
                      "\n"))
            tootv)
      (mastodon-media--inline-images))

;; Clean these two functions up. They are basically identical.
(defun mastodon-profile--get-following ()
  "Request a list of those who the user under point follows."
  (interactive)
  (let* ((id (mastodon-profile--field (mastodon-profile--toot-proporties) 'id))
         (acct (mastodon-profile--field (mastodon-profile--toot-proporties) 'acct))
         (buffer (format "*following-%s*" acct))
         (tootv (mastodon-http--get-json
                (mastodon-http--api (format "accounts/%s/following"
                                            id)))))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/following" id)
                          update-function
                          ,'mastodon-profile--add-author-bylines))     
      (mastodon-profile--add-author-bylines tootv))))

(defun mastodon-profile--get-followers ()
  "Request a list of those following the user under point."
  (interactive)
  (let* ((id (mastodon-profile--field (mastodon-profile--toot-proporties) 'id))
         (acct (mastodon-profile--field (mastodon-profile--toot-proporties) 'acct))
         (buffer (format "*followers-%s*" acct))
         (tootv (mastodon-http--get-json
                (mastodon-http--api (format "accounts/%s/followers"
                                            id)))))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint ,(format "accounts/%s/followers" id)
                          update-function
                          ,'mastodon-profile--add-author-bylines))
      (mastodon-profile--add-author-bylines tootv))))


(provide 'mastodon-profile)
;;; mastodon-profile.el ends here
