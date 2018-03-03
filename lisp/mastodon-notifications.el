;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.7.1
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

;; mastodon-notification.el provides notification functions.

;; todo: - Add keybinding of mastodon-notifications--get to `N'

;;; Code:

;;(require 'mastodon-media)
(require 'mastodon-http)
(require 'mastodon-tl)

(defgroup mastodon-notifications nil
  "Mastodon Notifications."
  :prefix "mastodon-notifications-"
  :group 'mastodon)

(defvar mastodon-notifications--types-alist
  '(("mention" . mastodon-notifications--mention)
    ("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog))
  "Alist of notification types that have been implemented.")

(defun mastodon-notifications--byline-concat (toot message)
  "Add byline for TOOT with MESSAGE."
      (concat
       " "
       (propertize message 'face 'highlight)
       " "
       "You!"))

(defun mastodon-notifications--byline (toot message)
  "Generate byline from TOOT based on MESSAGE for notification."
  (let ((id (cdr (assoc 'id toot)))
        (parsed-time (date-to-time (mastodon-tl--field 'created_at toot)))
        (faved (equal 't (mastodon-tl--field 'favourited toot)))
        (boosted (equal 't (mastodon-tl--field 'reblogged toot))))
    (propertize
     (concat (propertize "\n | " 'face 'default)
             (when boosted
               (format "(%s) "
                       (propertize "B" 'face 'mastodon-boost-fave-face)))
             (when faved
               (format "(%s) "
                       (propertize "F" 'face 'mastodon-boost-fave-face)))
             (mastodon-tl--byline-author toot)
             ;; only line that differes from mastodon-tl--timeline
             ;; possibly refactor
             (mastodon-notifications--byline-concat toot message)
             " "
             (propertize
              (format-time-string mastodon-toot-timestamp-format parsed-time)
              'timestamp parsed-time
              'display (if mastodon-tl--enable-relative-timestamps
                           (mastodon-tl--relative-time-description parsed-time)
                         parsed-time))
             (propertize "\n  ------------" 'face 'default))
     'favourited-p faved
     'boosted-p    boosted
     'toot-id      id
     'toot-json    toot)))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (mastodon-tl--content toot) "\n"
     (mastodon-notifications--byline toot "Mentioned")
     "\n\n")))

(defun mastodon-notifications--follow (note)
  "Format for a `follow' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
  (insert
    (propertize "Congratulations, you have a new follower!\n\n" 'face 'default)
    (mastodon-notifications--byline note "Follows")
    "\n\n")))

(defun mastodon-notifications--favourite (note)
  "Format for a `favorite' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (mastodon-tl--content toot) "\n"
     (mastodon-notifications--byline note "Favorited")
             "\n\n")))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (let ((toot (mastodon-tl--field 'status note)))
    (insert
     (mastodon-tl--content toot) "\n"
     (mastodon-notifications--byline note "Boosted")
     "\n\n")))

(defun mastodon-notifications--note (note)
  "Filters NOTE for those listed in `mastodon-notifications--types-alist'."
  (let* ((type (mastodon-tl--field 'type note))
         (fun (cdr (assoc type mastodon-notifications--types-alist))))
    (when fun
      (funcall fun note))))

(defun mastodon-notifications--notifications (json)
  "Format JSON in Emacs buffer."
  (mapc #'mastodon-notifications--note json)
    (goto-char (point-min))
  (while (search-forward "\n\n\n | " nil t)
    (replace-match "\n | "))
  (when mastodon-tl--display-media-p
    (mastodon-media--inline-images)))

(defun mastodon-notifications--get ()
  "Display NOTIFICATIONS in buffer."
  (interactive)
  (let* ((url (mastodon-http--api "notifications"))
         (buffer "*mastodon-notifications*")
         (json (mastodon-http--get-json url)))
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-mode)
      (setq mastodon-tl--buffer-spec
            `(buffer-name ,buffer
                          endpoint "notifications"
                          update-function
                          ,'mastodon-notifications--notifications))
      (mastodon-notifications--notifications json))))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
