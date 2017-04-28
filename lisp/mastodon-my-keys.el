;;; mastodon-my-keys.el --- Functions for inspecting Mastodon profiles

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

;;; Code:

(define-derived-mode mastodon-mode nil "Mastodon"
  "Major mode for Mastodon, the federated microblogging network."
  :group 'mastodon
  (let ((map mastodon-mode-map))
    (define-key map (kbd "b") #'mastodon-toot--boost)
    (define-key map (kbd "f") #'mastodon-toot--favourite)
    (define-key map (kbd "F") #'mastodon-tl--get-federated-timeline)
    (define-key map (kbd "H") #'mastodon-tl--get-home-timeline)
    (define-key map (kbd "j") #'mastodon-tl--goto-next-toot)
    (define-key map (kbd "k") #'mastodon-tl--goto-prev-toot)
    (define-key map (kbd "L") #'mastodon-tl--get-local-timeline)
    (define-key map (kbd "U") #'mastodon-profile--get-next-author)
    (define-key map (kbd "N") #'mastodon-notifications--get)
    (define-key map (kbd "D") #'mastodon-debug--inspect-toot)
    (define-key map (kbd "n") #'mastodon-toot)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "Q") #'kill-buffer-and-window)
    (define-key map (kbd "r") #'mastodon-toot--reply)
    (define-key map (kbd "t") #'mastodon-tl--thread)
    (define-key map (kbd "T") #'mastodon-tl--get-tag-timeline)
    (define-key map (kbd "u") #'mastodon-tl--update)))

(provide 'mastodon-my-keys)
;;; mastodon-my-keys.el ends here

