;;; -*- lexical-binding: t -*-
;;; mastodon-async.el --- Client for Mastodon

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "24.4"))
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

;; Bug, mastodon-async--html-buffer not properly defined

;;; Code:

(require 'json)

(defgroup mastodon-async nil
  "An async module for mastodon streams."
  :prefix "mastodon-async-"
  :group 'external)

;;;###autoload
(define-minor-mode mastodon-async-mode
  "Async Mastodon."
  :lighter " MasA")

(make-variable-buffer-local
 (defvar mastodon-async--queue "" ;;"*mastodon-async-queue*"
   "The intermediate queue buffer name."))

(make-variable-buffer-local
 (defvar mastodon-async--buffer "" ;;"*mastodon-async-buffer*"
   "User facing output buffer name."))

(make-variable-buffer-local
 (defvar mastodon-async--http-buffer "" ;;""
   "Buffer variable bound to http output."))


(defun mastodon-async--display-http ()
  "Display the async HTTP input buffer."
  (display-buffer mastodon-async--http-buffer))

(defun mastodon-async--display-buffer ()
  "Display the async user facing buffer."
  (interactive)
  (display-buffer mastodon-async--buffer))

(defun mastodon-async--display-queue ()
  "Display the async queue buffer."
  (display-buffer mastodon-async--queue))

(defun mastodon-async--stop-http ()
  "Stop the http processs and close the async and http buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (stop-process (get-buffer-process mastodon-async--http-buffer))
    (delete-process (get-buffer-process mastodon-async--http-buffer))
    (kill-buffer mastodon-async--http-buffer)
    (setq mastodon-async--http-buffer "")
    (kill-buffer mastodon-async--queue)))

(defun mastodon-async--stream-home ()
  "Open a stream of Home."
  (interactive)
  (mastodon-async--mastodon
   "user"
   "home"
   "home"
   'mastodon-async--process-queue-string))

(defun mastodon-async--stream-federated ()
  "Open a stream of Federated."
  (interactive)
  (mastodon-async--mastodon
   "public"
   "public"
   "federated"
   'mastodon-async--process-queue-string))

(defun mastodon-async--stream-local ()
  "Open a stream of Local."
  (interactive)
  ;; Need to add another layer of filtering for this to work
  ;; apparently it the local flag does not work
  (mastodon-async--mastodon
   "public"
   "public?local=true"
   "local"
   'mastodon-async--process-queue-local-string))

;; needs to be rewritten with async in mind
(defun mastodon-async--sync-get (timeline)
  "Display TIMELINE in buffer."
  (let* ((url (mastodon-http--api (concat "timelines/" timeline)))
         (buffer mastodon-async--buffer)
         (json (mastodon-http--get-json url)))    
    (with-output-to-temp-buffer buffer
      (switch-to-buffer buffer)
      (mastodon-tl--timeline json))))

(defun mastodon-async--mastodon (endpoint timeline name filter)
  "Make sure that the previous async process has been closed.

Then Start an async mastodon stream at ENDPOINT filtering toots
using FILTER."
  ;; (when (get-buffer mastodon-async--http-buffer)
  ;;   (progn (mastodon-async--stop-http)
  ;;          (when (get-buffer mastodon-async--buffer)
  ;;            (with-current-buffer mastodon-async--buffer
  ;;              (let ((inhibit-read-only t))
  ;;                (delete-region 1 (point-max)))))))  
  (let ((buffer (mastodon-async--start-process
                 (concat "streaming/" endpoint) filter name)))
    (with-current-buffer buffer
      (mastodon-async--display-buffer)
      (goto-char (point-max))
      ;;(mastodon-async--sync-get timeline)
      (when mastodon-tl--display-media-p
         (mastodon-async--inline-images 1 (point-max)))
      (goto-char 1))))

(defun mastodon-async--get (url callback)
  "An async get targeted at URL with a CALLBACK."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" .
            ,(concat
              "Bearer "
              (mastodon-auth--access-token))))))
    (url-retrieve url callback)))

(defun mastodon-async--set-local-variables(buffer
                                           http-buffer
                                           buffer-name
                                           queue-name)
  (with-current-buffer (get-buffer-create buffer)
    (setq mastodon-async--http-buffer http-buffer)
    (setq mastodon-async--buffer buffer-name)
    (setq mastodon-async--queue queue-name)))

(defun mastodon-async--setup-http (http-buffer name)
  (let ((queue-name(concat "*mastodon-async-queue-" name "*"))
        (buffer-name(concat "*mastodon-async-buffer-" name "*")))
    (mastodon-async--set-local-variables http-buffer http-buffer
                                         buffer-name queue-name)))

(defun mastodon-async--setup-queue (http-buffer name)
  (let ((queue-name(concat "*mastodon-async-queue-" name "*"))
        (buffer-name(concat "*mastodon-async-buffer-" name "*")))
    (mastodon-async--set-local-variables queue-name http-buffer
                                         buffer-name queue-name)))

(defun mastodon-async--setup-buffer (http-buffer name)
  (let ((queue-name (concat "*mastodon-async-queue-" name "*"))
        (buffer-name (concat "*mastodon-async-buffer-" name "*")))
    (mastodon-async--set-local-variables buffer-name http-buffer
                                         buffer-name queue-name)
    (with-current-buffer buffer-name
      (setq mastodon-tl--enable-relative-timestamps nil)
      (setq mastodon-tl--display-media-p nil)
      (mastodon-async-mode t))))

(defun mastodon-async--start-process (endpoint filter &optional name)
  "Start an async mastodon stream at ENDPOINT.
Filter the toots using FILTER."
  (let ((http-buffer (mastodon-async--get
                 (mastodon-http--api endpoint)
                 (lambda (status) (message "HTTP SOURCE CLOSED")))))
    ;; Set the local variables in each of the three
    ;; buffers
    (message (format "HTTP buffer: %s" http-buffer))
    (mastodon-async--setup-http  http-buffer (or name endpoint))
    (mastodon-async--setup-queue  http-buffer (or name endpoint))
    (mastodon-async--setup-buffer  http-buffer (or name endpoint))
    (with-current-buffer (get-buffer (concat "*mastodon-async-buffer-"
                                             (or name endpoint)
                                             "*"))
      (message (format "HTTP buffer 2: %s" mastodon-async--http-buffer)))
    (set-process-filter (get-buffer-process http-buffer)
                        (mastodon-async--http-hook filter))
    http-buffer))

(defun mastodon-async--http-hook (filter)
  "Return a lambda with a custom FILTER for processing toots."
  (lexical-let ((filter filter))
    (lambda (proc data)
      (with-current-buffer (process-buffer proc)
        (let* ((string
                (mastodon-async--stream-filter
                 (mastodon-async--http-layer proc data)))
               (queue-string (mastodon-async--cycle-queue string)))        
          (when queue-string
            (mastodon-async--output-toot
             (funcall filter queue-string))))))))


(defun mastodon-async--process-queue-string (string)
  "Parse the output STRING of the queue buffer."
  (let* ((split-strings (split-string string "\n" t))
         (event-type(replace-regexp-in-string
                     "^event: " ""
                     (car split-strings)))
         (data (replace-regexp-in-string
                "^data: " "" (cadr split-strings))))
    (when (equal "update" event-type)
      (json-read-from-string data))))

(defun mastodon-async--process-queue-local-string (string)
  "Use STRING to limit the public endpoint to displaying local steams only."
  (let ((json (mastodon-async--process-queue-string string)))
    (when json
      (when (mastodon-async--account-local-p json)
        json))))

(defun mastodon-async--account-local-p (json)
  "Test JSON to see if account is local."
  (not(string-match-p
       "@"
       (cdr(assoc 'acct (cdr (assoc 'account json)))))))
 
  
(defun mastodon-async--output-toot (toot)
  "Process TOOT and prepend it to the async user facing buffer."  
  (if (not(bufferp (get-buffer mastodon-async--buffer)))
      (mastodon-async--stop-http)
    (when toot
      (with-current-buffer mastodon-async--buffer
        (let* ((inhibit-read-only t)
               (previous (point))
               (string-with-nl
                (concat
                 (mastodon-tl--spoiler toot)
                 (mastodon-tl--content toot)
                 (when mastodon-tl--display-media-p
                   (mastodon-tl--media toot))
                 (mastodon-tl--byline toot)
                 "\n\n"))
               (string 
                (replace-regexp-in-string
                 "\n\n\n | " "\n | " string-with-nl))
               (offset (length string)))
          (goto-char 1)
          (when (stringp string)
            (insert string))
          (when mastodon-tl--display-media-p
             (mastodon-async--inline-images 1 offset))
          (if (equal 1 previous)
              (goto-char 1)
            (goto-char (+ offset previous))))))))

(defun mastodon-async--cycle-queue (string)
  "Append the most recent STRING from http buffer to queue buffer.

Then determine if a full message has been recived.  If so return it.
Full messages are seperated by two newlines"
    (with-current-buffer mastodon-async--queue
      (goto-char (max-char))
      (insert (decode-coding-string string 'utf-8))
      (goto-char 0)
      (let((next(re-search-forward "\n\n" nil t)))
        (when next
          (let ((return-string (buffer-substring 1 next))
                (inhibit-read-only t))
            (delete-region 1 next)
            return-string)))))

(defun mastodon-async--http-layer (proc data)
  "Passes PROC and DATA to ‘url-http-generic-filter’.

It then processes its output."
  (with-current-buffer (process-buffer proc)    
    (let ((start (max 1 ( - (point-max) 2))))
      (url-http-generic-filter proc data)
      (when (> url-http-end-of-headers start)
        (setq start url-http-end-of-headers))
      (let ((end (- (point-max) 2)))
        (buffer-substring start end)))))

(defun mastodon-async--stream-filter (string)
  "Remove comments from STRING."
  (replace-regexp-in-string  "^:.*\n" "" string))

(defun mastodon-async--inline-images-region ()
  (interactive)
  (mastodon-async--inline-images (region-beginning) (region-end) (current-buffer)))

(defun mastodon-async--inline-images-buffer ()
  (interactive)
  (mastodon-async--inline-images 1 (point-max) (current-buffer)))


(defun mastodon-async--get-image (link &optional buffer)
  (url-retrieve link 'mastodon-async--image-callback
                (list link (or buffer (current-buffer))) t))

(defun mastodon-async--inline-images (start end &optional buffer)
  (goto-char start)
  (let (line-coordinates)
    (while
        (setq line-coordinates (mastodon-media--select-next-media-line end))
      (let ((link (mastodon-media--line-to-link line-coordinates)))
        (when (mastodon-media--valid-link-p link)
          (mastodon-async--get-image link (or buffer (current-buffer))))))))

(defun mastodon-async--image-callback (data url buffer)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let((data (buffer-substring (point) (point-max))))
    (with-current-buffer buffer
      (mastodon-async--find-and-replace-image url data))))


(defun mastodon-async--find-and-replace-image (url data)
  (let ((loc nil)
        (inhibit-read-only t)
        (previous (point)))
     (goto-char 1)
    (when (setq loc (re-search-forward (concat "Media_Link:: " url)))
      (goto-char loc)
      (kill-whole-line)      
      (insert-image (create-image data nil t))
      (insert "\n")
      (if (equal 1 previous)
            (goto-char 1)
          (goto-char (+ 1 previous))))))

(provide 'mastodon-async)
;;; mastodon-async.el ends here
