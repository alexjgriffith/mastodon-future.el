;; eg
(defun mastodon-render--get-content-mod (event)
  (let ((content (mastodon-render--process-content
		  (mastodon-render--get-field event 'content))))
    (list 'type :content
	  'state t
	  'true content
	  'false ""
	  'face 'default))))



(eval-if-function  (lambda (x) x) "test")

;; (defun symbol-to-keyword (symbol)
;;   (let* ((keyword-string (format ":%s" symbol))
;;          (maybe-keyword (intern-soft keyword-string)))
;;     (if maybe-keyword maybe-keyword (intern-soft keyword-string))))



(defun mastodon-inspect--get-tl-vector (timeline)
  "Display TIMELINE in buffer."
  (let* ((url (mastodon-http--api (concat "timelines/" timeline)))
	 (buffer (concat "*mastodon-" timeline "*"))
	 (json (mastodon-http--get-json url)))
    json))






(defvar *bb* (mastodon-inspect--download-single-toot "4649149"))

(defvar *image* (mastodon-inspect--download-single-toot "4895135"))

(mastodon-format--get-context *bb*)

(mastodon-format--get-cw *bb*)


(intern-soft ":test2")

:test2

;; (intern-soft "test")


(mastodon-inspect--dump-json-in-buffer "args" mastodon-render-event-components)

(defmacro mastodon-render--design-layout (name options layout)
  (let* ((func (intern (format "mastodon-format--layout-%s" name)))
	 (docs (format "Layout NAME with OPTIONS.

Defines LAYOUT structure for toots." name)))
  `(defun ,name (event) ,docs
       (let ,(mapcar (lambda(part)
		       `(,(car part) (funcall ,(cadr part) event)))
		     options)))
    (mastodon-render--toot-add-default
     ,layout)))

(mastodon-render--design-layout
 "toot-default"
 mastodon-render-event-components
 `(,spoiler-text
      ,cw
      ,content
      ,images
      " | " ,boosted "" ,favourited ""
      ,display-name  ,acct
      ,reblog " " ,reblog-display-name
      ,reblog-acct " " ,time "\n"
      " ----------\n"))


(cadar mastodon-render-event-components)


(mastodon-render--layout-toot-default *bb*)

(funcall (cadar mastodon-render-event-components) *bb* )

(funcall(cadr(elt mastodon-render-event-components 9)) *bb*)

`(let ,(mapcar (lambda(part) `(,(car part) (funcall ,(cadr part) event))) mastodon-render-event-components))

(mastodon-inspect--dump-json-in-buffer "temp" (funcall (cadar mastodon-render-event-components) *image* ))



(setq debug-on-error t)

(mastodon-render--define-part content (mastodon-render--process-content
				       (mastodon-render--get-field)))
