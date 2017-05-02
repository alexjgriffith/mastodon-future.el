
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



(defvar *bb* (mastodon-inspect--download-single-toot "4649149"))

(defvar *image* (mastodon-inspect--download-single-toot "4895135"))

(mastodon-format--get-context *bb*)

(mastodon-format--get-cw *bb*)


(intern-soft ":test2")

:test2

;; (intern-soft "test")


(cadar mastodon-render-event-components)

(cadar mastodon-render-event-components)

(funcall (cadar mastodon-render-event-components) *bb* )

(mastodon-inspect--dump-json-in-buffer "temp" (funcall (cadar mastodon-render-event-components) *image* ))

(setq debug-on-error t)

(mastodon-render--define-part content (mastodon-render--process-content
                                       (mastodon-render--get-field)))
