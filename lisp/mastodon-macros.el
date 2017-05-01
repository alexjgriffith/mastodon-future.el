
;; eg
(defun mastodon-render--get-content-mod (event)
  (let ((content (mastodon-render--process-content
                  (mastodon-render--get-field event 'content))))
    (list 'type :content
          'state t
          'true content
          'false ""
          'face 'default))))


(defun eval-if-function (func var)
    (if (functionp func)
        (funcall func var)
      func))

(eval-if-function  (lambda (x) x) "test")

;; (defun symbol-to-keyword (symbol)
;;   (let* ((keyword-string (format ":%s" symbol))
;;          (maybe-keyword (intern-soft keyword-string)))
;;     (if maybe-keyword maybe-keyword (intern-soft keyword-string))))




(intern-soft ":test2")

:test2

;; (intern-soft "test")


(defmacro mastodon-render--define-part (name
                                        face
                                        test-fun
                                        true-value
                                        false-vaule)
  (let* ((func (intern (format "mastodon-format--get-%s" name)))
         (docs (format "Get %s from json." name))
          'true true-value
          'false false-value
          'false face)
      ))))

(mastodon-render--define-part content (mastodon-render--process-content
                                       (mastodon-render--get-field)))
