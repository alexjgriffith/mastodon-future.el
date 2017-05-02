
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



(defvar *bb* (mastodon-inspect--download-single-toot "4649149"))

(mastodon-render--define-part
 context default t
 (lambda(event )
   (mastodon-render--process-content
    (mastodon-render--get-field event 'content)))
 "")

(mastodon-format--get-context *bb*)


(intern-soft ":test2")

:test2

;; (intern-soft "test")


(defmacro mastodon-render--define-part (name face test-fun
                                             true-value false-value)
  "Create a getter for NAME from EVENT json strucutre. 

TEST-FUN, TRUE-VALUE, and FALSE-VALUE will be evaluated with a single
argument, event, if they are functions. If they are not functions their
literal values will be used in the output plist"
  (let* ((func (intern (format "mastodon-format--get-%s" name)))
         (docs (format "Get %s from EVENT json structure." name)))
   `(defun ,func (event) ,docs
      (let ((state (eval-if-function ,test-fun event))
            (true (eval-if-function ,true-value event))
            (false (eval-if-function ,false-value event)))
      (list 'type ',name
          'state state
          'true true
          'false false
          'face ',face)))))

(mastodon-render--define-part content (mastodon-render--process-content
                                       (mastodon-render--get-field)))
