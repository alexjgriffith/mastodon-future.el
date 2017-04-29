# mastodon-future
Add the following to your init file

``` elisp
(require 'mastodon)

(add-to-list 'load-path "path-to-modules/mastodon-future.el/lisp/")

(require 'mastodon-async)
(require 'mastodon-inspect)
(require 'mastodon-profile)
(require 'mastodon-notifications)

(define-key mastodon-mode-map (kbd "D") #'mastodon-inspect--toot)
(define-key mastodon-mode-map (kbd "U") #'mastodon-profile--get-next-author)
(define-key mastodon-mode-map (kbd "N") #'mastodon-notifications--get)
```

There is also an alternate toot-renderer. Note that it has **NOT** been fully integraeted with all time line funcitonality.

``` elsip
(require 'mastodon-render)
```
