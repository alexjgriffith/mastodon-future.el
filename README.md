# mastodon-future
Add the following to your init file

``` elisp
(require 'mastodon)

(require 'mastodon-async)
(require 'mastodon-inspect)
(require 'mastodon-profile)
(require 'mastodon-notifications)

(define-key mastodon-mode-map (kbd "D") #'mastodon-inspect--toot)
(define-key mastodon-mode-map (kbd "U") #'mastodon-profile--get-next-author)
(define-key mastodon-mode-map (kbd "N") #'mastodon-notifications--get)
```

