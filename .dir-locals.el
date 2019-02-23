((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root) "make static" projectile-compilation-cmd-map)
                   (setq projectile-project-run-cmd
                         (pcase system-type
                           ('darwin "cabal new-run lets-try-purescript -- --on 8080 --from ./share --users ./share/users.json --messages ./share/messages")
                           (_ "make start")))))))
 (dhall-mode . ((indent-tabs-mode . nil)))
 (haskell-mode . ((haskell-process-type . 'auto))))
