(use-modules
 ((ghc-servant) #:select
  (ghc-aeson-1.4.2.0
   ghc-conduit-1.3.0.3
   ghc-servant-server
   ghc-servant-websockets
   ghc-websockets))
 ((ghc-system) #:select (ghc-directory))
 ((ghc-optparse-generic) #:select (ghc-optparse-generic))
 ((gnu packages base) #:select (gnu-make))
 ((gnu packages haskell) #:select (ghc ghc-async ghc-uuid))
 ((gnu packages haskell-web) #:select (ghc-wai))
 ((purescript) #:select (purescript purescript-spago)))

`(,ghc
  ,ghc-aeson-1.4.2.0
  ,ghc-async
  ,ghc-conduit-1.3.0.3
  ,ghc-directory
  ,ghc-optparse-generic
  ,ghc-servant-server
  ,ghc-servant-websockets
  ,ghc-uuid
  ,ghc-wai
  ,ghc-websockets
  ,gnu-make
  ,purescript
  ,purescript-spago)
