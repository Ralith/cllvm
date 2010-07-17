(defpackage #:llvm-bindings (:nicknames #:%llvm)
  (:use #:common-lisp #:cffi)
  (:export
   #:context-ref
   #:module-ref
   #:type-ref
   #:type-handle-ref
   #:value-ref
   #:basic-block-ref
   #:builder-ref
   #:dispose-module
   #:link-in-jit
   #:link-in-interpreter
   #:verifier-failure-action
   #:verify-module))