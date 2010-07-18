(defpackage #:llvm-wrapper (:nicknames #:llvm)
  (:use #:common-lisp #:cffi)
  (:export
   #:add-incoming
   #:function-type
   #:build-phi
   #:build-call))
