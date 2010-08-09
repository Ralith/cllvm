(defpackage #:llvm-wrapper (:nicknames #:llvm)
  (:use #:common-lisp #:cffi)
  (:export
   #:add-incoming
   #:function-type
   #:build-phi
   #:build-call
   #:verify-module
   #:create-execution-engine-for-module
   #:create-interpreter-for-module
   #:create-jit-compiler-for-module
   #:struct-type
   #:const-gep
   #:build-gep))
