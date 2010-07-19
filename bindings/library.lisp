(in-package #:llvm-bindings)

(define-foreign-library llvm
  (t (:default "libLLVM-2.8svn")))

(use-foreign-library llvm)
