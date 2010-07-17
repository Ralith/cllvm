(in-package #:llvm-bindings)

(define-foreign-library llvm
  (t (:default "libLLVM-2.7")))

(use-foreign-library llvm)
