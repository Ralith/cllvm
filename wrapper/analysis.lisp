(in-package #:llvm-wrapper)

(defun verify-module (module)
  (lispifying-errors (%llvm:verify-module module :return-status)))
