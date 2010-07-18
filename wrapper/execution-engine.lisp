(in-package #:llvm-wrapper)

(defun create-execution-engine-for-module (out-execution-engine module)
  (lispifying-errors (%llvm:create-execution-engine-for-module out-execution-engine module)))

(defun create-interpreter-for-module (out-interpreter module)
  (lispifying-errors (%llvm:create-interpreter-for-module out-interpreter module)))

(defun create-jit-compiler-for-module (out-jit module)
  (lispifying-errors (%llvm:create-jit-compiler-for-module out-jit module)))
