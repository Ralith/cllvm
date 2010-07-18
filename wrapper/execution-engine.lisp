(in-package #:llvm-wrapper)

(defun create-execution-engine-for-module (module)
  (with-double-ptr (ret address %llvm:execution-engine-ref)
    (lispifying-errors (%llvm:create-execution-engine-for-module address module))
    ret))

(defun create-interpreter-for-module (module)
  (with-double-ptr (ret address %llvm:execution-engine-ref)
    (lispifying-errors (%llvm:create-interpreter-for-module address module))
    ret))

(defun create-jit-compiler-for-module (module)
  (with-double-ptr (ret address %llvm:execution-engine-ref)
    (lispifying-errors (%llvm:create-jit-compiler-for-module address module))
    ret))
