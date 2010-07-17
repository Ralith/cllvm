(in-package #:llvm-bindings)

(defcfun (link-in-jit "LLVMLinkInJIT") :void)
(defcfun (link-in-interpreter "LLVMLinkInInterpreter") :void)
