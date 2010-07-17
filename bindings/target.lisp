(in-package #:llvm-bindings)

(defcfun (initialize-x86-target-info "LLVMInitializeX86TargetInfo") :void)
(defcfun (initialize-x86-target "LLVMInitializeX86Target") :void)

(defun initialize-native-target ()
  #+:x86
  (progn (initialize-x86-target-info)
         (initialize-x86-target)
         t)
  #-(or :x86)
  nil)
