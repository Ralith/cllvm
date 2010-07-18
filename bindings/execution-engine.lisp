(in-package #:llvm-bindings)

(defctype generic-value-ref :pointer)
(defctype execution-engine-ref :pointer)

(defcfun (link-in-jit "LLVMLinkInJIT") :void)
(defcfun (link-in-interpreter "LLVMLinkInInterpreter") :void)

;;; Operations on generic values
(defcfun (create-generic-value-of-int "LLVMCreateGenericValueOfInt") generic-value-ref
  (type type-ref)
  (value :unsigned-long-long)
  (is-signed :boolean))

(defcfun (create-generic-value-of-pointer "LLVMCreateGenericValueOfPointer") generic-value-ref
  (pointer :pointer))

(defcfun (create-generic-value-of-Float "LLVMCreateGenericValueOfFloat") generic-value-ref
  (type type-ref)
  (value :double))

(defcfun (dispose-generic-value "LLVMDisposeGenericValue") :void
  (generic-value generic-value-ref))

;;; Operations on execution engines
(defcfun (create-execution-engine-for-module "LLVMCreateExecutionEngineForModule") :boolean
  (out-execution-engine (:pointer execution-engine-ref))
  (module module-ref)
  (out-error (:pointer (:pointer :char))))

(defcfun (create-interpreter-for-module "LLVMCreateInterpreterForModule") :boolean
  (out-interpreter (:pointer execution-engine-ref))
  (module module-ref)
  (out-error (:pointer (:pointer :char))))

(defcfun (create-jit-compiler-for-module "LLVMCreateJITCompilerForModule") :boolean
  (out-jit (:pointer execution-engine-ref))
  (module module-ref)
  (out-error (:pointer (:pointer :char))))
