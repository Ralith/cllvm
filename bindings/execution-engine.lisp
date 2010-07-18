(in-package #:llvm-bindings)

(defctype generic-value-ref :pointer)
(defctype execution-engine-ref :pointer)

(defcfun (link-in-jit "LLVMLinkInJIT") :void)
(defcfun (link-in-interpreter "LLVMLinkInInterpreter") :void)

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
