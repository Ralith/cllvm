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

(defcfun (dispose-execution-engine "LLVMDisposeExecutionEngine") :void
  (execution-engine execution-engine-ref))

(defcfun (run-static-constructors "LLVMRunStaticConstructors") :void
  (execution-engine execution-engine-ref))

(defcfun (run-static-destructors "LLVMRunStaticDestructors") :void
  (execution-engine execution-engine-ref))

(defcfun (run-function-as-main "LLVMRunFunctionAsMain") :int
  (execution-engine execution-engine-ref)
  (function value-ref)
  (argc :unsigned-int)
  (argv (:pointer (:pointer :char)))
  (env (:pointer (:pointer :char))))

(defcfun (run-function "LLVMRunFunction") generic-value-ref
  (execution-engine execution-engine-ref)
  (function value-ref)
  (argument-count :unsigned-int)
  (arguments (:pointer generic-value-ref)))
