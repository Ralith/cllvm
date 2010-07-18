(in-package #:llvm-bindings)
(declaim (optimize (debug 3)))

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

(defcfun (create-generic-value-of-float "LLVMCreateGenericValueOfFloat") generic-value-ref
  (type type-ref)
  (value :double))

(defcfun (generic-value-int-width "LLVMGenericValueIntWidth") :unsigned-int
  (generic-value generic-value-ref))

(defcfun (generic-value-to-int "LLVMGenericValueToInt") :unsigned-long-long
  (generic-value generic-value-ref)
  (is-signed :boolean))

(defcfun (generic-value-to-pointer "LLVMGenericValueToPointer") :pointer
  (generic-value generic-value-ref))

(defcfun (generic-value-to-float "LLVMGenericValueToFloat") :double
  (type type-ref)
  (generic-value generic-value-ref))

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
  (optimization-level :unsigned-int)
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
