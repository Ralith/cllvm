(in-package #:llvm-bindings)

(defcenum verifier-failure-action
  :abort-process
  :print-message
  :return-status)

(defcfun (verify-module "LLVMVerifyModule") :boolean
  (module module-ref)
  (action verifier-failure-action)
  (out-message (:pointer (:pointer :char))))

(defcfun (verify-function "LLVMVerifyFunction") :boolean
  (function value-ref)
  (action verifier-failure-action))
