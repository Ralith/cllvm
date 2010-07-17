(in-package #:llvm-bindings)

(defcenum verifier-failure-action
  :abort-process-action
  :print-message-action
  :return-status-action)

(defcfun (verify-module "LLVMVerifyModule") :boolean
  (module module-ref)
  (action verifier-failure-action)
  (out-message (:pointer (:pointer :char))))
