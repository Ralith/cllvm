(in-package #:llvm-bindings)

(defctype context-ref :pointer
  "The top-level container for all LLVM global data.  See the LLVMContext class.")

(defctype module-ref :pointer
  "The top-level container for all other LLVM Intermediate Representation (IR) objects. See the llvm::Module class.")

(defctype type-ref :pointer
  "Each value in the LLVM IR has a type, an LLVMTypeRef. See the llvm::Type class.")

(defctype type-handle-ref :pointer
  "When building recursive types using LLVMRefineType, LLVMTypeRef values may become invalid; use LLVMTypeHandleRef to resolve this problem. See the llvm::AbstractTypeHolder class.")

(defctype value-ref :pointer)
(defctype basic-block-ref :pointer)
(defctype builder-ref :pointer)
