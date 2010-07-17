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

(defcfun (module-create-with-name "LLVMModuleCreateWithName") module-ref
  (name :string))

(defcfun (dispose-module "LLVMDisposeModule") :void
  (module module-ref))

;;; Operations on all values
(defcfun (get-value-name "LLVMGetValueName") :string
  (value value-ref))
(defcfun (set-value-name "LLVMSetValueName") :void
  (value value-ref)
  (name :string))

;;; Operations on scalar constants
(defcfun (const-int "LLVMConstInt") value-ref
  (integer-type type-ref)
  (value :unsigned-long-long)
  (sign-extend :boolean))

;;; Operations on integer types
(defcfun (int1-type "LLVMInt1Type") type-ref)
(defcfun (int8-type "LLVMInt8Type") type-ref)
(defcfun (int16-type "LLVMInt16Type") type-ref)
(defcfun (int32-type "LLVMInt32Type") type-ref)
(defcfun (int64-type "LLVMInt64Type") type-ref)
(defcfun (int-type "LLVMIntType") type-ref
  (width :unsigned-int))
(defcfun (get-int-type-width "LLVMGetIntTypeWidth") type-ref
  (int-type type-ref))

;;; Operations on functions
(defcfun (add-function "LLVMAddFunction") value-ref
  (module module-ref)
  (name :string)
  (type type-ref))

;;; Operations on parameters
(defcfun (get-param "LLVMGetParam") value-ref
  (function value-ref)
  (index :unsigned-int))

;;; Operations on basic blocks
(defcfun (get-basic-block-parent "LLVMGetBasicBlockParent") value-ref
  (basic-block basic-block-ref))

(defcfun (append-basic-block "LLVMAppendBasicBlock") basic-block-ref
  (function value-ref)
  (name :string))

(defcfun (insert-basic-block "LLVMInsertBasicBlock") basic-block-ref
  (insert-before-bb basic-block-ref)
  (name :string))

(defcfun (delete-basic-block "LLVMDeleteBasicBlock") :void
  (basic-block basic-block-ref))

;;; Operations on phi nodes
(defcfun (add-incoming "LLVMAddIncoming") :void
  (phi-node value-ref)
  (incoming-values (:pointer value-ref))
  (incoming-blocks (:pointer basic-block-ref))
  (count :unsigned-int))
(defcfun (count-incoming "LLVMCountIncoming") :unsigned-int
  (phi-node value-ref))
(defcfun (get-incoming-value "LLVMGetIncomingValue") value-ref
  (phi-node value-ref)
  (index :unsigned-int))
(defcfun (get-incoming-block "LLVMGetIncomingBlock") basic-block-ref
  (phi-node value-ref)
  (index :unsigned-int))

;;;; Instruction builders
(defcfun (create-builder "LLVMCreateBuilder") builder-ref)
(defcfun (position-builder "LLVMPositionBuilder") :void
  (builder builder-ref)
  (basic-block basic-block-ref)
  (instruction value-ref))
(defcfun (position-builder-before "LLVMPositionBuilderBefore") :void
  (builder builder-ref)
  (instruction value-ref))
(defcfun (position-builder-at-end "LLVMPositionBuilderAtEnd") :void
  (builder builder-ref)
  (basic-block basic-block-ref))
(defcfun (get-insert-block "LLVMGetInsertBlock") basic-block-ref
  (builder builder-ref))
(defcfun (insert-into-builder "LLVMInsertIntoBuilder") :void
  (builder builder-ref)
  (instruction value-ref))
(defcfun (insert-into-builder-with-name "LLVMInsertIntoBuilderWithName") :void
  (builder builder-ref)
  (instruction value-ref)
  (name :string))
(defcfun (dispose-builder "LLVMDisposeBuilder") :void
  (builder builder-ref))

;;; Terminators
(defcfun (build-ret-void "LLVMBuildRetVoid") value-ref
  (builder builder-ref))
(defcfun (build-ret "LLVMBuildRet") value-ref
  (builder builder-ref)
  (value value-ref))
(defcfun (build-aggregate-ret "LLVMBuildAggregateRet") value-ref
  (builder builder-ref)
  (values (:pointer value-ref))
  (number :unsigned-int))
(defcfun (build-br "LLVMBuildBr") value-ref
  (builder builder-ref)
  (destination basic-block-ref))
(defcfun (build-cond-br "LLVMBuildCondBr") value-ref
  (builder builder-ref)
  (condition value-ref)
  (true-block basic-block-ref)
  (false-block basic-block-ref))

;;; Arithmetic
(defcfun (build-add "LLVMBuildAdd") value-ref
  (builder builder-ref)
  (lhs value-ref)
  (rhs value-ref)
  (name :string))

(defcfun (build-sub "LLVMBuildSub") value-ref
  (builder builder-ref)
  (lhs value-ref)
  (rhs value-ref)
  (name :string))

(defcfun (build-mul "LLVMBuildMul") value-ref
  (builder builder-ref)
  (lhs value-ref)
  (rhs value-ref)
  (name :string))

(defcfun (build-udiv "LLVMBuildUDiv") value-ref
  (builder builder-ref)
  (lhs value-ref)
  (rhs value-ref)
  (name :string))

(defcfun (build-sdiv "LLVMBuildSDiv") value-ref
  (builder builder-ref)
  (lhs value-ref)
  (rhs value-ref)
  (name :string))

;;; Casts
(defcfun (build-trunc "LLVMBuildTrunc") value-ref
  (builder builder-ref)
  (value value-ref)
  (destination-type type-ref)
  (name :string))