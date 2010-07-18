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

(defctype module-provider-ref :pointer
  "Interface used to provide a module to JIT or interpreter.  This is now just a synonym for llvm::Module, but we have to keep using the different type to keep binary compatibility.")

(defctype memory-buffer-ref :pointer
  "Used to provide a module to JIT or interpreter. See the llvm::MemoryBuffer class.")

(defctype pass-manager-ref :pointer)

(defctype use-ref :pointer)

(defbitfield attribute
  :zext-attribute
  :sext-attribute
  :no-return-attribute
  :in-reg-attribute
  :struct-ret-attribute
  :no-unwind-attribute
  :no-alias-attribute
  :by-val-attribute
  :nest-attribute
  :read-none-attribute
  :read-only-attribute
  :no-inline-attribute
  :always-inline-attribute
  :optimize-for-size-attribute
  :stack-protect-attribute
  :stack-protect-req-attribute
  (:alignment #.(ash 31 16))
  (:no-capture-attribute #.(ash 1 21))
  :no-red-zone-attribute
  :no-implicit-float-attribute
  :naked-attribute
  :inline-hint-attribute
  :stack-alignment)

(defcenum call-conv
  (:c 0)
  (:fast 8)
  (:cold 9)
  (:x86-stdcall 64)
  (:x86-fastcall 65))

(defcenum int-predicate
  (:eq 32)
  :ne
  :ugt
  :uge
  :ult
  :ule
  :sgt
  :sge
  :slt
  :sle)

(defcenum real-predicate
  :false
  :oeq
  :ogt
  :oge
  :olt
  :ole
  :one
  :ord
  :uno
  :ueq
  :ugt
  :uge
  :ult
  :ule
  :une
  :true)

;;;; Error handling
(defcfun (dispose-message "LLVMDisposeMessage") :void
  (message (:pointer :char)))

;;;; Modules

(defcfun (module-create-with-name "LLVMModuleCreateWithName") module-ref
  (name :string))

(defcfun (dispose-module "LLVMDisposeModule") :void
  (module module-ref))

(defcfun (dump-module "LLVMDumpModule") :void
  (module module-ref))

;;;; Types
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

;;; Operations on function types
(defcfun (function-type "LLVMFunctionType") type-ref
  (return-type type-ref)
  (param-types (:pointer type-ref))
  (param-count :unsigned-int)
  (is-var-arg :boolean))

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

;;; Operations on functions
(defcfun (add-function "LLVMAddFunction") value-ref
  (module module-ref)
  (name :string)
  (type type-ref))

(defcfun (set-function-call-conv "LLVMSetFunctionCallConv") :void
  (function value-ref)
  (call-conv call-conv))

;;; Operations on parameters
(defcfun (get-param "LLVMGetParam") value-ref
  (function value-ref)
  (index :unsigned-int))

;;; Operations on basic blocks
(defcfun (get-basic-block-parent "LLVMGetBasicBlockParent") value-ref
  (basic-block basic-block-ref))

(defcfun (get-first-basic-block "LLVMGetFirstBasicBlock") basic-block-ref
  (function value-ref))
(defcfun (get-last-basic-block "LLVMGetLastBasicBlock") basic-block-ref
  (function value-ref))
(defcfun (get-next-basic-block "LLVMGetNextBasicBlock") basic-block-ref
  (basic-block basic-block-ref))
(defcfun (get-previous-basic-block "LLVMGetPreviousBasicBlock") basic-block-ref
  (basic-block basic-block-ref))
(defcfun (get-entry-basic-block "LLVMGetEntryBasicBlock") basic-block-ref
  (function value-ref))

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
(definstr "RetVoid")
(definstr "Ret"
  (value value-ref))
(definstr "AggregateRet"
  (values (:pointer value-ref))
  (number :unsigned-int))
(definstr "Br"
  (destination basic-block-ref))
(definstr "CondBr"
  (condition value-ref)
  (true-block basic-block-ref)
  (false-block basic-block-ref))

;;; Arithmetic
(defvinstr "Add"
  (lhs value-ref)
  (rhs value-ref))

(defvinstr "Sub"
  (lhs value-ref)
  (rhs value-ref))

(defvinstr "Mul"
  (lhs value-ref)
  (rhs value-ref))

(defvinstr "UDiv"
  (lhs value-ref)
  (rhs value-ref))
(defvinstr "SDiv"
  (lhs value-ref)
  (rhs value-ref))

;;; Casts
(defvinstr "Trunc"
  (value value-ref)
  (destination-type type-ref))

;;; Comparisons
(defvinstr "ICmp"
  (predicate int-predicate)
  (lhs value-ref)
  (rhs value-ref))
(defvinstr "FCmp"
  (predicate real-predicate)
  (lhs value-ref)
  (rhs value-ref))

;;; Miscellaneous instructions
(defvinstr "Phi"
  (type type-ref))
(defvinstr "Call"
  (function value-ref)
  (arguments (:pointer value-ref))
  (arguments-length :unsigned-int))
