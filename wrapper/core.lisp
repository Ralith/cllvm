(in-package #:llvm-wrapper)

(defun function-type (return-type param-types &optional is-var-arg)
  (with-pointers-to-list (p-t-array length param-types)
    (%llvm:function-type return-type p-t-array length is-var-arg)))

(defun add-incoming (phi-node value block)
  (with-foreign-objects ((value-addr :pointer)
                         (block-addr :pointer))
    (setf (mem-ref value-addr :pointer) value
          (mem-ref block-addr :pointer) block)
    (%llvm:add-incoming phi-node value-addr block-addr 1)))

(wrap-vinstr phi (type))
(defun build-call (builder function arguments &optional (name ""))
  (with-pointers-to-list (arg-array length arguments)
    (%llvm:build-call builder function arg-array length name)))
