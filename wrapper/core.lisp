(in-package #:llvm-wrapper)

(defun function-type (return-type param-types &optional is-var-arg)
  (with-array-of-list (p-t-array length param-types)
    (%llvm:function-type return-type p-t-array length is-var-arg)))

(defun struct-type (element-types &optional packed)
  (with-array-of-list (e-t-array length element-types)
    (%llvm:struct-type e-t-array length packed)))

(defun get-struct-element-types (struct-type &aux
                                 (size (%llvm:count-struct-element-types struct-type)))
  (with-foreign-object (buffer '%llvm:type-ref size)
    (%llvm:get-struct-element-types struct-type buffer)
    (loop for index from 0 below size
          collecting (mem-aref buffer '%llvm:type-ref index))))

(defun const-gep (constant-pointer indices)
  (with-array-of-list (indice-array length indices)
    (%llvm:const-gep constant-pointer indice-array length)))

(defun build-gep (builder pointer indices &optional (name ""))
  (with-array-of-list (indice-array length indices)
    (%llvm:build-gep builder pointer indice-array length name)))

(defun add-incoming (phi-node value block)
  (with-foreign-objects ((value-addr :pointer)
                         (block-addr :pointer))
    (setf (mem-ref value-addr :pointer) value
          (mem-ref block-addr :pointer) block)
    (%llvm:add-incoming phi-node value-addr block-addr 1)))

(wrap-vinstr phi (type))
(defun build-call (builder function arguments &optional (name ""))
  (with-array-of-list (arg-array length arguments)
    (%llvm:build-call builder function arg-array length name)))
