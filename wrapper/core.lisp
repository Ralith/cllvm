(in-package #:llvm-wrapper)

(defmacro with-pointers-to-list ((array length list) &body body)
  (let ((list-name (gensym "list")))
    `(let* ((,list-name ,list)
            (,length (length ,list-name)))
       (with-foreign-object (,array :pointer ,length)
         (loop for index from 0
               for element in ,list-name
               do (setf (mem-aref ,array :pointer index) element))
         ,@body))))

(defmacro wrap-vinstr (name args)
  (let ((full-name-str (concatenate 'string "BUILD-" (symbol-name name))))
    `(defun ,(intern full-name-str) (builder ,@args &optional (name ""))
       (,(intern full-name-str (find-package '#:llvm-bindings))
         builder ,@args name))))

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
