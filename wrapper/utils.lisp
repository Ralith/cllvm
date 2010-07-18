(in-package #:llvm-wrapper)

(defmacro lispifying-errors (function-call)
  (let ((error-name (gensym "ERROR"))
        (error-addr-name (gensym "ERROR-ADDRESS")))
    `(with-foreign-objects ((,error-name '(:pointer :char))
                            (,error-addr-name '(:pointer (:pointer :char))))
       (setf (mem-ref ,error-addr-name :pointer) ,error-name)
       (unwind-protect (when (,@function-call ,error-addr-name)
                         (error (foreign-string-to-lisp ,error-name)))
         (%llvm:dispose-message ,error-name)))))

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
