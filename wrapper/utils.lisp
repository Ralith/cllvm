(in-package #:llvm-wrapper)
(declaim (optimize (debug 3)))

(defmacro with-double-ptr ((inner outer c-type) &body body)
  "Creates a pointer INNER (ostensibly of type C-TYPE) which is addressed by OUTER."
  `(with-foreign-objects ((,outer '(:pointer ,c-type)))
     (setf (mem-ref ,outer :pointer) (null-pointer))
     (symbol-macrolet ((,inner (mem-ref ,outer :pointer)))
       ,@body)))

(defmacro lispifying-errors (function-call)
  (let ((error-name (gensym "ERROR"))
        (error-addr-name (gensym "ERROR-ADDRESS")))
    `(with-double-ptr (,error-name ,error-addr-name (:pointer :char))
       (when (,@function-call ,error-addr-name)
         (error (unwind-protect (foreign-string-to-lisp ,error-name)
                  (%llvm:dispose-message ,error-name)))))))

(defmacro with-pointers-to-list ((array length list) &body body)
  (let ((list-name (gensym "LIST")))
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
