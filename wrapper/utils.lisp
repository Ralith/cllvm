(in-package #:llvm-wrapper)

(defmacro with-double-ptr ((inner outer c-type) &body body)
  "Creates a pointer INNER (ostensibly of type C-TYPE) which is addressed by OUTER.  Only OUTER is deallocated.
This is intended for use with functions which set take a pointer to a pointer and set the inner pointer's value to the address of some C-TYPE.  Note that no typechecking is done."
  `(let ((,inner (make-pointer 0)))
     (with-foreign-object (,outer '(:pointer ,c-type))
       (setf (mem-ref ,outer :pointer) ,inner)
       ,@body)))

(defmacro lispifying-errors (function-call)
  (let ((error-name (gensym "ERROR"))
        (error-addr-name (gensym "ERROR-ADDRESS")))
    `(with-double-ptr (,error-name ,error-addr-name (:pointer :char))
       (unwind-protect (when (,@function-call ,error-addr-name)
                         (error (foreign-string-to-lisp ,error-name)))
         (%llvm:dispose-message ,error-name)))))

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
