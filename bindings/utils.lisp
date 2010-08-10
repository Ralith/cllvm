(in-package #:llvm-bindings)

(defun lispify-name (string)
  (loop for char across string
        with last-char-upcased = t
        with output = ""
        do (when (and (upper-case-p char)
                      (not last-char-upcased))
             (setf output (concatenate 'string output "-")))
           (setf output (concatenate 'string output (string char)))
           (setf last-char-upcased (upper-case-p char))
        finally (return (string-upcase output))))

(defmacro definstr (c-name &body args)
  `(defcfun (,(intern (concatenate 'string "BUILD-" (lispify-name c-name))) ,(concatenate 'string "LLVMBuild" c-name)) value-ref
     (builder builder-ref)
     ,@args))

(defmacro defvinstr (c-name &body args)
  `(definstr ,c-name
     ,@args
     (name :string)))

(cffi:define-foreign-type freed-string () ()
  (:actual-type :pointer))

(defmethod translate-from-foreign (pointer (type freed-string))
  (prog1 (foreign-string-to-lisp pointer)
    (foreign-string-free pointer)))

(defmethod translate-to-foreign (value (type freed-string))
  (foreign-string-alloc value))

(defmethod free-translated-object (pointer (type freed-string) param)
  (declare (ignore param))
  (foreign-string-free pointer))

(define-parse-method freed-string ()
  (make-instance 'freed-string))
