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
