(in-package #:llvm-wrapper)

(defun add-incoming (phi-node value block)
  (with-foreign-objects ((value-addr :pointer)
                         (block-addr :pointer))
    (setf (mem-ref value-addr :pointer) value
          (mem-ref block-addr :pointer) block)
    (%llvm:add-incoming phi-node value-addr block-addr 1)))
