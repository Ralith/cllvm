(asdf:defsystem :cl-llvm
  :author "Benjamin Saunders"
  :depends-on (:cffi)
  :version "0.1"
  :components
  ((:module "bindings"
    :serial t
    :components ((:file "package")
                 (:file "library")))
   (:module "wrapper"
    :serial t
    :components ((:file "package")))))
