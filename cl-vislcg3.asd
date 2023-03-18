(in-package :cl-user)

(asdf:defsystem cl-vislcg3
  :depends-on (cffi acl-compat utilities fst)
  :serial t
  :components
  ((:file "package")
   (:file "load-vislcg3")
   (:file "vislcg3-api")
   (:file "cl-vislcg3")
   ))

:eof
