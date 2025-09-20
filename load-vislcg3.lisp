(in-package :vislcg3)


;; adapt to your environment

(define-foreign-library libcg3
  (:darwin
   (:default "/usr/local/lib/libcg3"))
  (:unix
   (:default "/usr/local/lib64/libcg3")))

(use-foreign-library libcg3)

:eof
