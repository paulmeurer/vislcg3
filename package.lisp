(in-package :cl-user)

(defpackage #:vislcg3
  #+ignore
  (:export #:fst-tokenize #:fst-map-tokens #:fst-net #:fst-tokenizer #:fst-pattern-matcher
	   #:token-boundary #:fst-lookup #:fst-apply-patterns #:set-pattern-match-type)
  (:use #:cl #:cffi #:acl-compat.mp #:cl-fst #:utils))

:eof