(in-package :vislcg3)

(defcfun "fdopen" :pointer
  (fd :short)
  (mode :string))

(defcfun "fdclose" :pointer
  (fd :short)
  (mode :string))

(defcfun "fflush" :short
  (stream :ulong))

(defcfun "cg3_init" :uint
  (in :pointer)
  (out :pointer)
  (err :pointer))

(defcfun "cg3_cleanup" :uint)

(defcfun "cg3_grammar_load" :pointer
  (filename :string))

(defcfun "cg3_grammar_free" :void
  (grammar :pointer))

(defctype size_t :ulong)

(defcfun "cg3_applicator_create" :pointer
  (grammar :pointer))

;; void cg3_applicator_setflags(cg3_applicator *applicator, uint32_t flags);

(defcfun "cg3_applicator_setflags" :pointer
  (applicator :pointer)
  (flags size_t))

(defcfun "cg3_applicator_free" :void
  (grammar :pointer))

(defcfun "cg3_sentence_new" :pointer
  (applicator :pointer))

(defcfun "cg3_sentence_runrules" :void
  (applicator :pointer)
  (sentence :pointer))

(defcfun "cg3_sentence_addcohort" :void
  (applicator :pointer)
  (cohort :pointer))

(defcfun "cg3_sentence_numcohorts" size_t
  (sentence :pointer))

(defcfun "cg3_sentence_getcohort" :pointer
  (sentence :pointer)
  (which size_t))

(defcfun "cg3_sentence_free" :void
  (sentence :pointer))

(defcfun "cg3_cohort_create" :pointer
  (sentence :pointer))

(defcfun "cg3_cohort_setwordform" :void
  (cohort :pointer)
  (wordform :pointer))

(defcfun "cg3_cohort_getwordform" :pointer
  (cohort :pointer))

(defcfun "cg3_cohort_setdependency" :pointer
  (cohort :pointer)
  (dep-self size_t)
  (dep-parent size_t))

(defcfun "cg3_cohort_getdependency" :pointer
  (cohort :pointer)
  (dep-self :pointer)
  (dep-parent :pointer))

#+ignore
(defcfun "cg3_cohort_getrelation_u8" :pointer
  (cohort :pointer)
  (rel :pointer)
  (dep-self :pointer)
  (dep-parent :pointer))

#||
void cg3_cohort_setdependency(cg3_cohort *cohort, uint32_t dep_self, uint32_t dep_parent);
void cg3_cohort_getdependency(cg3_cohort *cohort, uint32_t *dep_self, uint32_t *dep_parent);
void cg3_cohort_getrelation_u8(cg3_cohort *cohort_, const char *rel, uint32_t *rel_self, uint32_t *rel_parent);
// The Cohort takes ownership of the Reading here.
||#

(defcfun "cg3_cohort_addreading" :void
  (cohort :pointer)
  (reading :pointer))

(defcfun "cg3_cohort_numreadings" size_t
  (cohort :pointer))

(defcfun "cg3_cohort_numdelreadings" size_t
  (cohort :pointer))

(defcfun "cg3_cohort_getreading" :pointer
  (cohort :pointer)
  (which size_t))

(defcfun "cg3_cohort_getdelreading" :pointer
  (cohort :pointer)
  (which size_t))

(defcfun "cg3_cohort_free" :void
  (cohort :pointer))

(defcfun "cg3_reading_create" :pointer
  (cohort :pointer))


(defcfun "cg3_reading_addtag" :uint
  (reading :pointer)
  (tag :pointer))

(defcfun "cg3_reading_numtags" size_t
  (reading :pointer))

(defcfun "cg3_reading_gettag" :pointer
  (reading :pointer)
  (which size_t))

(defcfun "cg3_reading_numtraces" size_t
  (reading :pointer))

;; uint32_t cg3_reading_gettrace(cg3_reading *reading, size_t which);
(defcfun "cg3_reading_gettrace" size_t
  (reading :pointer)
  (which size_t))

(defcfun "cg3_reading_gettrace_ruletype" size_t
  (reading :pointer)
  (which size_t))

;; This is usually not to be used. The Cohort will take ownership of the Reading and free it on destruction
(defcfun "cg3_reading_free" :void
  (reading :pointer))

#||
#ifdef U_ICU_VERSION_MAJOR_NUM
cg3_tag *cg3_tag_create_u(cg3_applicator *applicator, const UChar *text);
#endif
||#

(defcfun "cg3_tag_create_u8" :pointer
  (applicator :pointer)
  (text :string))

(defcfun "cg3_tag_create_u16" :pointer
  (applicator :pointer)
  (text :pointer))

(defcfun "cg3_tag_create_u32" :pointer
  (applicator :pointer)
  (text :pointer))


;; cg3_tag *cg3_tag_create_u8(cg3_applicator *applicator, const char *text);
;; cg3_tag *cg3_tag_create_u16(cg3_applicator *applicator, const uint16_t *text);
;;c g3_tag *cg3_tag_create_u32(cg3_applicator *applicator, const uint32_t *text);
;; cg3_tag *cg3_tag_create_w(cg3_applicator *applicator, const wchar_t *text);

#||
#ifdef U_ICU_VERSION_MAJOR_NUM
const UChar *cg3_tag_gettext_u(cg3_tag *tag);
#endif
||#

(defcfun "cg3_tag_gettext_u8" :string
  (tag :pointer))

(defcfun "cg3_tag_gettext_u16" :pointer
  (tag :pointer))

(defcfun "cg3_tag_gettext_u32" :pointer
  (tag :pointer))

#||
const char *cg3_tag_gettext_u8(cg3_tag *tag);
const uint16_t *cg3_tag_gettext_u16(cg3_tag *tag);
const uint32_t *cg3_tag_gettext_u32(cg3_tag *tag);
const wchar_t *cg3_tag_gettext_w(cg3_tag *tag);
||#


:eof
