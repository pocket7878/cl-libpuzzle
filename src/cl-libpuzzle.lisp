#|
  This file is a part of cl-libpuzzle project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libpuzzle
  (:use :cl :cffi)
  (:export
    :*similarity-threshold* 
    :*similarity-high-threshold*
    :*similarity-low-threshold*
    :*similarity-lower-threshold*
    #:puzzle-init-context
    #:puzzle-free-context
    #:puzzle-set-max-width
    #:puzzle-set-max-height
    #:puzzle-set-lambdas
    #:puzzle-set-noise-cutoff
    #:puzzle-set-p-ratio
    #:puzzle-set-contrast-barrier-for-cropping
    #:puzzle-set-max-cropping-ratio
    #:puzzle-set-autocrop
    #:puzzle-init-cvec
    #:puzzle-init-dvec
    #:puzzle-fill-dvec-from-file
    #:puzzle-fill-cvec-from-file
    #:puzzle-fill-cvec-from-dvec
    #:puzzle-free-cvec
    #:puzzle-free-dvec
    #:puzzle-dump-cvec
    #:puzzle-dump-dvec
    #:puzzle-cvec-cksum
    #:puzzle-init-compressed-cvec
    #:puzzle-free-compressed-cvec
    #:puzzle-compress-cvec
    #:puzzle-uncompress-cvec
    #:puzzle-vector-sub
    #:puzzle-vector-euclidean-length
    #:puzzle-vector-normalized-distance
    #:with-context-cvecs))

(in-package :cl-libpuzzle)

(define-foreign-library libpuzzle
  (:unix (:or "libpuzzle.so.1" "libpuzzle.so"))
  (t (:default "libpuzzle")))

(use-foreign-library libpuzzle)

;;Define size_t type as unsigned int
(defctype size :unsigned-int)

;;Defile Puzzle Error Code
(define-foreign-type puzzle-code-type ()
  ()
  (:actual-type :int)
  (:simple-parser puzzle-code))

(define-condition puzzle-code-error ()
  (($code :initarg :puzzle-code :reader puzzle-error-code))
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream 
                     "libpuzzle function returned error"))))

(defmethod translate-from-foreign (value (type puzzle-code-type))
  (if (zerop value)
    :puzzle-ok
    (error 'puzzle-code-error)))

;;Define DVec
(defcstruct puzzle-dvec
  (size-of-vec size)
  (size-of-compressed-vec size)
  (vec :pointer))

;;Define CVec
(defcstruct puzzle-cvec
  (size-of-vec size)
  (vec :pointer))

;;Define Compressed CVec
(defcstruct puzzle-compressed-cvec
  (size-of-compressed-vec size)
  (vec :pointer))

;;Define Context
(defcstruct puzzle-context
  (puzzle-max-width :unsigned-int)
  (puzzle-max-height :unsigned-int)
  (puzzle-lambdas :unsigned-int)
  (puzzle-p-ratio :double)
  (puzzle-noise-cutoff :double)
  (puzzle-contrast-barrier-for-cropping :double)
  (puzzle-max-cropping-ratio :double)
  (puzzle-enable-autocrop :int)
  (magic :unsigned-long))

;;Puzzle init context
(defcfun "puzzle_init_context" :void
  (context :pointer))

(defcfun "puzzle_free_context" :void
  (context :pointer))

(defcfun "puzzle_set_max_width" puzzle-code
  (context :pointer)
  (width :unsigned-int))

(defcfun "puzzle_set_max_height" puzzle-code
  (context :pointer)
  (height :unsigned-int))

(defcfun "puzzle_set_lambdas" puzzle-code
  (context :pointer)
  (lambdas :unsigned-int))

(defcfun "puzzle_set_noise_cutoff" puzzle-code
  (context :pointer)
  (noise-cutoff :double))

(defcfun "puzzle_set_p_ratio" puzzle-code
  (context :pointer)
  (p-ratio :double))

(defcfun "puzzle_set_contrast_barrier_for_cropping" puzzle-code
  (context :pointer)
  (barrier :double))

(defcfun "puzzle_set_max_cropping_ratio" puzzle-code
  (context :pointer)
  (ratio :double))

(defcfun "puzzle_set_autocrop" puzzle-code
  (context :pointer)
  (enable :int))

(defcfun "puzzle_init_cvec" :void
  (context :pointer)
  (cvec :pointer))

(defcfun "puzzle_init_dvec" :void
  (context :pointer)
  (dvec :pointer))

(defcfun "puzzle_fill_dvec_from_file" puzzle-code
  (context :pointer)
  (dvec :pointer)
  (file :string))

(defcfun "puzzle_fill_cvec_from_file" puzzle-code
  (context :pointer)
  (cvec :pointer)
  (file :string))

(defcfun "puzzle_fill_cvec_from_dvec" puzzle-code
  (context :pointer)
  (cvec :pointer)
  (dvec :pointer))

(defcfun "puzzle_free_cvec" :void 
  (context :pointer)
  (cvec :pointer))

(defcfun "puzzle_free_dvec" :void 
  (context :pointer)
  (dvec :pointer))

(defcfun "puzzle_dump_cvec" puzzle-code
  (context :pointer)
  (cvec :pointer))

(defcfun "puzzle_dump_dvec" puzzle-code
  (context :pointer)
  (dvec :pointer))

(defcfun "puzzle_cvec_cksum" puzzle-code
  (context :pointer)
  (cvec :pointer)
  (sum  :pointer))

(defcfun "puzzle_init_compressed_cvec" :void
  (context :pointer)
  (compressed-cvec :pointer))

(defcfun "puzzle_free_compressed_cvec" :void
  (context :pointer)
  (compressed-cvec :pointer))

(defcfun "puzzle_compress_cvec" puzzle-code
  (context :pointer)
  (compressed-cvec :pointer)
  (cvec :pointer))

(defcfun "puzzle_uncompress_cvec" puzzle-code
  (context :pointer)
  (compressed-cvec :pointer)
  (cvec :pointer))

(defcfun "puzzle_vector_sub" puzzle-code
  (context :pointer)
  (cvec1 :pointer)
  (cvec2 :pointer)
  (fix-for-texts :int))

(defcfun "puzzle_vector_euclidean_length" :double
  (context :pointer)
  (cvec :pointer))

(defcfun "puzzle_vector_normalized_distance" :double
  (context :pointer)
  (cvec1 :pointer)
  (cvec2 :pointer)
  (fix-for-texts :int))

(defvar *similarity-threshold* 0.6)
(defvar *similarity-high-threshold* 0.7)
(defvar *similarity-low-threshold* 0.3)
(defvar *similarity-lower-threshold* 0.2)

(defmacro with-context-cvecs (context cvecs &body body)
  `(with-foreign-objects ((,context 'puzzle-context)
                          ,@(loop for cvec in cvecs
                                  collect
                                  `(,cvec 'puzzle-cvec)))
     (unwind-protect
       (progn
         (puzzle-init-context ,context)
         ,@(loop for cvec in cvecs
                 collect
                 `(puzzle-init-cvec ,context ,cvec))
         ,@body)
       ,@(loop for cvec in cvecs
               collect
               `(puzzle-free-cvec ,context ,cvec))
       (puzzle-free-context ,context))))
