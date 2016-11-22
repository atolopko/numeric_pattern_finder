(defclass pattern ()
  ((name :accessor patt-name :initarg :name)
   (params :accessor patt-params :initarg :params)))

(defgeneric pattern-term (patt ps i args)
  (:documentation "Calculates a term for this pattern, given the arguments"))

(defgeneric pattern-confidence (patt nseq)
  (:documentation "Returns a confidence score indicating how confident
we can be that this pattern can be used to simplify the specified
sequence.  Note that the confidence score is determined without
knowing any of the arguments; the pattern can only use the
simplified sequence itself to determine its level of confidence."))

(defgeneric pattern-simplify (patt seq args)
  (:documentation "Simplifies the specified sequence by applying the
inversion of this pattern's term-calculation function."))

(defgeneric pattern-sugg-args (patt nseq partial-args)
  (:documentation "Returns a list of suggested arguments for the next
uninitialized argument, based upon the given sequence and any previous
arguments that have been defined."))

(defparameter *patterns* '())

;; defines a new pattern subclass and instantiates an instance
(defmacro defpattern (name params &optional unregistered)
  `(progn
     (setf *patterns* (delete-if (lambda (patt) (eq (type-of patt) ',name))
                                 *patterns*))
     (defclass ,name (pattern) ())
     (setq ,name (make-instance ',name
                                :name ',name
                                :params '(,@params)))
     (unless ,unregistered
       (setf *patterns* 
             (append *patterns* (list ,name))))))

