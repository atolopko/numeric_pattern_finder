(defmacro arg (n) `(nth ,n args))

;(defmacro arg-eval (n) `(ps-eval (nth ,n args)))

;; extends a partial argument list with a pattern-stream argument; the
;; new pattern-stream argument is initialized with a simplified
;; sequence (but no args of its own)
(defun extend-args-with-patterns (patt partial-args nseq &rest disallowed-patterns)
  (let ((simplified-seq (pattern-simplify patt (nseq-seq nseq) partial-args))
        (patterns (set-difference *patterns* disallowed-patterns)))
    (loop for pattern in patterns collect
      (append partial-args
              (list (ps-no-args pattern simplified-seq))))))

(defun extend-args-with-pattern-streams (patt partial-args nseq &rest pattern-streams)
;  (let ((simplified-seq (pattern-simplify patt (nseq-seq nseq) partial-args)))
  (loop for ps in pattern-streams 
    do (setf (ps-nseq ps) (analyze-seq nil))
    collect (append partial-args (list ps))))


;(defmacro arg ((name symbol)) `(nth (position ',name (patt-params patt)) args))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some concrete patterns



;;; basic pattern

;; The basic pattern produces a sequence of sequential integers,
;; starting at 0.  For BACON fans, think of it as the producer of
;; independent variables.

(defpattern basic-patt ())

(defmethod pattern-term ((patt basic-patt) ps i args) i)

(defmethod pattern-confidence ((patt basic-patt) (nseq nseq))
  (if (loop
        for i from 0
        for elt in (nseq-seq nseq)
        always (= i elt))
      1.0
    0.0))
    

(defmethod pattern-simplify ((patt basic-patt) seq args) seq)

(defmethod pattern-sugg-args ((patt basic-patt) (nseq nseq) partial-args) nil)



;;; non-negative integers pattern

;; including this pattern should greatly increase search speed, as it
;; often occurs, and we will not need to find the (add-patt 1
;; (basic-patt)) pattern

(defpattern basic1-patt ())

(defmethod pattern-term ((patt basic1-patt) ps i args) (1+ i))

(defmethod pattern-confidence ((patt basic1-patt) (nseq nseq))
  (if (loop
        for i from 1
        for elt in (nseq-seq nseq)
        always (= i elt))
      1.0
    0.0))

(defmethod pattern-simplify ((patt basic1-patt) seq args) seq)

(defmethod pattern-sugg-args ((patt basic1-patt) (nseq nseq) partial-args) nil)


;;; constant pattern

(defpattern constant-patt (n))

(defmethod pattern-term ((patt constant-patt) ps i args) (arg 0))

(defmethod pattern-confidence ((patt constant-patt) (nseq nseq))
  (if (nseq-constant nseq) 1.0 0.0))

(defmethod pattern-simplify ((patt constant-patt) seq args)
  (make-list (length seq) :initial-element (arg 0)))

(defmethod pattern-sugg-args ((patt constant-patt) (nseq nseq) partial-args)
  (list (list (nseq-first nseq))))
;;; addition

(defpattern add-patt (a b))

(defmethod pattern-term ((patt add-patt) ps i args)
  (+ (ps-eval (arg 0) i) (ps-eval (arg 1) i)))

(defmethod pattern-confidence ((patt add-patt) (nseq nseq))
  ;; is seq constant, and non-zero?
  (if (nseq-constant nseq)
      0.0
    (+
     ;; +0.50 if the first elt is non-zero (likely translation)
     (if (zerop (nseq-first nseq))
         0.0 
         (if (nseq-monotonic nseq)
             ;; if monotonic and first elt not zero, this is a likely
             ;; candidate for translation
             0.50
           0.30))
     ;; +0.20 if sequence does not cross zero
     (if (or (< (nseq-max nseq) 0)
             (> (nseq-min nseq) 0)) 0.20 0.0)
     ;; +0.20 if the mean is not "close" to zero
     (if (< (abs (/ (nseq-mean nseq) (nseq-range nseq))) 0.10) 0.0 0.20))))
       

(defmethod pattern-simplify ((patt add-patt) seq args)
  (assert (= (length args) 1) nil "expected 1 arg")
  (loop for elt in seq collect (- elt (arg 0))))


(defmethod pattern-sugg-args ((patt add-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest first arg
         (remove '(0)
                 (list (list (- (nseq-first nseq)))
                       (list (nseq-min nseq))
                       (list (nseq-max nseq))
                       (list (floor (nseq-mean nseq)))
                       (list (ceiling (nseq-mean nseq)))
                       (list (nseq-mode nseq)))))
               ;; TODO: other suggestions?
         ;(mapcar 'list (make-integer-range 1 5)))
        ((null (second partial-args))
         ;; suggest second arg, which should never be a constant
         (extend-args-with-patterns patt
                                  partial-args
                                  nseq
                                  patt))))



;;; multiplication

(defpattern mult-patt (factor b))

(defmethod pattern-term ((patt mult-patt) ps i args)
  (* (ps-eval (arg 0) i) (ps-eval (arg 1) i)))

(defmethod pattern-confidence ((patt mult-patt) (nseq nseq))
  ;; is this "cheating"?
  (if (and
       (every 'integerp (nseq-seq nseq))
       (> (apply 'gcd (nseq-seq nseq)) 1))
      1.0 0.0))


(defmethod pattern-simplify ((patt mult-patt) seq args)
  (assert (= 1 (length args)) nil "expecting first arg to be initialized")
  (assert (not (zerop (arg 0))) nil "mult-patt factor arg must not be zero")
  (loop for elt in seq collect (/ elt (arg 0))))

(defmethod pattern-sugg-args ((patt mult-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest first arg s
         ;; all integer divisors <= min elt
         (mapcar 'list (remove-duplicates
                        (mapcar (lambda (l) (apply '* l))
                                (remove nil
                                        (power-set
                                         (factor (apply 'gcd (nseq-seq nseq)))))))))
        ((null (second partial-args))
         ;; suggest second arg, which should never be a constant
         (extend-args-with-patterns patt
                                  partial-args
                                  nseq
                                  patt))))

;;; negate pattern

(defpattern neg-patt (a))

(defmethod pattern-term ((patt neg-patt) ps i args) (- (ps-eval (arg 0) i)))

(defmethod pattern-confidence ((patt neg-patt) (nseq nseq))
  (cond ((<= (nseq-max nseq) 0) 1.0)
        ((eq (nseq-monotonic nseq) 'decreasing) 0.50)
        (t 0.0)))
      
(defmethod pattern-simplify ((patt neg-patt) seq args)
  (loop for elt in seq collect (- elt)))

(defmethod pattern-sugg-args ((patt neg-patt) (nseq nseq) partial-args)
  (extend-args-with-patterns patt
                           partial-args
                           nseq
                           patt))


;;; add2 pattern

;; combines (adds) two pattern streams

(defpattern add2-patt (a b))

(defmethod pattern-term ((patt add2-patt) ps i args)
  (+ (ps-eval (arg 0) i) (ps-eval (arg 1) i)))

;; no reasonable heuristic exists for to assign a confidence to this
;; pattern; we just have to try it...
(defmethod pattern-confidence ((patt add2-patt) (nseq nseq))
  (cond ((< (nseq-size nseq) 3) 0.0)
        (t 0.20)))

;; simplifies the seq iff the first arg is an executable pattern stream
(defmethod pattern-simplify ((patt add2-patt) seq args)
  (cond ((null args) seq)
        ((ps-executable-p (first args))
         (loop
           with ps = (first args)
           initially (ps-reset ps)
           for i from 0 below (length seq)
           for term = (ps-next ps)
           for elt in seq
           collect (- elt term)))
        (t (error
            "add2-patt can only be asked to simplify a seq when its 1st arg is an executable ps"))))
           
  
(defmethod pattern-sugg-args ((patt add2-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest first arg, which is a pattern
         ;;
         ;; TODO: we need to suggest *arbitrary* patterns, not ones
         ;; that necessarily simplifoy our sequence (bottom-up!)
         (apply
          'extend-args-with-pattern-streams
          patt
          partial-args
          nseq
          ;; hehe.  we use patterns we've seen *before* to guess what
          ;; the user might expect us to use in our add2-patt!
          *pattern-memory*))
        ;; suggest 2nd args
        ((null (second partial-args))
         (assert (ps-executable-p (first partial-args)) nil
                 "add2-patt should not be asked to suggest 2nd arg unless 1st arg is an *executable* ps")
         ;; 2nd arg can never be a constant
         (append
          (extend-args-with-patterns patt
                                     partial-args
                                     nseq
                                     ;; stop the insanity!
                                     patt)
          ;; and we'll want to use our memorized patterns for 2nd arg, as well!
          (pattern-sugg-args patt nseq nil)))))


;;; mult2 pattern

;; multiplies two pattern streams

(defpattern mult2-patt (a b))

(defmethod pattern-term ((patt mult2-patt) ps i args)
  (* (ps-eval (arg 0) i) (ps-eval (arg 1) i)))

;; no reasonable heuristic exists for to assign a confidence to this
;; pattern; we just have to try it...
(defmethod pattern-confidence ((patt mult2-patt) (nseq nseq))
  (cond ((< (nseq-size nseq) 3) 0.0)
        ;; if there exists a memorized pattern that actually works
        ;; (divides each element), then we should assume a reasonable
        ;; chance of this being a good search direction to follow (and
        ;; better than add2-patt, which cannot rely upon divisibility
        ;; property)
        ((pattern-sugg-args patt nseq nil) 0.6)
        (t 0.0)))

;; simplifies the seq iff the first arg is an executable pattern stream
(defmethod pattern-simplify ((patt mult2-patt) seq args)
  (cond ((null args) seq)
        ((ps-executable-p (first args))
         (loop
           with ps = (first args)
           initially (ps-reset ps)
           for i from 0 below (length seq)
           for term = (ps-next ps)
           for elt in seq
           collect (if (zerop term) 0 (/ elt term))))
        (t (error
            "mult2-patt can only be asked to simplify a seq when its 1st arg is an executable ps"))))
           
  
(defmethod pattern-sugg-args ((patt mult2-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest first args, which are themselves patterns
         (let* ((patterns *pattern-memory*)
                (args
                 (loop
                   with seq = (nseq-seq nseq)
                   for candidate-arg1-ps in patterns
                   for divides =
                   (loop 
                     for i from 0 to (nseq-size nseq)
                     for elt in seq
                     for term = (ps-term candidate-arg1-ps i) do
                     always (and (not (zerop term))
                                 (zerop (mod elt term))))
                   if divides
                   collect candidate-arg1-ps)))
           ;; TODO: we need to suggest *arbitrary* patterns, not ones
           ;; that necessarily simplify our sequence (bottom-up!)
           ;;
           ;; hehe.  we use patterns we've seen *before* to guess what
           ;; the user might expect us to use in our add2-patt!
           (apply
            'extend-args-with-pattern-streams
            patt
            partial-args
            nseq
            args)))
        ;; suggest 2nd args
        ((null (second partial-args))
         (assert (ps-executable-p (first partial-args)) nil
                 "mult2-patt should not be asked to suggest 2nd arg unless 1st arg is an *executable* ps")
         ;; suggest second args (never a constant value)
         (append
          (extend-args-with-patterns patt
                                     partial-args
                                     nseq
                                     ;; stop the insanity!
                                     patt)
          ;; and we'll want to use our memorized patterns for 2nd arg, as well!
          (pattern-sugg-args patt nseq nil)))))


;;; add previous pattern

;; generates pattern whose terms are the previous term some plus some
;; argument (which might itself be a pattern!)

(defpattern add-prev-patt (initial a))

(defmethod pattern-term ((patt add-prev-patt) ps i args)
  (if (zerop i)
      (arg 0)
    (+ (prev-term) (ps-eval (arg 1) (1- i)))))

(defmethod pattern-confidence ((patt add-prev-patt) (nseq nseq))
  (if (< (nseq-size nseq) 2)
      0.0
    ;; is sequence of diffs simpler than the sequence itself?
    (let ((simplified-nseq
           (analyze-seq (pattern-simplify patt (nseq-seq nseq) '()))))
      (+
       ;; does range decrease?
       (if (< (nseq-range simplified-nseq)
              (nseq-range nseq))
           0.45 0.0)
       ;; do we end up with fewer distinct values in the simplified
       ;; sequence? (perhaps a repeating, underlying pattern)
       (if (< (nseq-distinct-count simplified-nseq) 10)
           0.45 0.0)))))

;; simplify by taking the diff between neighboring terms:
;; (0 1 3 4 4 4 5 7 8 8 8 9 11 12 12 12 13 15 16 16)
;;   (1 2 1 0 0 1 2 1 0 0 1 2  1  0  0  1  2  1  0)
;; note: args will always be '() because pattern takes only a single arg
(defmethod pattern-simplify ((patt add-prev-patt) seq args)
  (loop
    for a in seq
    for b in (cdr seq)
    collect (- b a)))

;; note: args should always be streams, because a constant arg is
;; equivalent to mult-patt
(defmethod pattern-sugg-args ((patt add-prev-patt) (nseq nseq) partial-args)
  (mapcar (lambda (arg2) (cons (nseq-first nseq) arg2))
          (extend-args-with-patterns patt
                                     partial-args
                                     nseq)))


;;; add previous 2 pattern

;; a term is the sum of the previous 2 terms; only the first 2 terms
;; are parameterized; args must be literals (not pattern streams)

(defpattern add-prev2-patt (t1 t2))

(defmethod pattern-term ((patt add-prev2-patt) ps i args)
  (assert (and (numberp (first args)) (numberp (second args)))
          nil "args to add-prev2-patt must be numbers")
  (cond ((= i 0) (arg 0))
        ((= i 1) (arg 1))
        (t (+ (prev-term 1) (prev-term 2)))))

;; this pattern must be a leaf ps; we can verify immediately whether
;; this pattern matches the simplified sequence (it's cheaper to
;; perform exact verification, rather than "guessing" via some
;; heuristic, because the resultant node that gets enqueued may take a
;; while before it is processed
(defmethod pattern-confidence ((patt add-prev2-patt) (nseq nseq))
  (cond
   ;; this pattern only applies when we have at least 3 elements in
   ;; our seq
   ((< (nseq-size nseq) 3) 0.0)
   (t
    ;; verify that the sequence matches the pattern (we don't really
    ;; need a heuristic, since this is a base-level pattern, meaning
    ;; it can never have nested patterns)
    (let ((seq (nseq-seq nseq)))
      (if (loop
            for a in seq
            for b in (cdr seq)
            for c in (cddr seq)
            always (eq (+ a b) c))
          1.0
        0.0)))))

;; no simplification necessary (pattern must be a leaf, since we have
;; no subpatterns!)
(defmethod pattern-simplify ((patt add-prev2-patt) seq args)
  nil)

(defmethod pattern-sugg-args ((patt add-prev2-patt) (nseq nseq) partial-args)
  ;; we can only guess that our args are the first 2 elements in the provided seq!
  (list (subseq (nseq-seq nseq) 0 2)))


;;; mult previous pattern

;; generates pattern whose terms are the previous term multiplied
;; by the terms of some other pattern stream

(defpattern mult-prev-patt (initial a))

(defmethod pattern-term ((patt mult-prev-patt) ps i args)
  (if (zerop i)
      (arg 0)
    (* (prev-term) (ps-eval (arg 1) (1- i)))))

(defmethod pattern-confidence ((patt mult-prev-patt) (nseq nseq))
  (cond ((< (nseq-size nseq) 2) 0.0)
        ;; does each term divide the next term?
        ((loop
           for elt1 in (nseq-seq nseq)
           for elt2 in (cdr (nseq-seq nseq))
           always (and (not (zerop elt1))
                       (zerop (mod elt2 elt1))))
         1.0)
        (t 0.0)))

;; simplify by taking the multiplication factor between neighboring terms
;; note: args will always be '() because pattern takes only a single arg
(defmethod pattern-simplify ((patt mult-prev-patt) seq args)
  (loop
    for elt1 in seq
    for elt2 in (cdr seq)
    collect (/ elt2 elt1)))

;; note: args should always be streams, because a constant arg is
;; equivalent to mult-patt
(defmethod pattern-sugg-args ((patt mult-prev-patt) (nseq nseq) partial-args)
  (mapcar (lambda (arg2) (cons (nseq-first nseq) arg2))
          (extend-args-with-patterns patt
                                     partial-args
                                     nseq
                                     basic-patt)))



;;; alternate pattern

;; combines two sub-patterns via an alternating merge of patterns a
;; and b, where pattern a is repeated len1 times, then pattern b is
;; repeated len2 times, and so on (len=1 and len2=1 is perhaps the
;; most common usage)

(defpattern alt-patt (len1 len2 a b))

(defmethod pattern-term ((patt alt-patt) ps i args)
  (let* ((len1 (arg 0))
         (len2 (arg 1))
         (cycle-len (+ len1 len2))
         (completed-cycles (truncate (/ i cycle-len))))
    (if (< (mod i cycle-len) len1)
        (ps-eval (arg 2) (+ (* completed-cycles len1) (mod i cycle-len)))
      (ps-eval (arg 3) (+ (* completed-cycles len2) (- (mod i cycle-len) len1))))))

(defmethod pattern-confidence ((patt alt-patt) (nseq nseq))
  (if (< (nseq-size nseq) 4)
      0.0
    (loop
      for alt-len-args in (pattern-sugg-args patt nseq '())
      maximize
      (let* ((seq (nseq-seq nseq))
             ;; a little unusual to call pattern-simplify from
             ;; pattern-confidence, but it has the exact code we need to
             ;; use!
             (nseq1 (analyze-seq (pattern-simplify patt seq (list (first alt-len-args) (second alt-len-args)))))
             (nseq2 (analyze-seq (pattern-simplify patt seq (list (first alt-len-args) (second alt-len-args) t)))))
        (cond
         ;; we need the alternating pattern to appear at least
         ;; twice, otherwise we won't assume the pattern exists
         ((< (nseq-size nseq) (* 2 (+ (first alt-len-args) (second alt-len-args))))
          0.0)
         ;; is either (or both) of the split sequences constant?  (this is
         ;; a very strong sign of an alternating pattern!)
         ((and (not (nseq-constant nseq))
               (or (nseq-constant nseq1)
                   (nseq-constant nseq2)))
          0.9)
         ;; do the two merged sequences lie on disjoint ranges?
         ((or (< (nseq-max nseq1) (nseq-min nseq2))
              (< (nseq-max nseq2) (nseq-min nseq1)))
          0.8)
         ;; is inflection count reduced when the seq is split into two?
         ((< (+ (nseq-inflection-count nseq1) (nseq-inflection-count nseq2))
             (nseq-inflection-count nseq))
          0.7)
         ;; is range reduced for each of the split sequences?
         ((< (+ (nseq-range nseq1) (nseq-range nseq2))
             (nseq-range nseq))
          0.5)
         (t 0.2))))))
;; TODO: could consider R^2 values; this is a good measure of
;; whether the sequences are linear, but with different slopes


;; simplify by splitting into 2 sequences
(defmethod pattern-simplify ((patt alt-patt) seq args)
  ;; note the returned simplified sequence is dependent upon which
  ;; argument has yet to be determined (arg a or b): if arg a not yet
  ;; specified, we generate the simplified sequence for the first
  ;; pattern stream arg, a; otherwise we generate the simplified
  ;; sequence for the second parttern stream arg, b
  (loop
    with want-odd-elts = (= (length args) 2)
    and seq-len = (length seq)
    and cycle-len = (+ (arg 0) (arg 1))
    and len1 = (arg 0)
    and len2 = (arg 1)
    for i from 0 to seq-len
    for elt in seq do
    when (and want-odd-elts (< (mod i cycle-len) len1))
    collect elt
    when (and (not want-odd-elts) (>= (mod i cycle-len) len1))
    collect elt))


;; note: it is probably common for the two underlying sequences to be
;; quite similar, and differ by a single subpattern (e.g. negation).
;; How should we capture this heuristic?  Or do we need to?  Can't we
;; assume that if the system can find the solution to *one* of the
;; subpatterns, then it should also find the solution to the other
;; subpattern (if it's nearly the same one)?
(defmethod pattern-sugg-args ((patt alt-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest args for the alternating pattern repeat lengths
         ;; TODO: should compute more combinations (beam-search would
         ;; be useful here)
         '((1 1) (1 2) (2 1) (2 2) (3 1) (1 3) (3 2) (2 3) (3 3)))
        (t
         ;; we first want to suggest constant values for the repeating
         ;; elements, then pattern streams; the only constants that make
         ;; sense are the 1st and 2nd elts of the simplified seq
         (cons
          ;; the constant arg value that we suggest depends upon whether
          ;; we're being asked to provide the first or second argument
          (append partial-args
                  (if (= (length partial-args) 2)
                      (list (first (nseq-seq nseq)))
                    (list (second (nseq-seq nseq)))))
          ;; now suggest the pattern stream args; any combination of pattern
          ;; streams can be used to produce our alternating pattern
          (extend-args-with-patterns patt
                                   partial-args
                                   nseq
                                    ;; nested alt-patterns are not
                                    ;; inconceivable, but let's rule them out
                                    ;; for sake of reducing the search space
                                    alt-patt)))))


;;; (repeating) cycle pattern

;; an example of a pattern that maintains state!  note that since freq
;; can be a pattern, the cycle length can change, which explains why
;; we don't see a modulo operator here (which would imply a fixed
;; cycle length)
;;
;; note that this pattern is not intelligent enough to discern cycles
;; that do not start at the beginning of the sequence
;;
;; TODO: the way we *should* handle cycles is to simplify them by
;; making each cycle repetition into single elt (a list!); but this
;; would entail a "meta-level" jump up in system complexity (every
;; other pattern would have to handle *list* elts, in addition to
;; integer elts)
(defpattern cycle-patt (freq a))

(defmethod pattern-term ((patt cycle-patt) ps i args)
 (progn
   (when (or (null (ps-var 'index))
             (>= (ps-var 'index) (ps-var 'count)))
     (ps-set-var 'index 0)
     (ps-set-var 'count (ps-next (arg 0)))
     (assert (not (zerop (ps-var 'count))) nil
             "cycle-patt's freq arg must be positive (if freq arg is a pattern stream, it must return only positive-valued terms)"))
   (ps-set-var 'index (1+ (ps-var 'index)))
   (ps-eval (arg 1) (1- (ps-var 'index)))))

(defmethod pattern-confidence ((patt cycle-patt) (nseq nseq))
  (cond
   ((< (nseq-size nseq) 4) 0.0)
   ;; a constant sequence is *not* a cycle (implies distinct-count > 1)
   ((nseq-constant nseq) 0.0)
   ;; if it's monotonic, repeating cycles are not likely (but they are
   ;; still possible)
   ((nseq-monotonic nseq) 0.20)
   ;; a sequence with only 2 cycles would be a poor example, so we'll
   ;; assume a cycle is not the underlying pattern in this case (note
   ;; that cycles are delimited by 2 inflection points)
   ((< (nseq-inflection-count nseq) 2) 0.20)
   ;; for a cycle pattern, we expect to see each term repeated at least twice
   ((> (nseq-distinct-count nseq) (/ (nseq-size nseq) 2)) 0.20)
   ;; for a cycle pattern, we expect to see each term repeated at
   ;; least twice; the more times these distinct terms are repeated,
   ;; the more confident we'll be that we have a fixed cycle; our
   ;; "scale" maxes out after 5 repetitions of distinct terms
   (t (* 0.9 (log (min 5 (/ (nseq-size nseq)
                            (nseq-distinct-count nseq))) 5)))))

;; to simplify, we return the *longest* *complete* cycle in our
;; subsequence (determined by the largest term returned by the freq
;; param, which may be either a pattern stream or a literal); since
;; the seq is finite in length, we can always find the longest cycle
;; within it; returning the longest cycle is what we want, because
;; every cycle, regardless of its length, is guaranteed to be a prefix
;; of the longest cycle
(defmethod pattern-simplify ((patt cycle-patt) seq args)
  (cond ((= (length args) 0)
         ;; can't simplify when no args have been suggested
         seq)
        ((= (length args) 1)
         (apply 'subseq seq
                ;; the following loop returns a list containing the start and
                ;; end positions (inclusive, exclusive) of the longest
                ;; complete cycle in the sequence
                (loop with max-cycle-pos = 0 and max-cycle-len = 0
                  for i from 0
                  for cycle-len = (ps-eval (arg 0) i)
                  for pos = (+ (or pos 0) cycle-len)
                  if (> pos (length seq))
                  return (list max-cycle-pos (+ max-cycle-pos max-cycle-len))
                  else if (> cycle-len max-cycle-len) do
                  (setf max-cycle-len cycle-len)
                  (setf max-cycle-pos (- pos cycle-len)))))))

(defmethod pattern-sugg-args ((patt cycle-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest cycle frequencies
         ;;
         ;; TODO: should look for distance between inflection
         ;; points and return most confident cycle lengths
         ;; first
         ;;
         ;; TODO: note that we're only suggesting constant
         ;; values for the cycle length arg!  (i.e., the cycle
         ;; lengths we suggest will always be fixed); but we'd
         ;; need to suggest *arbitrary* patterns, not ones
         ;; that necessarily simplify our sequence
         ;; (bottom-up!)
         (append
          ;; constant cycle lengths
          (loop
            for cycle-len from 2 upto (truncate (/ (nseq-size nseq) 2))
            collect (list cycle-len))
          ;; pattern cycle lengths
          (apply
           'extend-args-with-pattern-streams
           patt
           partial-args
           nseq
           ;; hehe.  we use patterns we've seen *before* to guess what
           ;; the user might expect us to use in our cycle-patt!
           ;; note: we can only return pattern streams that generate
           ;; positive values!
           (remove-if (lambda (ps) (notevery 'positive-p (ps-gen ps (nseq-size nseq))))
                      *pattern-memory*))))
        (t
         ;; 2nd arg should never be a constant, since that would make
         ;; cycle-patt the same as the identity-patt; furthermore,
         ;; shouldn't return pattern streams that we know will return
         ;; non-positive numbers
         (extend-args-with-patterns patt
                                  partial-args
                                  nseq
                                  basic-patt))))

         
;;; repeat pattern

;; cycle length can be constant or a pattern stream; each cycle has a
;; constant value, but the value can change between cycles, which is
;; determined by a second pattern stream

(defpattern repeat-patt (freq a))

(defmethod pattern-term ((patt repeat-patt) ps i args)
  (progn
    ;; initialization
    (when (null (ps-var 'repeated-val-index))
      (when (pattern-stream-p (arg 0)) (ps-reset (arg 0)))
      (ps-set-var 'repeat-index 0) ; the "inner" loop index
      (ps-set-var 'repeat-count (ps-next (arg 0)))
      (ps-set-var 'repeated-val-index 0)) ; the "outer" loop index
    ;; reset after each cycle
    (when (>= (ps-var 'repeat-index)
              (ps-var 'repeat-count))
      (ps-set-var 'repeat-index 0)
      (ps-set-var 'repeat-count (ps-next (arg 0)))
      (ps-set-var 'repeated-val-index (1+ (ps-var 'repeated-val-index))))
    (let ((val (ps-eval (arg 1) (ps-var 'repeated-val-index))))
      (ps-set-var 'repeat-index (1+ (ps-var 'repeat-index)))
      val)))

(defmethod pattern-confidence ((patt repeat-patt) (nseq nseq))
  (cond
   ((< (nseq-size nseq) 4) 0.0)
   ;; we need to see at least 2 occurrences of each value (we'll allow
   ;; for the case where one of the cycles is incomplete)
   ((>= (* (1- (nseq-distinct-count nseq)) 2) (nseq-size nseq)) 0.0)
   ;; constant sequences are another degenerate case
   ((nseq-constant nseq) 0.0)
   ;; there must always be at least 2 neighboring terms that are the same
   ;((/= (nseq-first nseq) (elt (nseq-seq nseq) 1)) 0.0)
   (t
    (loop
      for elt1 in (nseq-seq nseq)
      for elt2 in (cdr (nseq-seq nseq))
      for elt3 in (cddr (nseq-seq nseq))
      always (or (= elt1 elt2) (= elt2 elt3))) 0.8)))

(defmethod pattern-simplify ((patt repeat-patt) seq args)
  (cond ((null args)
         ;; our first arg determines the *length* of each repeated
         ;; cycle, so we'll simplify by tranforming our sequence into
         ;; one that represents the consecutive counts of equal
         ;; elements
         (tally-equal-neighbors seq))
        (t
         ;; our second arg determines the *value* of each repeated cycle
         ;; so we'll simplify by extracting 1 term from each cycle
         (collapse-equal-neighbors seq))))

(defmethod pattern-sugg-args ((patt repeat-patt) (nseq nseq) partial-args)
  (cond ((null partial-args)
         ;; suggest 1st args
         (append
          ;; we'll guess a fixed cycle length equal to the longest repeating
          ;; cycle of a single value
          (list (list (apply 'max (tally-equal-neighbors (nseq-seq nseq)))))
          ;; and we'll guess that the cycle length might be a sequence itself!
          (extend-args-with-patterns patt
                                     partial-args
                                     nseq)))
        ;; suggest 2nd args
        (t
         (extend-args-with-patterns patt
                                    partial-args
                                    nseq))))



;;; fixed cycle pattern

;; since cycles are fixed in length, we don't care about determining
;; the underlying pattern that defines the terms within a cycle; we'll
;; just assume we can find some *arbitrary* subsequence that repeats

;; note: this eliminates the need for the rise-fall-patt, which is a
;; special case of this pattern

(defpattern fixed-cycle-patt (cycle-elts))

(defmethod pattern-term ((patt fixed-cycle-patt) ps i args)
  ;; the first (and only) arg is a *list* of the elements in the
  ;; repeating cycle
  (nth (mod i (length (arg 0))) (arg 0)))

(defmethod pattern-confidence ((patt fixed-cycle-patt) (nseq nseq))
  (cond
   ;; a constant sequence is *not* a cycle (implies distinct-count > 1)
   ((nseq-constant nseq) 0.0)
   ;; if it's monotonic, fixed cycles are not possible
   ((nseq-monotonic nseq) 0.0)
   ;; a sequence with only 2 cycles would be a poor example, so we'll
   ;; assume a cycle is not the underlying pattern in this case; 
;   ((< (nseq-inflection-count nseq) 2) 0.0)
   ;; we determine precisely whether a fixed-cycle pattern exists in
   ;; our sequence and we can simply use our pattern-sugg-args
   ;; function to figure this out
   ((car (pattern-sugg-args patt nseq '())) 1.0)
   (t 0.0)))

;; no simplification necessary; this is a base sequence
(defmethod pattern-simplify ((patt fixed-cycle-patt) seq args) nil)

(defmethod pattern-sugg-args ((patt fixed-cycle-patt) (nseq nseq) partial-args)
  (assert (null partial-args) nil
          "pattern-sugg-args for fixed-cycle-patt should always be called with 0 partial-args")
  ;; find the shortest repeating cycle that works
  (list (loop
          with seq = (nseq-seq nseq)
          with len = (nseq-size nseq)
          for i from 2 to (1+ (/ (nseq-size nseq) 2))
          for cycle-elts = (subseq seq 0 i) do
          when (equal seq (make-repeated-seq cycle-elts len))
          return (list cycle-elts))))


;;; root pattern

(defpattern root-patt (a) 'unregistered)

(defmethod pattern-term ((patt root-patt) ps i args) (ps-eval (arg 0) i))

(defmethod pattern-confidence ((patt root-patt) (nseq nseq)) 1.0)

(defmethod pattern-simplify ((patt root-patt) seq args) seq)

(defmethod pattern-sugg-args ((patt root-patt) (nseq nseq) partial-args)
  (extend-args-with-patterns patt nil nseq))
