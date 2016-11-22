;;; sequence analysis heuristics
;;;
;;; Calculates various metrics for a sequence of numbers.  These
;;; metrics will be used to determine confidence values for pattern
;;; applicability (used in each pattern's pattern-confidence method).

(defstruct nseq ()
  (seq)
  (size)
  (first)
  (max)
  (min)
  (range)               ; (- max min)
  (max-pos)
  (min-pos)
  (median)
  (mean)
  (mode)
;  (bias)                ; sum of deltas (shows rising/falling trend)
  (distinct-count)
  (inflection-count)
  (constant)
  (monotonic))          ; nil, 'increasing, 'decreasing

(defparameter *default-nseq* (make-nseq))

   
(defun analyze-seq (seq)
  (if (null seq)
      *default-nseq*
    (let ((max-val (apply 'max seq))
          (min-val (apply 'min seq)))
      (make-nseq
       :seq seq
       :size (length seq)
       :first (car seq)
       :max max-val
       :min min-val
       :range (- max-val min-val)
       :max-pos (position max-val seq)
       :min-pos (position min-val seq)
       :median (elt seq (truncate (/ (length seq) 2)))
       :mean (/ (reduce #'+ seq) (length seq))
       :mode (seq-mode seq)
;     :bias (sum (mapcar '- (cdr seq) seq))
       :inflection-count (seq-inflection-count seq)
       :distinct-count (seq-distinct-number-count seq)
       :constant (loop with first = (car seq) for elt in (cdr seq) do always (eq elt first))
       :monotonic (seq-monotonicity seq)))))


(defun seq-mode (seq)
  (do* ((seq (sort (copy-seq seq) #'<) (cdr seq))
        (mode)
        (mode-n 0)
        (curr-val (car seq))
        (n 0))
      ((null seq) mode)
    (if (= curr-val (car seq))
        (incf n)
      (progn (setq curr-val (car seq))
             (setq n 1)))
    (when (> n mode-n)
      (setq mode (car seq))
      (setq mode-n n))))

(defun seq-distinct-number-count (seq)
  (do* ((seq (sort (copy-seq seq) #'<) (cdr seq))
        (n 0)
        (curr-val nil))
      ((null seq) n)
    (unless (equal curr-val (car seq))
      (incf n)
      (setq curr-val (car seq)))))


(defun seq-inflection-count (seq)
  (third
   (reduce (lambda (acc val)
             (cond ((and (eq (first acc) 'up)
                         (< val (second acc)))
                    (list 'down val (1+ (third acc))))
                   ((and (eq (first acc) 'down)
                         (> val (second acc)))
                    (list 'up val (1+ (third acc))))
                   ((and (eq (first acc) 'even)
                         (not (= val (second acc))))
                    (list (if (> val (second acc)) 'up 'down) val (third acc)))
                   (t (list (first acc) val (third acc)))))
           seq
           :initial-value (list 'even (car seq) 0))))


;; returns nil if sequence is not monotonic
;; otherweise, returns 'increasing, 'decreasing, or 'constant
(defun seq-monotonicity (seq)
  (cond ((< (length seq) 2) nil)
        ((zerop (seq-inflection-count seq))
         (cond ((> (elt seq (1- (length seq))) (elt seq 0)) 'increasing)
               ((< (elt seq (1- (length seq))) (elt seq 0)) 'decreasing)
               (t nil)))))



