(defun make-relation (l1 l2)
  (loop for e1 in l1 append
    (loop for e2 in l2 collect (list e1 e2))))


(defun make-integer-range (lower upper)
  (loop for i from lower to upper collect i))


(defun make-repeated-seq (seq elt-count)
  (loop
    with seq-len = (length seq)
    for i from 0 below elt-count
    collect (nth (mod i seq-len) seq)))


(defun filter (pred l)
  (loop for e in l
    when (funcall pred e)
    collect e))


(defun positive-p (n)
  (> n 0))


(defun negative-p (n)
  (< n 0))


(defun power-set (set)
  (if (null set)
      '(())
    (let ((pset (power-set (cdr set))))
      (append pset (mapcar (lambda (x) (cons (car set) x)) pset)))))


(defmacro sum (seq) `(reduce '+ ,seq))


(defun collapse-equal-neighbors-recursive (seq)
  (cond ((null seq) '())
        ((eq (car seq) (cadr seq)) (collapse-equal-neighbors (cdr seq)))
        (t (cons (car seq) (collapse-equal-neighbors (cdr seq))))))


(defun collapse-equal-neighbors (seq)
  (reverse
   (reduce (lambda (distinct elt) (if (eq elt (car distinct))
                                      distinct
                                    (cons elt distinct)))
           seq
           :initial-value nil)))


(defun tally-equal-neighbors (seq)
  (mapcar 'cdr
          (reverse
           (reduce (lambda (tallies elt) (if (eq elt (caar tallies))
                                             (progn
                                               (incf (cdar tallies))
                                               tallies)
                                           (cons (cons elt 1) tallies)))
                   seq
                   :initial-value nil))))


;;;  (c) Copyright Gerald Roylance 1983, 1984, 1985, 1986
;;;      All Rights Reserved.
;;;  This file may be distributed noncommercially provided
;;;  that this notice is not removed.


(defparameter factor-k      30.)
(defparameter factor-trials '(     2.  3.  5.  7. 11. 13. 17. 19. 23. 29.))
(defparameter factor-sieve  '( 1.              7. 11. 13. 17. 19. 23. 29.))

(defun factor (n)
  (declare (special factor-k factor-trials factor-sieve))
  (do ((limit   (isqrt n))
       (trials  factor-trials)
       (sieve   factor-sieve)
       (k       factor-k)
       (factors nil)
       (i0      0)
       (i       0))
      ((> i limit)
       (nreverse (cons n factors)))

    (cond ((null trials)
	   (setq i0 (+ i0 k))
	   (setq trials sieve)))

    (setq i (+ i0 (pop trials)))

    (do ()
	((or (not (zerop (mod n i)))
	     (> i limit)))
      (push i factors)
      (setq n (floor n i))
      (setq limit (isqrt n)))
    ))
