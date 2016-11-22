(defparameter *basic-patt-stream* (make-instance 'pattern-stream :patt basic-patt))

;;; example (complex) pattern streams

;(setq fib-ps (ps add-prev2-patt 1 1))
;
;(setq neg-fib-plus-2-ps (ps neg-patt (ps add-patt 2 fib-ps)))
;
;;(setq rise-fall-ps (ps rise-fall-patt 6))
;(setq rise-fall-ps (ps fixed-cycle-patt '(0 1 2 3 2 1 0)))
;
;(setq fixed-cycle-ps (ps cycle-patt 5 (ps add-patt 4)))
;
;(setq lengthening-cycle-ps (ps cycle-patt (ps add-patt 1) (ps add-patt 1)))
;
;(setq lengthening-fib-ps (ps cycle-patt (ps add-patt 1) (ps add-prev2-patt 1 1)))
;
;(setq value-repeat-ps (ps repeat-patt 5 (ps add-patt 1)))
;
;(setq alt-negate-ps (ps alt-patt (ps neg-patt)))
;
;(setq alt-negate-offset2-ps (ps alt-patt (ps neg-patt) (ps add-patt 2)))
;
;
;;; not doing what I want; I need a cross between cycle-patt and repeat-patt?
;;(setq rise-fall-cycle-length-ps (ps cycle-patt
;;                                    (ps mult-patt 2 (ps add-patt 3))
;;                                    (ps rise-fall-patt (ps identity-patt i))))
;
;(setq lengthening-repeat-ps (ps repeat-patt (ps add-patt 1) (ps add-patt 1)))
;
;;; is the first term correct?
;(setq rise-fall-increment-ps (ps add-prev-patt (ps fixed-cycle-patt '(0 1 2 1 0))))
;
;(setq rise-fall-pos-neg-increment-ps (ps add-prev-patt (ps neg-patt (ps rise-fall-patt 5))))
;
;(setq rising-basic-cycle-ps (ps add-patt (ps cycle-patt 4) (ps repeat-patt 4)))
;
;(setq nested-repeats-ps (ps repeat-patt (ps repeat-patt 3 (ps add-patt 1))))
