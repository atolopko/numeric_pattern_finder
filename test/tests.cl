(assert (equal (ps-gen (ps basic-patt) 10)
               '(0 1 2 3 4 5 6 7 8 9)))

(assert (equal (ps-gen (ps add-patt 1) 10)
               '(1 2 3 4 5 6 7 8 9 10)))

(assert (equal (ps-gen (ps add-prev-patt 1) 10)
               '(0 1 2 3 4 5 6 7 8 9)) nil)

(assert (equal (ps-gen (ps add-prev2-patt) 10)
               '(1 1 2 3 5 8 13 21 34 55)))

(let ((ps (ps basic-patt)))
  (setf root-node (make-instance 'patt-node :global-ps ps :local-ps ps))
  (assert (node-is-leaf root-node)))

(let ((ps (ps-no-args add-patt)))
  (setf root-node (make-instance 'patt-node :global-ps ps :local-ps ps))
  (assert (not (node-is-leaf root-node))))

(let ((ps (ps add-patt 5 1)))
  (setf root-node (make-instance 'patt-node :global-ps ps :local-ps ps))
  (assert (node-is-leaf root-node)))

;; pattern stream deep copy

;TODO: equal doesn't compare objects... :(
; but I manually verified ps-copy and it seems to be working
;(step(let* ((ps (ps cycle-patt (ps add-patt 1) 8))
;       (ps2 (ps-copy ps)))
;  (assert (equal ps ps2)))
;)
  

;; test reset pattern stream

(let ((ps (ps cycle-patt (ps add-patt 1) (ps add-prev2-patt)))
      (seq1 nil)
      (seq2 nil))
  (setf seq1 (ps-gen ps 20))
  ;; TODO: this isn't really testing what we want unless we change something about the nested pattern
  (ps-reset ps)
  (let ((nested-ps1 (first (ps-args ps)))
        (nested-ps2 (second (ps-args ps))))
    (assert (zerop (ps-next-index nested-ps1)))
    (assert (zerop (ps-next-index nested-ps2)))
    (assert (null (ps-terms nested-ps1)))
    (assert (null (ps-terms nested-ps2)))))

;; test ps-confidence

(let ((ps (ps-some-args add-patt '(1 2 3) 3)))
  (assert (= 1.0 (ps-confidence ps))))

(let ((ps (ps-some-args add-patt '(2 4 6) 2 (ps-some-args mult-patt '(0 2 4) 2))))
  (assert (= (ps-confidence ps) 1.0))) 

(let ((ps (ps-some-args add-patt '(2 4 6) 2 (ps-some-args mult-patt '(0 2 4)))))
  (ps-confidence ps))

(let ((ps (ps-some-args add-patt '(1 2 3) 3)))
  (assert (= 1.0 (ps-confidence ps))))





;; test node-copy

(let* ((ps (ps add-patt 1 2))
       (node (make-instance 'patt-node :ps ps))
       (node2 (node-copy-and-set-args node ps '(1 2))))
  (assert (equal (ps-args (node-ps node2)) '(1 2))))

(let* ((ps (ps add-patt 1 2))
       (node (make-instance 'patt-node :ps ps))
       (node2 (node-copy-and-set-args node ps '(3 4))))
  (assert (equal (ps-args (node-ps node2)) '(3 4))))

(let* ((ps (ps add-patt 1 (ps mult-patt 3)))
       (node (make-instance 'patt-node :ps ps))
       (node2 (node-copy-and-set-args node ps (list 1 (ps mult-patt 4)))))
  (assert (ps-equal (node-ps node2) (ps add-patt 1 (ps mult-patt 4)))))

;; test node-next-uninit-ps

;; expect nil when all args are init'd
(let* ((ps1 (ps add-patt 3 4))
       (node (make-instance 'patt-node :ps ps1)))
  (assert (null (node-next-uninit-ps node))))

(let* ((ps1 (ps-some-args add-patt '() 1))
       (node (make-instance 'patt-node :ps ps1)))
  (assert (eq ps1 (node-next-uninit-ps node))))

;; test nested uninit'd arg
(let* ((ps2 (ps-no-args mult-patt))
       (ps1 (ps add-patt 3 ps2))
       (node (make-instance 'patt-node :ps ps1)))
  (assert (eq ps2 (node-next-uninit-ps node))))

(let* ((ps2 (ps-some-args mult-patt '() 1))
       (ps1 (ps add-patt 3 ps2))
       (node (make-instance 'patt-node :ps ps1)))
  (assert (eq ps2 (node-next-uninit-ps node))))

;; test doubly nested uninit'd arg
(let* ((ps3 (ps-no-args add-prev2-patt))
       (ps2 (ps mult-patt 3 ps3))
       (ps1 (ps add-patt 9 ps2))
       (node (make-instance 'patt-node :ps ps1)))
  (assert (eq ps3 (node-next-uninit-ps node))))





;; test node-expand

(let* ((ps (ps-no-args add-patt))
       (node (make-instance 'patt-node
                            :global-ps ps
                            :local-ps ps))
       (children (node-expand node '(1 2 3) 5)))
  (assert (every (lambda (e) (eq e t))
                 (mapcar 'node-is-leaf children)))
  (assert (every '=
                 (mapcar (lambda (node) (first (node-args node))) children)
                 ; this will change and will break...
                 '(1 2 3 4 5)))
  (mapcar (lambda (node) (ps-gen (node-local-ps node)))
          children))
       
  
                            
