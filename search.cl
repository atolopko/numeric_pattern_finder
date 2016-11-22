:;;;;;;;;;;;;;;;;;;;
;;; node base class
  
(defclass node () ())

(defgeneric node-search (goal &optional ration))

(defgeneric node-expand (node))

(defgeneric node-match (node goal))

(defgeneric node-is-leaf (node))

(defparameter *next-unique-id* 1000)

(defun next-unique-id ()
  (incf *next-unique-id*))
  


;;;;;;;;;;;;;;;;;;;;;;
;;; pattern node class

(defclass patt-node (node)
  ((ps :accessor node-ps :initarg :ps)
   ;; the overall confidence for this node (determined by examining
   ;; the partially-initialized pattern streams)
   (id :reader node-id :initform (next-unique-id))
   (parent :accessor node-parent :initform nil)
   (confidence :accessor node-confidence :initform 1.0)
   (ration :accessor node-ration :initarg :ration :initform 0)))

(defmethod print-object ((node patt-node) stream)
  (format stream
          "<NODE ID:~A/~A R=~A C=~A PS:~A>"
          (node-id node)
          (if (node-parent node)
              (node-id (node-parent node)) nil)
          (node-ration node)
          (node-confidence node)
          (node-ps node)))

(defun make-node (ps ration)
  (make-instance 'patt-node :ps ps :ration ration))

(defmethod node-equal ((node1 patt-node) (node2 patt-node))
  (ps-equal (node-ps node1) (node-ps node2)))


;; distributes the parent-node's ration to each of the frontier-nodes,
;; proportional to their confidence values
(defun distribute-ration (parent-node frontier-nodes)
  ;; calc a confidence normalizer that we can use to
  ;; distribute rations to our new frontier nodes
  (let ((confidence-normalizer
         (float (reduce '+ (mapcar 'node-confidence frontier-nodes)))))
    ;; distribute the parent node's ration to the newly created
    ;; nodes, proportional to their confidence values
    (assert (or (null frontier-nodes)
                (not (zerop confidence-normalizer)))
            nil
            "should not have confidence-normalizer=0 if there are any frontier-nodes~%node=~A"
            parent-node)
    (loop
      with ration = (node-ration parent-node)
      for new-node in frontier-nodes do
      (setf (node-ration new-node)
            (truncate
             (* ration (/ (node-confidence new-node)
                          confidence-normalizer))))
      collect new-node)))


;; Finds the next uninit'd argument in the node's ps tree, determines
;; all possible values for that argument, and returns a set of new
;; nodes, one per argument.  Across multiple invocations, this method
;; generates a set of nodes that represents the cross product of all
;; the arguments for a pattern stream tree.
(defmethod node-expand ((node patt-node))
  (let ((ps (node-next-uninit-ps node)))
    (if (null ps)
        nil
      (loop
        with new-node
        for arg-set in (reverse
                        (pattern-sugg-args (ps-patt ps)
                                           (ps-nseq ps)
                                           (ps-args ps) ))
        do
        (assert (consp arg-set) nil
                "sugg-args-fn must return a list of lists")
        (setf
         ;; make a copy of the node, which will have newly set arguments
         ;; for one of its (nested) pattern streams
         new-node (node-copy-and-set-args node ps arg-set)
         ;; set the overall confidence of this node
         (node-confidence new-node) (ps-confidence (node-ps new-node))
         ;; link the new node to its parent
         (node-parent new-node) node)
        collect new-node))))


(defmethod node-next-uninit-ps ((node patt-node))
  (ps-next-uninit-ps (node-ps node)))
        

;; copies the node, and also updates a nested pattern stream with the
;; specified arguments; doesn't copy the node's ration value, as this
;; will be set later
(defmethod node-copy-and-set-args ((node patt-node) (ps pattern-stream) args)
  (make-instance 'patt-node
                 :ps (ps-copy (node-ps node) ps args)))


(defmethod node-match ((node patt-node) seq)
  (equal (ps-gen (node-ps node) (length seq))
         seq))

;; a pattern node is a leaf if its pattern is ready to be generated,
;; which is the case only when there are no more uninitialized args
;; (at any level of nesting)
(defmethod node-is-leaf ((node patt-node))
  (ps-executable-p (node-ps node)))


;;;;;;;;;;
;;; search

(defstruct search-result
  (goal-seq)
  (goal-node)
  (ration)
  (starved-q)
  (processed-q)
  (internal-node-count)
  (leaf-node-count)
  (pruned-node-count))

(defun make-root-node (seq ration)
  (make-node (ps-no-args root-patt seq) ration))


;; contains the main loop for searching the solution space; returns
;; the search tree nodes, so that we can reuse them in subsequent
;; iterations (and for analysis)
(defmethod node-search (goal &optional (ration 10000))
  (setf *next-unique-id* 0)
  (do* (;; queue containing nodes to be expanded (for breadth-first search)
        (q (make-queue (list (make-root-node goal ration))))
        ;; queue containing frontier-nodes that have since been expanded
        (processed-q (make-queue))
        ;; nodes with confidence 0
        (pruned-nodes nil)
        ;; nodes that are fully initialized and are potential goal
        ;; states (reset each iteration)
        (executable-nodes nil nil)
        ;; nodes that could be expanded, but ration=0; we can restart
        ;; our search from these nodes if we fail to find a goal with
        ;; initial ration
        (starved-q (make-queue))
        ;; nodes to be expanded (reset each iteration)
        (frontier-nodes nil nil)
        (internal-node-count 0)
        (pruned-node-count 0)
        (leaf-node-count 0)
        (node (dequeue q) (dequeue q)))
      ((null node)
       ; queue was empty, so we're done, but w/o success!
       (make-search-result :goal-seq goal
                           :goal-node nil
                           :ration ration
                           :starved-q starved-q
                           :processed-q processed-q
                           :internal-node-count internal-node-count
                           :leaf-node-count leaf-node-count
                           :pruned-node-count pruned-node-count))

    (assert (> (node-ration node) 0) nil
            "unexpectedly encountered a node with ration=0.0")
    ;(format t "### expanding node ~A~%" node)
    (loop
      for new-node in (node-expand node) do
      (if (node-is-leaf new-node)
          (push new-node executable-nodes)
        (if (zerop (node-confidence new-node))
            (progn
              ;(push new-node pruned-nodes)
              (incf pruned-node-count))
          (push new-node frontier-nodes))))

    ;; TODO: if > 1 frontier node claims confidence of 1.0, we might
    ;; consider arbitrarily picking only one; seems a bit risky, but
    ;; if the confidence is justified, *either* of them should lead to
    ;; a solution!

    (distribute-ration node frontier-nodes)
    (let ((starved (remove-if-not 'zerop frontier-nodes :key 'node-ration)))
      (enqueue-all starved-q starved)
      (setf frontier-nodes (set-difference frontier-nodes starved)))
    (enqueue-all q frontier-nodes)

    (when *verbose*
      (format t "### expanded node ~A~%into:~%~{   ~A~%~}~%"
              node
              frontier-nodes))

    (enqueue processed-q node)
    (incf internal-node-count)

    ;; determine if any of the executable nodes are a goal state
    (loop for node in executable-nodes do
      (when *verbose*
        (format t "### testing ~A~%" node))
      ;(when (= (node-id node) 3354) (break))
      (incf leaf-node-count)
      (when (node-match node goal)
        (return-from node-search
          (make-search-result :goal-seq goal
                              :goal-node node
                              :ration ration
                              :starved-q starved-q
                              :processed-q processed-q
                              :internal-node-count internal-node-count
                              :leaf-node-count leaf-node-count
                              :pruned-node-count pruned-node-count))))

    ;; output status (occassionally)
    (when (zerop (mod internal-node-count 10))
      (format t "### queue=~A~15Tstarved=~A~30Texpanded=~A~45Tpruned=~A~60Ttested=~A~%"
              (q-size q)
              (q-size starved-q)
              internal-node-count
              pruned-node-count
              leaf-node-count))))

