(defparameter *files* '("queue" "util" "seq" "pattern" "pattern-stream"
                        "search" "patterns" "pattern-streams"))

(defparameter *verbose* nil
  "When non-nil, search will output expanded (internal leaf) nodes.")

(defparameter *basic-patterns* nil
  "Some patterns we can use to initialize our *pattern-memory*")

(defparameter *pattern-memory* nil
  "Remembers the patterns that have been solved.  This is something of
a gimmick that allows the system to exhibit a simple form of
learning")

(defun file-source (file-base) (concatenate 'string file-base ".cl"))

(defun file-compiled (file-base) (concatenate 'string file-base ".fas"))

(defun load-app (&optional compile)
  (dolist (file *files*)
    (when compile (compile-file (file-source file)))
    (load (if compile
              (file-compiled file)
            (file-source file))))
  (setf *basic-patterns* (list (ps basic-patt) (ps basic1-patt)))
  (forget-patterns))

(defun solve-seq (seq &optional (ration 10000))
  (setq *result* (node-search seq ration))
  (let ((success (search-result-goal-node *result*)))
    (when success
      (memorize-pattern *result*))
    (format t "~[PATTERN FOUND!~;SORRY!~]~%" (if success 0 1))
    (format t "SEQ:~20T~A~%EXTENDED SEQ:~20T~A~%START RATION:~20T~A~%INTERNAL NODES:~20T~A~%LEAF NODES:~20T~A~%PRUNED NODES::~20T~A~%STARVED NODES:~20T~A~%SOLUTION:~%~A"
            seq
            (if success (extend-seq *result*) nil)
            (search-result-ration *result*)
            (search-result-internal-node-count *result*)
            (search-result-leaf-node-count *result*)
            (search-result-pruned-node-count *result*)
            (q-size (search-result-starved-q *result*))
            (search-result-goal-node *result*))))

(defun extend-seq (result)
  (ps-gen (node-ps (search-result-goal-node result))
          (* 2 (length (search-result-goal-seq result)))))

(defun memorize-pattern (result)
  (push (node-ps (search-result-goal-node result))
        *pattern-memory*))

(defun forget-patterns ()
  (setf *pattern-memory* (copy-list *basic-patterns*)))

(defun recite-patterns ()
  (dolist (ps *pattern-memory*) (format t "~A~%" (ps-gen ps))))


