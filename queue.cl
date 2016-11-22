(defclass queue ()
  ((head :accessor q-head :initform '())
   (tail :accessor q-tail :initform '())
   (size :accessor q-size :initform 0)))


(defun make-queue (&optional elts)
  (let ((q (make-instance 'queue)))
    (enqueue-all q elts)))


(defmethod enqueue ((q queue) elt)
  (cond ((null (q-head q))
         (setf (q-tail q) (list elt))
         (setf (q-head q) (q-tail q)))
        (t
         (setf (cdr (q-tail q)) (list elt))
         (setf (q-tail q) (cdr (q-tail q)))))
  (incf (q-size q))
  q)


(defmethod enqueue-all ((q queue) elts)
  (dolist (elt elts)
    (enqueue q elt))
  q)


(defmethod dequeue ((q queue))
  (when (not (emptyp q))
    (decf (q-size q)))
  (cond ((null (q-head q)) nil)
        ((eq (q-head q) (q-tail q))
         (let ((elt (car (q-head q))))
           (setf (q-tail q) nil)
           (setf (q-head q) nil)
           elt))
        (t
         (let ((elt (car (q-head q))))
           (setf (q-head q) (cdr (q-head q)))
           elt))))


(defmethod emptyp ((q queue))
  (null (q-head q)))


(defmethod queue-copy ((q queue))
  (make-queue (q-head q)))

