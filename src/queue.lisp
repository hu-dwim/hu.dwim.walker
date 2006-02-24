;; -*- lisp -*-

(in-package :it.bese.arnesi)

(defclass queue ()
  ((head-index :accessor head-index)
   (tail-index :accessor tail-index)
   (buffer :accessor buffer)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t :identity t)
    (format stream "~D" (queue-count queue))))

(defmethod initialize-instance :after
    ((queue queue)
     &key
     (size 20)
     (element-type t)
     &allow-other-keys)
  (assert (< 1 size)
          (size)
          "Initial size of a queue must be greater than 1.")
  (setf (head-index queue) 0
        (tail-index queue) 0
        (buffer queue) (make-array (1+ size) :element-type element-type)))

(defmethod enqueue ((queue queue) value)
  (when (queue-full-p queue)
    (grow-queue queue))
  (setf (aref (buffer queue) (head-index queue)) value)
  (move-head queue)
  queue)

(defmethod dequeue ((queue queue) &optional (default-value nil))
  (if (queue-empty-p queue)
      default-value
      (prog1
          (aref (buffer queue) (tail-index queue))
        (move-tail queue))))

(defmethod queue-empty-p ((queue queue))
  (= (head-index queue) (tail-index queue)))

(defmethod queue-full-p ((queue queue))
  (= (mod (tail-index queue) (length (buffer queue)))
     (mod (1+ (head-index queue)) (length (buffer queue)))))

(defmethod grow-queue ((queue queue))
  (let ((new-buffer (make-array (* (length (buffer queue)) 2)
                                :element-type (array-element-type (buffer queue)))))
    (if (< (head-index queue) (tail-index queue))
        ;; growing from the bottom. conceptualy the new elements need
        ;; to go between tail and head. it's simpler to just move them
        ;; all
        (loop
           with num-elements = (queue-count queue)
           for index upfrom 0
           until (queue-empty-p queue)
           do (setf (aref new-buffer index) (dequeue queue))
           finally (setf (tail-index queue) 0
                         (head-index queue) num-elements))
        ;; growing from the top, a simple copy is sufficent
        (loop
           for element across (buffer queue)
           for index upfrom 0
           do (setf (aref new-buffer index) element)))
    (setf (buffer queue) new-buffer)
    queue))

(defmacro incf-mod (place divisor)
  `(setf ,place (mod (1+ ,place) ,divisor)))

(defmethod move-tail ((queue queue))
  (incf-mod (tail-index queue) (length (buffer queue))))

(defmethod move-head ((queue queue))
  (incf-mod (head-index queue) (length (buffer queue))))

(defmethod queue-count ((queue queue))
  (cond
    ((= (head-index queue) (tail-index queue))
     0)
    ((< (tail-index queue) (head-index queue))
     (- (head-index queue) (tail-index queue)))
    ((> (tail-index queue) (head-index queue))
     (- (+ (length (buffer queue)) (head-index queue))
        (tail-index queue)))))
