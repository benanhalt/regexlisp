

(defclass nfa ()
  ((start-state :initarg :start-state :reader start-state)
   (accepting-state :initarg :accepting-state :reader accepting-state)))


(defun make-nfa (start accept)
  (make-instance 'nfa :start-state start :accepting-state accept))

(defclass nfa-state ()
  ((transitions :initarg :transitions :accessor transitions)))

(defun make-state (&rest transitions)
  (make-instance 'nfa-state :transitions transitions))

(defclass transition ()
  ((input :initarg :input :reader input)
   (destination :initarg :destination :reader destination)))

(defun make-transition (input dest)
  (make-instance 'transition :input input :destination dest))

(defun add-transition (state input dest)
  (push (make-transition input dest) (transitions state)))

(defun nfa-atom (s)
  (let ((start-state (make-state))
        (accepting-state (make-state)))
    (add-transition start-state s accepting-state)
    (make-nfa start-state accepting-state)))


(defun nfa-concat (a b)
  (add-transition (accepting-state a) :epsilon (accepting-state b))
  (make-nfa (start-state a) (accepting-state b)))

(defun nfa-alternative (a b)
  (let ((start-state (make-state))
        (accepting-state (make-state)))

    (add-transition start-state :epsilon (start-state a))
    (add-transition start-state :epsilon (start-state b))

    (add-transition (accepting-state a) :epsilon accepting-state)
    (add-transition (accepting-state b) :epsilon accepting-state)
    (make-nfa start-state accepting-state)))

(defun nfa-star (a)
  (let ((start-state (make-state))
        (accepting-state (make-state)))

    (add-transition start-state :epsilon (start-state a))
    (add-transition start-state :epsilon accepting-state)

    (add-transition (accepting-state a) :epsilon start-state)
    (add-transition (accepting-state a) :epsilon accepting-state)

    (make-nfa start-state accepting-state)))

(defun reachable-states (state input)
  (labels ((accepts (input)
             (lambda (transition) (or (eq input :epsilon)
                                      (char-equal input (input transition))))))

    (mapcar #'destination (remove-if-not (accepts input) (transitions state)))))

;; (defun run-nfa (nfa input-string)
;;   (loop
;;      with states = (list (start-state nfa))
;;      for i from 0 to (1- (length input)) do
;;        (let ((input (elt input i)))
;;          (setf states
;;                (loop
;;                   for s in states
;;                   appending
;;                     (mapcar #'destination (remove-if-not (accepts input) (transitions s)))))


