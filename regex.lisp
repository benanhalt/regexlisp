

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
  (add-transition (accepting-state a) :epsilon (start-state b))
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

(defun epsilon-states (state)
  (mapcar #'destination
          (remove-if-not
           (lambda (tr) (eq :epsilon (input tr)))
           (transitions state))))


(defun epsilon-closure (states)
  (loop
     with new-states = states
     for current-states = states then (append current-states new-states)
     do (setf new-states (remove-if (lambda (s) (member s current-states))
                                    (mapcan #'epsilon-states new-states)))
     until (null new-states)
     finally (return current-states)))


(defun follow-transitions (state input)
  (mapcar
   #'destination
   (remove-if-not (lambda (tr) (equal input (input tr)))
                  (transitions state))))

(defun reachable-states (states input)
  (epsilon-closure
   (mapcan (lambda (state) (follow-transitions state input))
           states)))

(defun run-nfa (nfa input-string)
  (loop
     with states = (epsilon-closure (list (start-state nfa)))
     for input across input-string do
       (setf states (reachable-states states input))
       (if (null states) (return :reject))
     finally (return (if (member (accepting-state nfa) states)
                         :accept
                         :reject))))
