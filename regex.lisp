

(defclass nfa ()
  ((start-state :initarg :start-state :reader start-state)
   (accepting-state :initarg :accepting-state :reader accepting-state)))


(defun make-nfa (start accept)
  (make-instance 'nfa :start-state start :accepting-state accept))

(defclass nfa-state ()
  ((transitions :initarg :transitions :accessor transitions)
   (entering :initform (lambda (pos) pos) :accessor entering)))

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
     for current-states = states then (remove-duplicates (append current-states new-states))
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

(defun enter-states (states pos)
  (mapc (lambda (s) (funcall (entering s) pos)) states)
  states)

(defun run-nfa (nfa input-string)
  (loop
     with states = (enter-states (epsilon-closure (list (start-state nfa))) 0)
     for input across input-string
     for i upfrom 0
     do
       (setf states (enter-states (reachable-states states input) i))
       (if (null states) (return :reject))
     finally (return (if (member (accepting-state nfa) states)
                         :accept
                         :reject))))



(defvar *pos*)

(defun nfa-from-pattern (pattern)
  (let* ((*pos* 0)
         (nfa (parse-pattern pattern)))
    (unless (eql *pos* (length pattern))
      (error "unexpected end of pattern"))
    nfa))

(defun parse-pattern (pattern)
  (loop
     with nfas = '()
     do (case (elt pattern *pos*)
          (#\* (incf *pos*)
               (push (nfa-star (pop nfas)) nfas))

          (#\| (incf *pos*)
               (setf nfas (list
                           (nfa-alternative (reduce #'nfa-concat (reverse nfas)) (parse-pattern pattern)))))

          (#\( (incf *pos*)
               (let ((start-pos *pos*))
                 (push (make-capture (parse-pattern pattern) (subseq pattern start-pos *pos*)) nfas))
               (unless (and (< *pos* (length pattern)) (eql (elt pattern *pos*) #\)))
                 (error (format nil "expected ) at ~d" *pos*)))
               (incf *pos*))

          (#\) (loop-finish))

          (t (push (nfa-atom (elt pattern *pos*)) nfas)
             (incf *pos*)))

       until (>= *pos* (length pattern))
       finally (return (reduce #'nfa-concat (reverse nfas)))))



(defun make-capture (nfa name)
  (let (start)
    (setf (entering (start-state nfa))
          (lambda (pos) (setf start pos)))
    (setf (entering (accepting-state nfa))
          (lambda (pos) (print (list name start pos)))))
  nfa)
