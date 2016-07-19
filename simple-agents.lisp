;;;;
;;;; Author Mark Royer
;;;;
;;;; see package description in defpackage.lisp

(in-package :simple-agents)

;;;;<Point Definition> Start

(defstruct (point (:print-function print-point)) 
  "Represents a two dimensional point as an (x,y) coordinate."
  (x 0 :type integer)
  (y 0 :type integer))

(defun print-point (p stream depth)
"Display a point formatted as (x,y)."
  depth
  (format stream "(~A,~A)" (point-x p) (point-y p)))

;;;; Point Definition End

;;;;<Grid Definition> Start
(defclass Grid ()
  ((x :type integer
      :reader grid-x
      :initarg :x
      :initform 5
      :documentation "The width of the Grid.")
   (y :type integer
      :reader grid-y
      :initarg :y
      :initform 5
      :documentation "The height of the Grid.")
   (arr :type simple-array
        :documentation "A 2d array of objects that are in the Grid."))
  (:documentation 
"Represents a two dimensional map that elements may be placed on."))
  

;;;; Initialize the Grid
(defmethod initialize-instance :after ((g Grid) &rest args)
  args ; Just to make the compiler not give a warning
  (setf (slot-value g 'arr) 
        (make-array (list (grid-x g) (grid-y g)) :initial-element nil)))


(defgeneric display-lines  (g stream)
  (:documentation 
"Displays a horizontal line that is the length of the width of the grid.

@parameter g Grid, the grid to display the line
@parameter stream, the stream to print to
@return nil"))
(defmethod display-lines ((g Grid) stream)
  (let ((n (grid-x g)))
    (format stream "~& ")
    (loop for z from 0 to (* n 2)
          do
          (format stream "~A" "-"))
    (format stream "~%"))
  nil)

;;; Display a grid on the given stream
(defmethod print-object ((g Grid) stream)
  (let ((arr (slot-value g 'arr)) (spot nil))
    (display-lines g stream)
    (loop for y downfrom (- (grid-y g) 1) to 0
	  do
	  (format stream "~A|" y)
	  (loop for x from 0 to (- (grid-x g) 1)
		do
		(setq spot (aref arr x y))
		(cond ((null spot)
		       (format stream " |" ))
		      (t
		       (print-object spot stream)
		       (format stream "|"))))
	  (display-lines g stream)))
  
  (format stream " ")
  (loop for i from 0 to (- (grid-x g) 1)
        do
        (format stream " ~A" i))
  (format stream "~&"))


(defgeneric put-object (g obj p)
  (:documentation
"Put the object on the grid at position (x,y).  Set the old position of 
the object to null if the object was on the board.

@parameter g Grid, the grid to put the object
@parameter obj standard-object, the object to put on the grid
@parameter p point, the (x,y) coordinate the object will be placed
@return Grid, The newly updated grid"))
(defmethod put-object ((g Grid) (obj standard-object) (p point))
  (let ((arr (slot-value g 'arr)) 
        (max-x (grid-x g)) (max-y (grid-y g))
        (x (point-x p)) (y (point-y p)))
    (when 
      ;; Can we actually move to the spot?
      (and (< x max-x) (< y max-y) (>= x 0) (>= y 0) (not (object-at-p g p)))
       
       ;; If the object was already somewhere else on the board set that
       ;; position to null
       (let* ((old-p (get-location g obj))) 
         (when (eq (get-object g old-p) obj)
           (setf (aref arr (point-x old-p) (point-y old-p)) nil)))
         
         ;; Set the board position to contain the object
         (setf (aref arr x y) obj)))
    g) ; Return the new board


(defgeneric get-object (g p)
  (:documentation
"Return the standard-object object at location (x,y) or null if nothing is 
there.

@parameter g Grid, the grid to get the object
@parameter p point, location of the object
@return The object at location p or nil"))
(defmethod get-object ((g Grid) (p point))
  (let ((x (point-x p)) (y (point-y p)))
    (cond 
      ((or (>= x (grid-x g)) (>= y (grid-y g)) (< x 0) (< y 0))
       nil)
      (t
       (aref (slot-value g 'arr) x y)))))

(defgeneric object-at-p (g p)
  (:documentation
"Returns true iff there is an object at location (x,y) on the grid or if the 
location is outside of the bounds of the grid.

@parameter g Grid, the grid to look-up an object on
@parameter p point, the location on the grid
@return true iff an object could move the the location on the grid"))
(defmethod object-at-p ((g Grid) (p point))
  (let ((x (point-x p)) (y (point-y p)))
    (cond 
      
      ;; Return true iff out of bounds
      ((or (>= x (grid-x g)) (>= y (grid-y g)) (< x 0) (< y 0))
       t)
      
      ;; Otherwise check to see if there is an object at the point
      (t
       (not (null (aref (slot-value g 'arr) x y)))))))


(defgeneric get-location (g obj)
  (:documentation 
"Returns the (x,y) location of the object on the Grid.  If the object is not
on the Grid the result is (-1,-1).

@parameter g Grid, the grid to look up the object on
@parameter obj standard-object, the object to find in g
@return point, the location where the object is (x,y) or (-1,-1) if the object
    is not on the graph."))
(defmethod get-location ((g Grid) (obj standard-object))
  (block this-function
    (let ((arr (slot-value g 'arr)))
      (loop for x from 0 to (- (grid-x g) 1) 
            do
            (loop for y from 0 to (- (grid-y g) 1)
                  do
                  (when (eq (aref arr x y) obj)
                    (return-from this-function (make-point :x x :y y))))))
    (make-point :x -1 :y -1)))


(defgeneric in-bounds (g x y)
  (:documentation 
"Returns true iff (x,y) is within the bounds of the grid.

@parameter g Grid
@parameter x integer
@parameter y integer
@return true iff (x,y) is within the bounds of the grid"))
(defmethod in-bounds ((g Grid) (x integer) (y integer))
  (and (<= 0 x) (< x (grid-x g)) (<= 0 y) (< y (grid-y g))))

;;;; End Grid definition

;;;;<Rock Definition> Start
(defclass Rock () ()
  (:documentation "An obstaclethat can be placed at a location in the grid."))

;;; We'll draw rocks just as an X.
(defmethod print-object ((a Rock) stream)
  (format stream "X" ))

;;;; End Rock definition

;;;;<Agent Definition> Start
;;;; Represents an AI agent.


;;;; Directions an agent can look
(defparameter agent-up "^" 
  "Agent is facing up")
(defparameter agent-down "v" 
  "Agent is facing down")
(defparameter agent-left "<" 
  "Agent is facing left")
(defparameter agent-right ">" 
  "Agent is facing right")


;;;; Where the agent has bumped
(defparameter bumped-forward 'forward
  "Agent bumped into a wall to its front")
(defparameter bumped-right 'right
  "Agent bumped into a wall to its right")
(defparameter bumped-left 'left
  "Agent bumped into a wall to its left")
(defparameter bumped-back 'back
  "Agent bumped into a wall to its back")

;;;; Actions the agent can take
(defparameter turn-left 'turn-left
  "Agent should turn left")
(defparameter turn-right 'turn-right
  "Agent should turn right")
(defparameter go-forward 'go-forward
  "Agent should move forward")
(defparameter go-left 'go-left
  "Agent should move left, ie side-step left)")
(defparameter go-right 'go-right
  "Agent should move right, ie side-step right")
(defparameter do-nothing 'do-nothing
  "Agent should take no action")


(defclass Agent ()
  ((direction :writer set-direction
              :reader get-direction
              :initform agent-up
              :initarg :direction
              :documentation 
"The direction the agent is currently facing. We put here only for convenience 
of display. The Agent doesn't actually make use of this information.")
   
   ;; Here we have past sensor for bumping
   (last-bumped :writer set-last-bumped
                :reader get-last-bumped
                :initform nil ; haven't bumped anything yet
                :initarg :last-bumped
                :documentation 
"Represents the last location relative to the agent that it bumped into 
according to its last action."))
  
  (:documentation
"An agent may take actions according to what it senses in the world around it. 
This simple agent is a reflex agent"))


;;; Displays an Agent as a triangle.
(defmethod print-object ((a Agent) stream)
  (format stream "~A" (get-direction a)))

(defgeneric get-location-infront (a g p)
  (:documentation
"Returns the (x,y) coordinate the Agent is looking at. If the location is not
in the grids bounds, then (-1,-1) is returned.

@parameter a Agent
@parameter g Grid
@parameter p point, the location the agent is at
@return point, representing the location in front of the object"))
(defmethod get-location-infront ((a Agent) (g Grid) (p point))
  (let ((dir (get-direction a)) (x (point-x p)) (y (point-y p)) )
    (cond 
      ((equal dir agent-up)
       (setq y (+ y 1)))
      ((equal dir agent-down)
       (setq y (- y 1)))
      ((equal dir agent-left)
       (setq x (- x 1)))
      ((equal dir agent-right)
       (setq x (+ x 1))))
    
    (when (not (in-bounds g x y))
      (setq x -1)
      (setq y -1))
    (make-point :x x :y y)))

(defgeneric turn-left (a)
  (:documentation
"Turns the agent counter clock-wise.

@parameter a Agent
@return nil"))
(defmethod turn-left ((a Agent))
  (let ((dir (get-direction a)))
    (cond 
      ((equal dir agent-up)
       (set-direction agent-left a))
      ((equal dir agent-left)
       (set-direction agent-down a))
      ((equal dir agent-down)
       (set-direction agent-right a))
      ((equal dir agent-right)
       (set-direction agent-up a))))
  
  (set-last-bumped nil a)
  nil)

(defgeneric turn-right (a)
  (:documentation
"Turns the agent clock-wise.

@parameter a Agent
@return nil"))
(defmethod turn-right ((a Agent))
  (let ((dir (get-direction a)))
    (cond ((equal dir agent-up)
	   (set-direction agent-right a))
	  ((equal dir agent-left)
	   (set-direction agent-up a))
	  ((equal dir agent-down)
	   (set-direction agent-left a))
	  ((equal dir agent-right)
	   (set-direction agent-down a))))
  
  (set-last-bumped nil a)
  nil)

(defgeneric forward-p (a g)
  (:documentation
"Returns true iff the Agent can move to the location in front of it.

@parameter a Agent
@parameter g Grid
@return true iff the Agent can move to the location in front of it"))
(defmethod forward-p ((a Agent) (g Grid))
  (let ((p (get-location-infront a g (get-location g a))))
    (not (object-at-p g p))))

(defgeneric backward-p (a g)
  (:documentation
"Returns true iff the Agent can move to the location behind it.

@parameter a Agent
@parameter g Grid
@return true iff the Agent can move to the location behind it"))
(defmethod backward-p ((a Agent) (g Grid))
  (let ((cp (make-instance 'Agent :direction (get-direction a) 
                            :last-bumped (get-last-bumped a))))
        
    (turn-left cp) (turn-left cp)
        
    (let ((p (get-location-infront cp g (get-location g cp))))
      (not (object-at-p g p)))))

(defgeneric left-p (a g)
  (:documentation
"Returns true iff the Agent can move to the location to its left.

@parameter a Agent
@parameter g Grid
@return true iff the Agent can move to the location to its left"))
(defmethod left-p ((a Agent) (g Grid))
  (let ((copy (make-instance 'Agent :direction (get-direction a) 
                             :last-bumped (get-last-bumped a))))
    (set-direction (get-direction a) copy)
    (turn-left copy)
    (let ((p (get-location-infront copy g (get-location g a))))
      (not (object-at-p g p)))))

(defgeneric right-p (a g)
  (:documentation
"Returns true iff the Agent can move to the location to its right.

@parameter a Agent
@parameter g Grid
@return true iff the Agent can move to the location to its right"))
(defmethod right-p ((a Agent) (g Grid))
  (let ((copy (make-instance 'Agent :direction (get-direction a) 
                             :last-bumped (get-last-bumped a))))
    
    (set-direction (get-direction a) copy)
    (turn-right copy)
 
    (let ((p (get-location-infront copy g (get-location g a))))
      (not (object-at-p g p)))))

(defgeneric move-forward (a g)
  (:documentation
"Moves the Agent to the location in front of it if it can move there.

@parameter a Agent
@parameter g Grid
@return nil"))
(defmethod move-forward ((a Agent) (g Grid))
  (if (forward-p a g)
      (let ((p (get-location-infront a g (get-location g a))))
        (put-object g a p)
        (set-last-bumped nil a))
      (set-last-bumped bumped-forward a))
  nil)

(defgeneric move-backward (a g)
  (:documentation
"Moves the Agent to the location behind it if it can move there.

@parameter a Agent
@parameter g Grid
@return nil"))
(defmethod move-backward ((a Agent) (g Grid))
  
  (let ((cp (make-instance 'Agent :direction (get-direction a) 
                           :last-bumped (get-last-bumped a))))
    
    (turn-left cp) (turn-left cp)
  
    (if (backward-p a g)
        (let ((p (get-location-infront cp g (get-location g a))))
          (put-object g a p)
          (set-last-bumped nil a))
        (set-last-bumped bumped-back a))
    nil))


(defgeneric move-left (a g)
  (:documentation
"Moves the Agent to the location to the left if it can move there. ie, the agent
will side-step to its left if possible.

@parameter a Agent
@parameter g Grid
@return nil"))
(defmethod move-left ((a Agent) (g Grid))
  (if (left-p a g)
      (let ((copy (make-instance 'Agent :direction (get-direction a) 
                                 :last-bumped (get-last-bumped a))))
        (turn-left copy)
        (let ((p (get-location-infront copy g (get-location g a))))
          (put-object g a p))
        (set-last-bumped nil a))
      (set-last-bumped bumped-left a))
  nil)

(defgeneric move-right (a g)
  (:documentation
"Moves the Agent to the location to the right if it can move there. ie, the agent
will side-step to its right if possible.

@parameter a Agent
@parameter g Grid
@return nil"))
(defmethod move-right ((a Agent) (g Grid))
  (if (right-p a g)
      (let ((copy (make-instance 'Agent :direction (get-direction a) 
                                 :last-bumped (get-last-bumped a))))
      
        (set-direction (get-direction a) copy)
        (turn-right copy)
        (put-object g a (get-location-infront copy g (get-location g a)))
        (set-last-bumped nil a))
      (set-last-bumped bumped-right a))
   
  
  nil)


(defgeneric make-move (a g)
  (:documentation
"The agent will make a move according to the current grid configuration. It 
will sense and choose an action to perform accordingly.

@parameter a Agent
@parameter g Grid
@return the action that was performed"))
(defmethod make-move ((a Agent) (g Grid))
  (let ((action (get-action a g)))
    (cond 
      ((equal action go-forward)
       (move-forward a g)
       go-forward)
      ((equal action go-left)
       (move-left a g)
       go-left)
      ((equal action go-right)
       (move-right a g)
       go-right)
      ((equal action turn-left)
       (turn-left a)
       turn-left)
      ((equal action turn-right)
       (turn-right a)
       turn-right)
      ((equal action do-nothing)
       do-nothing) ; actually do nothing
      ))
  )



(defgeneric get-action (a g)
  (:documentation
"Sense the Agent's surroundings and get an action.

@parameter a Agent
@parameter g Grid
@return an action"))
(defmethod get-action ((a Agent) (g Grid))
  (rule-matching a (sense a g)))
    
(defgeneric sense (a g)
(:documentation
"Returns a list containing what the agent sensed. For a simple agent this is
two items: 

(car result) is t if the agent can go forward.
(cadr result) is a location the agent last bumped into.

@paramater a Agent
@parameter g Grid
@return list, percept list"))
(defmethod sense ((a Agent) (g Grid))
  (list (forward-p a g) (get-last-bumped a)))

(defgeneric rule-matching (a percept-list)
  (:documentation
"Match the sensed values and match them to some action according to some 
rule.

@parameter a Agent
@parameter percept-list list, what the agent has sensed
@return the rule that matches the percept-list"))
(defmethod rule-matching ((a Agent) percept-list)
  (let ((lst-bumped (cadr percept-list)))
    (cond
      ((not (equal lst-bumped bumped-forward))
       go-forward)
      ((not (equal lst-bumped bumped-right))
       go-right)
      (t
        do-nothing)))) ; Notice this will never happen

;;;; End Agent definition

;;;;<History-Agent Definition> Start

(defclass History-Agent (Agent)
  
  ((last-last-bumped
    :reader get-last-last-bumped
    :writer set-last-last-bumped
    :initform nil
    :documentation "What the agent bumped into two moves ago.")
   
   (corner-checking :type boolean
                    :reader get-corner-checking
                    :writer set-corner-checking
                    :initform nil
                    :documentation
                    "Is the agent currently verifying its in a corner?")
   
   (finish-up :type boolean
              :reader get-finish-up
              :writer set-finish-up
              :initform nil
              :documentation "Is the agent moving into a corner and stopping?")

   (ab-count
    :reader get-ab-count
    :writer set-ab-count
    :initform 0
    :type integer
    :documentation 
"How many spots the agent has slid across while checking if the current 
location is really a corner.")
   
   (max-adjacent-blocks :type integer
                        :writer set-max-adjacent-blocks
                        :reader get-max-adjacent-blocks
                        :initarg :max-adjacent-blocks
                        :initform 3
                        :documentation 
"The maximum number of blocks that can form wall in the grid. 
This assumes >= 2."))
  
  (:documentation
"A History-Agnet uses additional information that it has aquired about its 
world to make decisions about what to do next."))

;;; see generic turn-left
(defmethod turn-left :after ((a History-Agent))
  (set-last-last-bumped nil a)
  nil)

;;; see generic turn-right
(defmethod turn-right :after ((a History-Agent))
  (set-last-last-bumped nil a)
  nil)

;;; see generic move-forward
(defmethod move-forward :before ((a History-Agent) (g Grid))
  (set-last-last-bumped (get-last-bumped a) a)
  nil)

;;; see generic move-left
(defmethod move-left :before ((a History-Agent) (g Grid))
  (set-last-last-bumped (get-last-bumped a) a)
  nil)

;;; see generic move-right
(defmethod move-right :before ((a History-Agent) (g Grid))
  (set-last-last-bumped (get-last-bumped a) a)
  nil)

;;; see generic rule-matching
(defmethod rule-matching :around ((a History-Agent) percept-list)
  (let ((lst-bumped (cadr percept-list))
        (lst-lst-bumped (get-last-last-bumped a)))
    (cond
      
      ;; If we're corning checking get a corner checking move
      ((or (get-corner-checking a) (get-finish-up a))
       (wall-checking a lst-bumped lst-lst-bumped))
      
      ;; Do we think we're in a corner?
      ((and (equal lst-bumped bumped-right) 
            (equal bumped-forward lst-lst-bumped))
       (set-corner-checking t a)
       (set-last-bumped bumped-forward a)
       (set-last-last-bumped nil a) ; unecessary but doesn't hurt
       turn-right) ; Turn and make sure we're in a corner
          
      ;; Otherwise just do what a non-history agent would do
      (t 
       (call-next-method a percept-list))
      )))

(defgeneric wall-checking (a lst-bumped lst-lst-bumped)
  (:documentation
"Actions to be made when the agent is determining if the current object is a 
wall or an obstacle.

@parameter a History-Agent, the agent to make the move
@parameter lst-bumped, direction last bumped
@parameter lst-lst-bumped, direction bumped two moves ago
@return an action to be made"))
(defmethod wall-checking ((a History-Agent) lst-bumped lst-lst-bumped)
  
  (let ((cn (get-ab-count a))
        (max-n (get-max-adjacent-blocks a) ))
    
    (cond 
      ((get-corner-checking a)
       
      (if (< cn max-n)
           (cond 
             
             ((equal bumped-forward lst-bumped)
              (set-ab-count (1+ cn) a)
              go-right)
             
             ((and (equal nil lst-bumped) 
                   (equal bumped-forward lst-lst-bumped))
              go-forward)
             
             ;; We ran into another corner this can't be a corner
             ;; Turn-right and reset
             ((equal bumped-right lst-bumped)
              (set-corner-checking nil a)
              (set-ab-count 0 a)
              turn-right)
             
             ((= 0 cn) ; Just started to check
              (set-ab-count (1+ cn) a)
              go-forward)
             
             ;; If we get here it means the agent went forward so
             ;; lst-bumped = nil and lst-lst-bumped = nil
             ;; So we must reset
             (t
              (set-corner-checking nil a)
              (set-ab-count 0 a)
              go-forward)
             
             )
           (progn ; else this must be a wall go back to the corner
             (set-corner-checking nil a)
             (set-finish-up t a)
             turn-left)
           )
       )
    
      ;; If we know its a wall just go back to the corner
      ((get-finish-up a)
       (cond 
         ((> cn 0)
          (set-ab-count (1- cn) a)
          go-forward)
         (t
          do-nothing)))
      )
    )
  )

;;;; End History-Agent Definition


;;;;<Simulator Definition> Start

(defun run-simulator ()
"Run the simulator producing three files: test1.out, test2.out, test3.out, 
and test4.out. The first file, is a grid world without obstacles. The second 
file, is a grid world with obstacles, but no two obstacles are adjacent. And 
the third file, is a grid world where objects may be adjacent, but no more than 
three objects may be adjacent.  Users looking to do custom runs should invoke 
the run method, which runs a single test at a time."
  
  (run :rock-list '() :start-spot (make-point :x 2 :y 2) :file-name "test1.out")
        
  (run :start-spot (make-point :x 1 :y 2) :file-name "test2.out")
  
  (let ((agent (make-instance 'History-Agent)))
    (set-direction agent-left agent)
    (run :agent agent :file-name "test3.out" :start-spot (make-point :x 2 :y 2)
         :rock-list '((1 3) (1 2) (2 3))))
  
  
  (let ((agent (make-instance 'History-Agent)))
    (set-direction agent-left agent)
    (set-max-adjacent-blocks 9 agent)
    (run :agent agent :file-name "test4.out" :grid-x 10 :grid-y 10
         :start-spot (make-point :x 2 :y 2)
         :rock-list '((1 1)(1 2)(1 3)(1 4)(1 5)(1 6)(1 7)(1 8)
                      (2 8)(3 8)(4 8)(5 8)(6 8)(7 8)(8 8)(8 7)(8 6)
                      (3 6)(4 6)(5 6)(6 6)(7 6)(8 5)(8 4)
                      (3 5)(3 4))))
  
  (format t "~&~%The runs were written to test1.out, test2.out, ") 
  (format t "test3.out, and test4.out.~%~%")
  
  )


(defun run (&key (agent (make-instance 'Agent)) 
                 (start-spot (make-point)) 
                 (grid-x 5) (grid-y 5) 
                 (rock-list '((1 1) (1 3)(2 3) (4 2)) )
                 (file-name nil) (max-moves 100))
"Do a single run of the simulation.  If no keys are specified, the simulation is
run with a simple reflex agent initially located at (0,0) in a 5x5 Grid.
Obstacles are placed at {(1,1),(1,3),(2 ,3),(4,2)} by default. If a file-name is 
specified the output will be written to a file with that name as well as to 
*standard-out*.

@key agent Agent, the agent to run the simulation with
@key start-spot point, the initial location of the agent
@key grid-x integer, the width of the grid
@key grid-y integer, the height of the grid
@key rock-list list, the location of the obstacles on the grid
@key file-name string, a file to output the run to
@key max-moves integer, the maximum number of moves the agent may make
@return list, a list of points that the agent visited during the run"
  
  (setup-output file-name)
  
  (let ((g (make-instance 'Grid :x grid-x :y grid-y))
        (same-spot-count 0) (last-spot (make-point :x -1 :y -1))
        (result-list nil) (num-moves 0))
    
    (put-object g agent start-spot)
    (place-rocks g rock-list)
    
    (display g)
    (do () ((or (>= same-spot-count 8) (>= num-moves max-moves)))
           
      (setf result-list (append result-list (list (get-location g agent))))
      (display "Next Move: ")
      (display (make-move agent g))
      (display g)
      (if (equal-points last-spot (get-location g agent))
          (setq same-spot-count (1+ same-spot-count))
          (setq same-spot-count 0))
      
      (setq last-spot (get-location g agent))
      (setf num-moves (1+ num-moves))
      )
    
    (when (>= num-moves max-moves)
      (freshline)
      (display "Agent never finished."))
    (freshline)
    (display "Total of ")
    (display (length result-list))
    (display " moves")
    
    (close-output)
      
    result-list))


(defparameter *run-output* nil "File output stream")

(defun setup-output (file-name)
"Ask the user if it wants to output to a file.  *run-output* will output to a
file if the answer is yes.

@parameter file-name string, the file to write output to
@return nil"
  
  (cond 
    
    ((not (null file-name))
     (princ file-name)
     (setf *run-output* (open file-name :direction :output :if-exists 
                              :supersede :if-does-not-exist :create)))
    
    (t
     (format t "~%Do you want to write to a file? (yes/no)")
     (finish-output *standard-output*)
     (when (string-equal (read) "yes")
       (format t "~%Enter the file name: ")
       (finish-output *standard-output*)
       (let ((file-name (read-line)))
         (setf *run-output* (open file-name :direction :output :if-exists 
                                  :supersede :if-does-not-exist :create))))))
    
  nil)

(defun close-output ()
"Close the *run-output* if it's open.

@return nil"
  (if (and (not (null *run-output*)) (open-stream-p *run-output*))
      (close *run-output*))
  
  nil)

(defun display (str)
"Display the given string on the *run-output* stream.

@parameter str string,  the string to display
@return nil"
  
  (format t "~A" str)
  (if (and (not (null *run-output*)) (open-stream-p *run-output*))
      (format *run-output* "~A" str))
  
  nil)

(defun freshline ()
"Display a fresh-line on the *run-output* stream.

@return nil"
  (fresh-line)
  (if (and (not (null *run-output*)) (open-stream-p *run-output*))
      (fresh-line *run-output*))
  nil)


(defun equal-points (p1 p2)
"Return true iff p1 is equal to p2

@parameter p1 point
@parameter p2 point
@return true iff p1 == p2"
  (and (= (point-x p1) (point-x p2)) (= (point-y p1) (point-y p2))))

(defun place-rocks (g lst)
"Place a bunch of rocks on the given grid at specified locations.

@parameter g, A grid
@parameter lst, A list ((x1,y1),...,(xn,yn)) of points to place rocks
@return nil"
  
  (loop for i from 0 to (- (length lst) 1)
        do
        (let ((x (car (elt lst i))) (y (cadr (elt lst i))))
          (put-object g (make-instance 'Rock) (make-point :x x :y y)))
        )
  
  nil)

;;;; Simulator Definition End
