;;;; Author Mark Royer

(in-package :common-lisp-user)

(defpackage #:simple-agents
  (:nicknames :simple-agents)
  (:use :cl)
  (:export
    run-simulator
    run
    Grid
    grid-x
    grid-y
    put-object
    get-object
    object-at-p
    get-location
    in-bounds
    Rock
    Agent
    get-direction
    get-last-bumped
    get-location-infront
    turn-left
    turn-right
    forward-p
    left-p
    right-p
    move-forward
    move-left
    move-right
    move-backward
    History-Agent
    Point
    point-x
    point-y)
  (:documentation 
"This package defines a world and two simple types of agents that can 
move around in it.  The first is a simple reflex agent, which is defined 
by the Agent class.  The second (History-Agent) extends the simple reflex 
agent with knowledge of where it has previously been."))
