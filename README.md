# lisp-simple-grid-agents

A simple lisp program that explores AI agents with reflexive and
history characteristics.

The program is a simulator that runs a reflex agent and a history
agent on a some grids.  The program can be run by loading the file
agents.asd and invoking either of the following two functions:

1. simple-agents:run
2. simple-agents:run-simulator

The function `simple-agents:run` is a customizable way to run a single
experiment.  For ease of use the function `simple-agents:run-simulator`
was created to run four experiments with a simple reflex agent
(Agent) and a history agent (History-Agent).  Each of these runs is
output to test1.out, test2.out, test3.out, and test4.out.

## Dependencies

This program was built using the Lisp flavor SBCL.  Documentation is
in the Albert Doc style. If you wish to create documentation, then
Albert is required, too.

## Building

A user can create an executable script called

`agents`

by typing

`make`

at the command line.

This system made use of the Albert documentation tool.  If a user has
the Albert package installed on his or her system, typing

`make document`

at the command line will create HTML documentation for the
project.

Typing

```
make clean
```

will remove all generated files.

## Additional Comments

This project was developed using SBCL.  The makefile also
supports allegro common lisp. A flag for the lisp type (lisp) can be
changed at the top of the Makefile.

## License

The project is licensed under the terms of the
[GPL3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.

<!--  LocalWords:  asd SBCL
 -->
