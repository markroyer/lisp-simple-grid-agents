# Mark Royer
#
# A simple Makefile for compiling lisp programs.  To compile a user can 
# simply type
# 
# make
#
# at the command line.  To remove all of the files that were built and leave
# only the source files, a user can type
#
# make clean
#
# at the command line. 


# The top level directory
rootdir = .

# lisp version
lisp = sbcl

# documentation location
docsLocation = docs

# executable name
prog = agents

# entrance function to the program
main = simple-agents:run-simulator

# packages to use
packages = simple-agents


# The source ASDF (Another System Definition Facility) file
asdfile = $(prog).asd


# Set the flags depending on allegro or sbcl lisp
ifeq ($(lisp), sbcl)
load = "--load"
eval = "--eval"
exit = "(sb-ext:quit)"
else
load = "-L"
eval = "-e"
exit = "(exit)"
endif

plist = $(shell export plist="";for i in $(packages); do export plist=" $(eval) \\\"(use-package '"$$i")\\\" ""$$plist"; done; echo $$plist)

default: all 


all:
        # Compile source files
	@$(lisp) $(eval) "(require 'asdf)" $(load) $(asdfile) $(eval) "(asdf:operate 'asdf:load-op '$(prog))" $(eval) $(exit)
        # Make an executable file to intialize stuff
	@echo "$(lisp) $(eval) \"(require 'asdf)\" $(load) $(asdfile) $(eval) \"(asdf:operate 'asdf:load-op '$(prog))\" $(plist) $(eval) \"($(main))\" $(eval) \""$(exit)"\"" > $(prog)
	@chmod ugo+x $(prog)
	@echo "Executable file $(prog) created"

# This only works with albert
# http://albert.sourceforge.net/
# May need to consider something else?
document:
	@$(lisp) $(eval) "(require 'asdf)" $(load) $(asdfile) $(eval) "(require :albert)" $(eval) "(albert:document-systems :$(packages))" $(eval) $(exit)
	@-cd $(docsLocation) && jade -d /usr/share/albert/albert.dsl -t sgml /usr/share/albert/xml.dcl book.xml > /dev/null
	@cp /usr/share/albert/albert.css $(docsLocation)
	@cp -r /usr/share/albert/icons $(docsLocation)/


# Remove all the binary files in the path
clean:
	@-find $(rootdir) \( -name "*~" -o -name "*.fasl" -o -name $(prog) \) -exec rm '{}' \;
	@-rm -rf $(docsLocation) test1.out test2.out test3.out test4.out

