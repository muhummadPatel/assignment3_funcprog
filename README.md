##Assignment 3 - _Functional Programming: Fixing the World (rubiks cube solver)_

**Author** : Muhummad Yunus Patel  
**Student#** : PTLMUH006  
**Date** : 27-April-2015  

**NOTE**: Should you require any clarification, please feel free to contact me 
           at muhummad.patel@gmail.com. Thank you. :)

###Description:
This assignment involved using Scheme to write a functional program to solve a
2x2x2 spherical rubiks cube.

* The _source code_ for the solution is provided in the assignment3.scm file. 
* The _answers to the theoretical questions_ are provided in the 
  theoryAnswers.pdf file.
* _Unit testing for each procedure_ is provided in the comments below the 
  defintion of that procedure.


###Environment:
* OS: Mac OSX Yosemite (v10.10.2)
* Scheme interpreter and version: Gambit (v4.7.4)


###Running the solution:
The solution may be run in a command-line/terminal window as follows:

* Navigate to the directory containing this file and the assignment3.scm file
* Type 'gsi' and enter to run the Gambit Scheme interpreter
* Type '(load "Assignment3.scm")' to load the code in Assignment3.scm
* Run the solveCube procedure with a state list for a shufled cube and n=0
* You should then receive a list of moves to do in order to solve the cube