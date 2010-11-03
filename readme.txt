
                    The Quick C-- Compiler

     ``A new perspective on programming-language infrastructure''
                                                         
This directory contains the source code for Quick C--.  If you write a
compiler that emits C--, we'll turn your C-- into efficient machine
code.  More information can be found at http://www.cminusminus.org.

To build the compiler from source code, see the install.txt document.  

Simple usage for qc
--------------------------

To test the parser just do:

  $ ./qc -dump_cmm demos/hello.c-- 

which should output the internal representation of hello.c-- when
everything went fine.




Problems and Bug Reports
--------------------------------

Once the compiler is built, to make effective use of it, you should
read the release notes in doc/release.ps.  The notes describe what
parts of the C-- specification are and are not currently supported.
You can also try `man qc--'.  Quick C-- is still under development, so
if there is a feature you need, please ask for it.

If you have trouble building the compiler or using it, send email to
bugs@cminusminus.org.  You can view the status of bug reports at
http://www.cminusminus.org/qc--bugs.  If either this email or this URL 
fails, please send a letter of complaint to nr@eecs.harvard.edu with a 
copy to help@eecs.harvard.edu.
