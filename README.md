jlite-compiler
==============

A group project for course CS4212: Compiler Design at National University of Singapore. Build a compiler for JLITE language which is strongly-typed and is a subset of Java

Compilation
=============
In terminal, run:

    make

A directory `bin` is created containing all intermediate files. An executable file `jlite_main` is in `bin` directory. A copy of `jlite_main` is also put in your current directory.

To clean the project (removing files generated in compilation, including `jlite_main`), in terminal, run:

    make clean

Instruction on running
=======================
Run without optimization:

    ./bin/jlite_main <path to source code>

Run with optimization:
    ./bin/jlite_main -opt <path to source code>

Optimization part (Cui Wei’s note)
===================================
Call function: optimize_ir3_program located at line 366 of file ir3_optimization.

Parameter: (cls_list, main, md_list), tuple with three elements, class-definition list, main method, and other method list all from what was generated in the ir3 form.

return value: (cls_list, main, md_list), tuple in the same form, but with methods optimized.

