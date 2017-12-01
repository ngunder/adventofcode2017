aoc2017
=====
My solutions to the 2017 advent of code using Erlang.

To run, you need Erlang and Rebar3.

Build
-----
    $ rebar3 compile

How to run
-----
    $ rebar3 shell
    Eshell V8.3 (abort with ^G)
    1> aoc2017:go(Day).
Where Day is day number for the problem you want to see the result from.

You can also run
    
    1> aoc2017:go(all).
    
To see all of the results at once.
