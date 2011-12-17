#lang racket

#|
Q:

Consider the encoding procedure that you designed in exercise 2.68. What is the order of growth
in the number of steps needed to encode a symbol? Be sure to include the number of steps needed
to search the symbol list at each node encountered. To answer this question in general is difficult.

Consider the special case where the relative frequencies of the n symbols are as described in
exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed
to encode the most frequent and least frequent symbols in the alphabet.
|#

#|
A.

encode-symbol, has to go down to the last level, n is the depth of the tree, where n
is the number of symbols. On every node, it cons'es a number. Also there is the member? function
which spends max of n steps to find out if a given symbol is a member of the set. This is done for
every symbol in the message. So the worst case growth is of the order O(n * n).
|#
