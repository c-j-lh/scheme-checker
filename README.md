Not all my commits are my work. Refer to author labels in code.


Prof's comments to address:

# midterm-project

Implement strict-andmap without an accumulator. I think we'll need CPS. Nontrivial (without GPT ofc).


# week11-12 code
(define rand-int-list

This implementation is typical of a programmer whose priority is efficiency:
it is tail-recursive with an accumulator (and yet uses reverse instead of reverse!),
it was not conceived inductively, and
it is untested.




The idea of randomness is good but not substantiated.
A telling random example could have been
  (seq (disj (atom 0)
             (disj (atom 1)
                   (atom 2)))
       (seq (disj (atom 0)
                  (disj (atom 1)
                        (atom 2)))
            (disj (atom 0)
                  (disj (atom 1)
                        (atom 2)))))
i.e., a list of 3 integers, each between 0 and 2.
Then
  (list (random 3) (random 3) (random 3))
would have been a fitting random input for positive tests.

Here, it would have been more fitting to analyze whether your tests (random or not)
provide code coverage.

3.2

what is described here is not what is implemented in the .scm file

> (eq? ns nsp) prevents certain expressions from causing non-termination.

Go to the end of your thoughts: which expressions and why do they cause non-termination?
And why are there no such tests in the compiled code presented in the last section?


# week11-12-report
6.4

the loop/k structure doesn't correspond to what happens in the first exercise

How would you compile, e.g.,
  (seq (atom 1) (seq (star (disj (mt) (atom 2))) (atom 3)))
?
