# HINTDSL Tutorial

*HINTDSL is standing in for a better name.*

Student assigments are often submitted electronically, but while autograders may return scores, they rarely provide feedback to students beyond canned responses to common problems. Human graders can provide more detailed feedback, but the process is time-consuming and infeasible for large classes. HINTDSL provides instructors with a way of giving students insight into their mistakes automatically. A hint defined by HINTDSL requires as input only the true solution and the student's solution; it does not require any knowledge base or even an encoding of the problem itself.

A HINTDSL hint is an expression of some difference between the student's incorrect solution and the correct solution given by the instructor (of course, if the student's solution is correct, there is no need for a hint). The HINTDSL is flexible and lets the instructor choose what kind of information to provide to the student. We assume that instructors already have some means of testing if a student solution is correct and are only searching for hints if the solution is known to be incorrect.

The HINTDSL itself is a subset of Racket. An instructor uses HINTDSL to write a function that takes the student solution and true solution and returns the attributes necessary to express the hint to the student; an additional view layer can be used to present the hint in the desired formatting. Hints can synthesized using a variety of engines; examples below use enumerative search and Rosette.

For this tutorial, we will create hints for homework problems in which the student writes deterministic finite automata. HINTDSL is sufficiently general to be used for a variety of domains, however.

## A simple counterexample hint
> Given the alphabet {0, 1}, write a DFA that accepts all strings that contain exactly 2 consecutive '0's.

As a first example, let's write a hint that finds a string that is incorrectly accepted or incorrectly rejected on the student's solution.

First of all, we need some apparatus to handle the DFA domain. We write a Racket macro that will provide a function that, given a word in the DFA's alphabet, will return true if the word is accepted and false is not. The precise definition of this macro depends on the format the student's solutions will be submitted in (for example, if the student is working in a web interface, the submitted automaton might be defined in JSON or XML). The details of the macro are not discussed here, but see `automata.rkt` for a sample implementation.

Now, let's define our hint. Hints are always an expression of the difference between the student solution and the true solution. We will write a hint expressing the property "a word on which the student solution DFA behaves differently than the true solution DFA". This property can be written as the following boolean function:

```
(define (diff-outcome? M1 M2 word)
  (not (eq? (M1 word) (M2 word))))
```
Since `M1` and `M2` are the result of the Racket macro that transforms an automaton representation into a function, if those two functions have different outcomes on the same input word, their corresponding automata behave differently on that word.

Now that we have a property defined, we need to define a search strategy to find a word where that function returns true. Since the problem is fairly simple, let's use the simplest possible search strategy and enumerate all words in the {0, 1} alphabet until we find a word to serve as a counterexample. We'll make use of two convenience methods: `wordgenerator` a generator that will return the next word in the language of all strings made from symbols in our alphabet, and `words-up-to-k`, which calculates the number of words in that language that have a length less than k.

Using those methods, we write a method that will recursively consume words from the generator until it has found a word for which our chosen property is true, or until we have checked all the words of length less than k.

```
(define (find-counterexample M1 M2 alphabet k)
  (let ([limit (words-up-to-k (length alphabet) k)]
        [gen (wordgenerator alphabet)])
    (letrec ([f (lambda (i)
                  (let ([w (gen)])
                    (cond [(eq? i limit) "no counterexample"]
                          [(diff-outcome? M1 M2 w) w]
                          [else (f (add1 i))])))])
      (f 1))))
```


## A more complicated hint

> Given the alphabet {0, 1}, write a DFA that accepts all words with the substring '01'.

Our first hint had to do with the semantic difference between the true and student solution: some word was either present in the language described by the true solution but not the student solution, or vice versa. We can also write hints that describe *syntactic* differences between the two true solutions.

States in a DFA represent equivalence classes; any string that arrives in a particular state, no matter what path it followed to get there, should have the same outcome. If two words arrive in the same state on the student DFA, but have different outcomes on the true DFA (i.e. one is accepted and one is rejected), then those words belong to different equivalence classes and therefore shouldn't be able to arrive in the same state. If we can identify two such words, then we know that the state they arrive in is defining an incorrect equivalence class, and we can return the name of that state to the student as a hint.

To find such a state, let's use a different search strategy; we'll use Rosette to synthesize an answer to the property we've formulated. Rosette is a solver-aided language that includes all of Racket; see here for instructions for installing and using Rosette.

First, we need to make a change to the macros we use to create our DFA representations. We'll use the same macro to create the true DFA, but we'll tweak the one used to build the student DFA so that after consuming a word, it returns the name of the state it arrives in.

## A custom hint
