CIS 552 Final Project, Fall 2020

Project name: 
Github repo: https://github.com/cis552/project_6-project/
Group members: Yoni and Adrian
Mentor: Stephanie

*NOTE*: after CIS 552 is over, we will be removing all repositories from github. If you want to make your project public, you should move it to your own github account.

Comments:
Great job! You've taken on a rather complex purely functional data structure and have analyzed/extended it in several ways. It's clear that you've learned a great deal from this project. 


## Correctness    25/25
+ Tackled super tricky data structure
- Didn't do Applicative/Monad instances, but we talked about that beforehand

## Design          32/35 
* Modularity
  + Good job manipulating the explicit import and export lists in the definitions of PriorityQueue and Sequence types.
* Types
  + Awesome job here: you are working with some crazy-expressive type definitions. 
* FP
  + Great in-depth exploration of a purely functional data structure. 
* Functors, Monads, etc.
  + Glad you figured out how to use the "Measured" type class to generalize the data structure from sequences to priority queues.
  - Not sure why the comment before fmap' says "Pseudo-applicative"? The applicative operations are pure and (<*>), not fmap. 
* Abstraction
- It's disappointing that you weren't able to completely get rid of FingerTree in favor of the more general FingerTree2. In particular, you should have been able to implement both sequences and priority queues with the same definitions. 
## Testing         15/15
+ Tons of test cases, especially for split
+ Many QuickCheck properties, great!
- Shrink should not be recursive. Let QC find the smaller trees. (And you may be skipping over a potential counterexample with your current definition.)
## Style           5/5
- Your Disneyland application could use a few more comments (and some clean up).
## Other
- Good job adding the benchmarking!
- However, it would have been better to split up the benchmarks into smaller groups so that the slower ones don't dominate the results. As it is, it is difficult to see the difference between the faster ones. 
- For comparisons with asympototic running time, would be also good to vary the size of the data structures that you use for the benchmark. That way you should be able to see the O(n) or O(lg n) behavior. 


## Proposal  5/5
## Checkpoint #1 3/5
## Checkpoint #2 4/5
## Presentation 5/5

## Project score: 94
