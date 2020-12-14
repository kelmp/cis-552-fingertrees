# proj
## FingerTree
### `split`
#### The `Split` type
This type is eventually merged into two elements by the "split" function by
putting the middle element at the start of the right element.
This three-element structure is required to construct certain intermediate
structures in "split".

#### Overview of splitting
All of the split methods work off of the same goal. Given a predicate,
find a spot where the predicate flips from false (on the left) to true
(on the right).

For clarity's sake, this spot will be called "the flip," and elements
before or after are called "pre-flip" or "post-flip" respectively.

#### `split` helpers
- Anticipated Q: Why do splitTwoAndPair and splitThreeOrTriple allow all three elements to end up on the right, but not on the left?
    - A: If this function was called, the flip is somewhere among these three
    elements. All three on the right means the flip happens at x, whereas
    all three on the left would mean that the flip happens sometime after z,
    which contradicts the reason the function was called.
- Anticipated Q2: Why does it not matter whether you're splitting a Some or a
tuple? Why the same result data type?
    - A: The result of this method is used for outer branches of a FingerTree, so
    a Some type is needed. (TODO might need more explanation)