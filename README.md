Names: Yoni Gutenmacher, Adrian Wang

Pennkeys: yonigut, wangadr

## Note about `stack.yaml`

I did see on the website that stack.yaml is supposed to use resolver `lts-16.11`. However, this causes "Access violation in generated code when writing 0x0," which I wasn't able to resolve without reverting to `lts-16.12`.

After poking around on the Internet for a bit this might have something to do with Windows and ghc 8.8.3 not getting along too well. Hopefully I'll have altered this and checked functionality by the time submission happens - if not, I have another project due on Tuesday so that's probably why I forgot.

# File overview

## `src`

This folder contains the main library, and should be looked at first.

- `FingerTree2.hs` contains our initial FingerTree implementation, where the cached parameter used for `Measured` is locked as an `Int` (length). This is useful in a pure data structure context, and is currently used to show the random-access capabilities of finger trees in the demo.

- `FingerTree.hs` contains a more general FingerTree implementation, where the type used for `Measured` is any monoid. This is necessary for priority queues, where efficiently extracting the highest-priority element requires priority-based caching.

- `PriorityQueue.hs` contains an implementation of priority queues using FingerTrees. Extraction of the max-priority element uses a `split` operation.

- `Sequence.hs` is somewhat analogous to `Seq` in Haskell's standard library, using FingerTrees to store arbitrary elements in order.

## `test`

This folder contains files used for testing correctness and performance.

- `Tests.hs` contains HUnit and QuickCheck tests for our finger trees, priority queues, and sequences.

- `Benchmark.hs` uses the `criterion` library to benchmark our finger trees' performance at various tasks, compared to common Haskell data structures.

## `app`

This folder contains our demo, which demonstrates one possible use case for finger trees.

Finger trees have solid performance in common data structure operations - O(1) access and insertion to head and tail, and indexing based on `split` (which means logarithmic runtime proportional to the size of the smaller half of the split). These are used to create a scenario involving people waiting in line for Not-Disney Not-Land-Or-World.

- New customers are added to the front or back of the line via O(1) FingerTree operations.
- A random customer will get angry each round to demonstrate indexing capabilities.
- Letting people into the Cosmos-Themed Landform Ride involves repeatedly calling `tail` which is amortized O(1).
