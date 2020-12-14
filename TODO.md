# FingerTree.hs
- Clear out TODOs and comments
- Rename to OldFingerTree.hs, make sure nothing breaks
- Reorganize methods to follow same order as FingerTree2
- Add comments/headers matching FingerTree2

# FingerTree2.hs
- Add following methods from FingerTree:
    - `toList` and `fromList`
    - `arbitrarySizedTree` and `arbitrary`
    - `shrink` and shrink helpers
- Rename to FingerTree.hs, make sure nothing breaks

# PriorityQueue.hs
- Implement all stubs

# Sequence.hs
- Implement all stubs

# Benchmark.hs
- Implement benchmark suite from article in Stephanie's email
- Benchmark FingerTree operations compared to List, Seq, (maybe other stuff too?)

# Tests.hs
- Fill in unit tests
- Add QuickCheck properties that aren't from HW5
    - Possibly make these test props of PQ and Sequence in addition to FingerTree

# Application (Main.hs?)
- Come up with heavily random-access use case that would benefit from FingerTrees
- Implement said use case