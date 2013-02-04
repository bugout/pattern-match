Pattern Match

COMS 4701 Artificial Intelligence Project 1

=============

Pattern matching can be modeled as a search problem on the syntax tree
of pattern and data. We use DFS search to search for matching
patterns.

Here, I implemented two versions of pattern matching. The first
version 'match-post.lsp" uses post-order traversal in DFS. The other
version, 'match-pre.lsp" searches in pre-order.

Post-order traversal first generates the bindings for subtrees before
examining the consistency with their parents. In the pre-order traversal
version, bindings are passed from parents to their children. If there
is conflicts when matching children, nil value is returned. If no
conflict is found, we expand the binding and continue to search down.

---------------------------
Input format: 
(pattern-match pattern data)
---------------------------

Examples:

(pattern-match '(?x 2 (?x)) '(1 2 (1))) ((?x 1))
 
(pattern-match '(?x 2 (?x)) '(3 2 (1))) NIL
 
(pattern-match '(1 (* ?x *)) '(1 (a (b c) d))) (((?x a)) ((?x (b c)))
((?x d)))
 
---------------------------
More about testing
---------------------------
'test.lsp' is a simple test script to check the
result.  (runtest) validates the correctness of our implementations.
(time (runtesttime)) tests the performance of our implementations.

Performance of post-order version:
Real time: 0.779717 sec.
Run time: 0.776048 sec. 
Space: 14240000 Bytes 
GC: 16, GC time: 0.020001 sec.

Performance of pre-order version:
Real time: 0.80227 sec.
Run time: 0.80005 sec.
Space: 20480000 Bytes
GC: 24, GC time: 0.024 sec.

Post-order version is slightly faster than the pre-order version and
its space usage is only half the size of the pre-order version. But
its code size is slightly more than the pre-order version.

