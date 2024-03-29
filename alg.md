## Introduction

This page describes the algorithms used in Resolve for ast
differencing and merging.

Resolve is implemented on the Software Evolution Library, a Common
Lisp library for representing syntax trees of multiple languages.  The
representation is intended to be fully unparsable to a file that is
byte equivalent to an initial input file (unlike an abstract syntax
tree that may discard irrelevant syntactic details or apply semantics
perserving transformations.)   However, the trees are still called ASTs
for some reason.

ASTs, at their most basic, have a list of children, which may be other
ASTs or leaf values.  The leaf values are typically strings.  AST nodes
may also have a "kind" value, which is the syntactic category of the node.
The default unparsing algorithm for an AST is to concatenate a string
for each of its children, in order.  For children that are themselves
strings, this is an identity function.

It should be clear that there can be more than one AST corresponding
to a parse of a given program, each of which unparses to the same program.

## Differencing of ASTs

The differencing algorithm computes the least costly edit between one AST
and another, with the following operations:

- Insertion of a child (which may be a leaf string or another AST)
- Removal of a child
- Wrapping a node (that is, inserting other AST nodes before, above, and after the node, causing the node to be moved downward in the tree.
- Unwrapping a node (the inverse of wrapping; move a descendant of some node up to take the place of the node)
- Wrapping and unwrapping sequences of children (the same as wrapping and unwrapping, but applied to more than one child)
- String-to-string edits on leaf values, using standard string edit operations (insert, delete of characters).

String edits are optional; an alternative approach is to break leaf
strings into tokens (which does not change the unparsing of the AST)
and then cause each token to be treated as an unchangeble value that
can be inserted, removed, or moved up and down the tree, but not
internally edited.

Edits do NOT currently support motion of ASTs that change their order
in the AST, although this would be nice to have.

There is a cost function for edit operations that is proportional to
the size of the things inserted or removed, plus a cost per operation
itself.  The cost of a set of edits is the sum of the costs of its operations.
The diff algorithm tries to find an edit that minimizes this total cost.

There is a polynomial time algorithm for computing these diffs, but in
the worst case it can be expensive.  The core of the diff algorithms
is a dynamic programming algorithm on child sequences, with recursive
differencing of children.  Diffs are memoized so the edit difference
between two subtrees need only be computed once.  Wrap and unwrap
distances are also memoized.  Also, the full algorithm for sequence
diffing isn't used; instead, a prepass finds identical subsequences
and reduces the problem of edits of the residual differences.  This
can miss the best edit sequence in some cases, but is typically much
faster, and in practice seems to work well, as the pathological cases
don't come up in real programs (although it would be useful to check
carefully if this is always true).  On C programs, the time to compute
a diff is dominated by the Clang parser.

The diff objects created by the algorithm contain not just the values
being inserted and removed, but also all the parts of the trees that
remain the same.  This enables a printed representation of the diff
to be created by walking the diff.

Diffs can be decomposed into edit trees -- a tree of nested edits
where each subedit is isolated relative to it parent.  In practice,
edits tend to decompose naturally in this way.

## Merging Diffs

The merge algorithm operates on three ASTs: a root AST ("original"),
and two branch versions ("left" and "right").   It computes diffs
from original to each of left and right, then walks these diffs to
identify and attempt to resolve conflicts.

There are two modes of conflict resolution.  In the first, an attempt
is made to obtain a merged AST using heuristics to handle cases where
different edit operations are encountered.   For example, if one diff
has a "same" operation (that is, don't change this part of the AST),
but the other performs a change, the second is used.  For differences
that do not intefere this should do the right thing.

The other modes introduces special conflict nodes into the AST that
record both changes.  These conflict nodes can then be used by search
based techniques to try to resolve conflicts using additional information
(does the program compile, does it pass tests).

## Issues

The default representation of ASTs causes merges to sometimes not be syntactically correct.  For example, consider two comma-separated lists of expressions

Original:  `x , z`

Left: `x , a , z`

Right: `x , b , z`

The differences created here involve two insertions:  insertion of a subexpression (` a ` or ` b` ), and the insertion of a string "," for the separator.  When these diffs are merged the second insertion is common, so the only conflict is the first part.  A straightforward conflict resolution inserts both, giving

Merged:  `x , a  b , z`

which is not syntactically correct.

The problem here is the default representation doesn't know that the
comma has to be replicated.  The solution will be to enrich the AST
representation so the separators are not explicitly represented in
leaf strings, but instead are introduced during unparsing.

## Motivation and History

Resolve was developed to provide a diff/merge algorithm integrated into
the Software Evolution Library.  While based on ideas from preexisting
tooling, such as Gumtree, Resolve is not intended to supersede them.

Resolve's development has been motivated by practical considerations.
The algorithms should be fast, and should produce results that programmers
find to be natural and useful.   This is contrast to some academic
work where the goal is to find algorithms that are guaranteed to find
optimal (in some specific sense) edits with running times that are
often some higher degree polynomial in the size of the ASTs.

We took a practical approach to developing the diff algorithm, using
it on "real" code and seeing where it performed well and where it did
not.  An example of this: for diffing long sequences of (for example)
statement ASTs, we had at first implemented the usual O(mn) dynamic
programming algorithm.  But we found that in practice this was
overkill: in essentially all cases, the heuristic algorithm that first
identifies long common subsequences would produce the same edit, but
up to orders of magnitude faster.  This was known before (the old line
oriented Unix diff algorithm did this, for example).

Experience on real code showed that we needed to be able to move code
up and down the AST (wrap and unwrap), as this is a common idiom in code
differences (for example, wrapping an if statement around a code block).
We took the same practical approach to this, using heuristics to limit
search at the cost of sometimes not producing the very best edits.

## Areas for Future Research

### Heuristics for Fast Tree Comparison

It's useful to be able to quickly determine if two subtrees are good candidates
for recursive application of the differencing algorithm (that is, the cost of
the edit from one to the other may be small).   Find good algorithms for doing
this.  An example might be compute the bag distance of leaf values between the
two trees.

### Sliding L1 Window Problem

This problem comes up in quickly evaluating whether sequence wrapping/unwrapping
is worth investigating.  Given a vector x and a longer vector y of nonnegative integers,
compute the L1 norm of the the difference between x and contiguous subsequences of
y of the same length as x.   For the L2 norm, this could be done by convolution using
FFTs.  Is there an algorithm better than the trivial quadratic one for the L1
case?

### Graceful Degradation

Modify the algorithm to quickly find a "good" edit, then (as time allows) gradually improve
is.  This would mean maintaining some sort of graph representing the overall edit problem,
with promising parts being progressively explored (or pruned off).

### General Move Edits

Allow order-violating motions of subtrees.
