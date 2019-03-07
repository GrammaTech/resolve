A simple experiment to collect information on frequency of different
types of conflicts from a small number of C/C++ programs which we can
easily uniformly build, our "benchmarks."

    conflicts-w-line-diff = ∅
    conflicts-w-line-diff-after-clang-format = ∅
    conflicts-w-AST-diff = ∅

    ∀ b ∈ benchmarks
      ∀ merge-commit ∈ 'git log --merges'
        l := left-parent(merge-commit)
        r := right-parent(merge-commit)
        conflicts-w-line-diff := conflicts-w-line-diff ∪ {merge-commit,conflicts(l,r,'line)}
        conflicts-w-line-diff-after-clang-format := conflicts-w-line-diff-after-clang-format ∪ {merge-commit,conflicts(format(l),format(r),'line)}
        conflicts-w-AST-diff := conflicts-w-AST-diff ∪ {merge-commit,conflicts(l,r,'ast)}

We could then look at the above results for insight into different
classes of differences and merges, impact of differencing at AST
vs. line, impact of formatting, and if we re-run after adding new diff
operations it could yield insight into the impact of new diff
operations.
