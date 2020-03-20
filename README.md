Resolve
=======

TRL: 5

Software AST-based diff calculation, display, and automated resolution.

<div style="color: #eeeeec; background-color: #2e3436; padding:0.25em; max-width:40em;">
<pre>
<span style="color: #b4fa70; background-color: #2e3436;"># Example output of git configured to use Resolve's <span style="background-color: #555753;">ast-diff</span> command</span>
<span style="color: #b4fa70; background-color: #2e3436;">$ </span><span style="background-color: #2e3436; font-weight: bold;">git diff HEAD~10..HEAD -- components/fix-compilation.lisp </span><span style="background-color: #2e3436;">
ast-diff components/fix-compilation.lisp
index ef102965..68b4f128 100644
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 66</span><span style="background-color: #2e3436;">
    (clang-tidy obj)
    (loop :for attempt :below max-attempts :do
       ;; Compile
       (</span><span style="color: #b4fa70; background-color: #2e3436;">{+with-temporary-file+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-with-temp-file-]</span><span style="background-color: #2e3436;"> (</span><span style="color: #b4fa70; background-color: #2e3436;">{+:pathname +}</span><span style="background-color: #2e3436;">bin)
         (multiple-value-bind (bin errno stderr)
             (ignore-phenome-errors
              (phenome obj :bin bin))
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 163</span><span style="background-color: #2e3436;">
        (variable-name (aref match-data variable-name-index)))
    ;; Insert a declaration and initialization
    (setf (lines obj)
          (append (</span><span style="color: #b4fa70; background-color: #2e3436;">{+subseq lines 0+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-take-]</span><span style="background-color: #2e3436;"> (1- line-number)</span><span style="color: #ff4b4b; background-color: #2e3436;">[- lines-]</span><span style="background-color: #2e3436;">)
                  (list (format nil "~a ~a;" random-type variable-name)
                        (format nil "~a = ~a;"
                                variable-name
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 170</span><span style="background-color: #2e3436;">
                                (- (random (expt 2 32)) (expt 2 31))))
                  (</span><span style="color: #b4fa70; background-color: #2e3436;">{+subseq lines+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-drop-]</span><span style="background-color: #2e3436;"> (1- line-number)</span><span style="color: #ff4b4b; background-color: #2e3436;">[- lines-]</span><span style="background-color: #2e3436;">))))
  obj)

(register-fixer
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 230</span><span style="background-color: #2e3436;">
         (lines (lines obj))
         (orig (nth (1- line-number) lines)))
    (setf (lines obj)
          (append (</span><span style="color: #b4fa70; background-color: #2e3436;">{+subseq lines 0+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-take-]</span><span style="background-color: #2e3436;"> (1- line-number)</span><span style="color: #ff4b4b; background-color: #2e3436;">[- lines-]</span><span style="background-color: #2e3436;">)
                  (list (concatenate 'string
                          (subseq orig 0 col-number)
                          new-expression
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 237</span><span style="background-color: #2e3436;">
                          (subseq orig col-number)))
                  (</span><span style="color: #b4fa70; background-color: #2e3436;">{+subseq lines+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-drop-]</span><span style="background-color: #2e3436;"> line-number</span><span style="color: #ff4b4b; background-color: #2e3436;">[- lines-]</span><span style="background-color: #2e3436;">))))
  obj)</span></pre>
</div>

## Abstract
Resolve implements multi-lingual semantic differencing of software
source code and implements an automated technique of merge conflict
resolution.  This provides developers with more meaningful views of
software changes and it frees developers from the tedious manual task
of merge conflict resolution.

## Use Cases

### Improved difference calculation and view
Senior developers spend a significant portion of their time reviewing
the work products of other developers.  The primary mode of
representing these work products are as differences against the
existing code base.  By calculating syntactically aware differences
over software abstract syntax trees (ASTs) we provide more informative
and meaningful views of software changes.

### Automated merge conflict resolution
On project with multiple developers, a significant portion of time is
typically spent manually resolving conflicting changes between
parallel branches of development.  By automatically resolving merge
conflicts we are able to save projects a significant amount of
expensive developer time and free developers from the often tedious
job of manual conflict resolution.

## Tools
- command-line AST differencing
- web-based AST differencing
- test-suite guided merge conflict resolution

## Language Support
- JavaScript via ESTree ASTs (w/acorn)
- C/C++ via Clang ASTs
- Java via JavaParser ASTs
- Common-Lisp via Eclector

## License
Resolve, and any accompanying documentation or other materials are
GrammaTech, Inc. (GrammaTech) Proprietary Information.

See [LICENSE.txt](LICENSE.txt) for more information.
