Resolve
=======

TRL: 6

Software AST-based diff calculation, display, and automated resolution.

<div style="color: #eeeeec; background-color: #2e3436; padding:0.25em; max-width:50em;">
<pre>
<span style="color: #b4fa70; background-color: #2e3436;"># Example output of git configured to use Resolve's <span style="background-color: #555753;">ast-diff</span> command</span>
<span style="color: #b4fa70; background-color: #2e3436;">$ </span><span style="background-color: #2e3436; font-weight: bold;">git diff HEAD~2..HEAD~1 -- src/event.js</span><span style="background-color: #2e3436;">
ast-diff src/event.js
index cb70e8eb..b63b93b9 100644
</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 589</span><span style="background-color: #2e3436;">
// synthetic events by interrupting progress until reinvoked in response to
// *native* events that it fires directly, ensuring that state changes have
// already occurred before other listeners are invoked.
function leverageNative( el, type, </span><span style="color: #ff4b4b; background-color: #2e3436;">[-forceAdd, -]</span><span style="color: #b4fa70; background-color: #2e3436;">{+expectSync+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-allowAsync-]</span><span style="background-color: #2e3436;"> ) </span><span style="color: #b4fa70; background-color: #2e3436;">{+{</span><span style="background-color: #2e3436;">

</span><span style="color: #b4fa70; background-color: #2e3436;">        // Missing expectSync indicates a trigger call, which must force setup through jQuery.event.add</span><span style="background-color: #2e3436;">
</span><span style="color: #b4fa70; background-color: #2e3436;">        +}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-{</span><span style="background-color: #2e3436;">

</span><span style="color: #ff4b4b; background-color: #2e3436;">        // Setup must go through jQuery.event.add-]</span><span style="background-color: #2e3436;">
</span><span style="color: #ff4b4b; background-color: #2e3436;">        </span><span style="background-color: #2e3436;">if ( </span><span style="color: #b4fa70; background-color: #2e3436;">{+!expectSync+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-forceAdd-]</span><span style="background-color: #2e3436;"> ) {
                jQuery.event.add( el, type, </span><span style="color: #b4fa70; background-color: #2e3436;">{+returnTrue+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-forceAdd-]</span><span style="background-color: #2e3436;"> );
                return;
        }

</span><span style="color: #8cc4ff; background-color: #2e3436;">line: 603</span><span style="background-color: #2e3436;">
        // Register the controller as a special universal handler for all event namespaces
        dataPriv.set( el, type, </span><span style="color: #b4fa70; background-color: #2e3436;">{+false+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-forceAdd-]</span><span style="background-color: #2e3436;"> );
        jQuery.event.add( el, type, {
                namespace: false,
                handler: function( event ) {
                        var </span><span style="color: #b4fa70; background-color: #2e3436;">{+notAsync+}</span><span style="color: #ff4b4b; background-color: #2e3436;">[-maybeAsync-]</span><span style="background-color: #2e3436;">, result,
                                saved = dataPriv.get( this, type );</span></pre>
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

#### Limitations

Currently the reliability of printed diffs is limited. In particular
irregular indentation may result in malformed diffs or errors during
diff printing.

This is in consequence of the high-level representation of ASTs in
Resolve: because the representation abstracts away details such as
indentation and syntactic delimiters and separators, they have to be
carefully reconstructed to present the result to humans.

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

Resolve supports any language that can be parsed by [tree-sitter][]
and represented by [SEL][]. This includes, but is not limited to:

- C
- C++
- JavaScript
- Python
- TypeScript
- Rust

## Copyright and License

Copyright (c) 2020 GrammaTech, Inc.

See [LICENSE.txt](LICENSE.txt) for more information.

We ask that all contributors complete our Contributor License
Agreement (CLA), which can be found at
[GrammaTech-CLA-resolve.pdf](./GrammaTech-CLA-resolve.pdf),
and email the completed form to `CLA@GrammaTech.com`.  Under this
agreement contributors retain the copyright to their work but grants
GrammaTech unlimited license to the work.

## Acknowledgment
This material is based upon work supported by the Defense Advanced
Research Projects Agency (DARPA) under Contract No. D17PC00096. Any
opinions, findings and conclusions or recommendations expressed in
this material are those of the author(s) and do not necessarily
reflect the views of the Defense Advanced Research Projects Agency
(DARPA); or its Contracting Agent, the U.S. Department of the
Interior, Interior Business Center, Acquisition Services Directorate,
Division III.

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[SEL]: https://github.com/GrammaTech/sel
