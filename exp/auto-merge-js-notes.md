# Setup

The benchmarks listed in the `exp/results.org` file for ast-merge testing
were checked out out to a directory, `js-benchmarks/` using the command
`git clone git@git.grammatech.com:synthesis/benchmark/${benchmark}`.
The existing `exp/bin/conflicting-merges` script in resolve was utilized
to find the merges in the benchmarks.

# Benchmark Evaluation

From the existing benchmarks, I attempted to determine which, if any, would
be ammendable to our approach, with the results shown below.  Those benchmarks
which would not work with auto-merge are listed with a brief description of
the issue(s), while those which will work with auto-merge are listed with the
time required to run the tests (a number useful for estimating the time
required for auto-merge to execute).

## Benchmarks with issues

| Benchmark      | Issues                                                                  |
|----------------|-------------------------------------------------------------------------|
| axios          | Test suite does not succeed on fife (needs browser among other things). |
| body-parser    | No conflicted merges.                                                   |
| chalk          | No conflicted merged.                                                   |
| generator      | Test suite does not succeed on fife.                                    |
| minimist       | No conflicted merges.                                                   |
| node-fs-extra  | No conflicted merges.                                                   |
| node-glob      | No conflicted merges.                                                   |
| rxjs           | Too many TypeScript files for our approach to work reasonably.          |
| through2       | Test suite does not succeed on fife.                                    |
| tslib          | No test suite.                                                          |

## Benchmark Testing Time

| Benchmark      | Test command runtime (sec) |
|----------------|----------------------------|
| async          |                      0:25  |
| bluebird       |                      0:04  |
| classnames     |                      0:05  |
| colors.js      |                      0:01  |
| commander.js   |                      0:27  |
| express        |                      0:04  |
| moment         |                      0:25  |
| node-uuid      |                      0:01  |
| prop-types     |                      0:08  |
| request        |                      0:35  |
| underscore     |                      0:08  |
| yargs          |                      0:19  |

# Experiments

The section below contains a listing of experiments performed on
the benchmarks above which could be utilized with auto-merge.

For these experiments, `auto-merge` was built with a heap size
(`LISP_HEAP` or `--dynamic-space-size`) of 129024 (Mb) and a stack
size (`LISP_STACK` or `--control-stack-size`) of 128 (Mb).
Experiments were executed using the
`exp/auto-merge-js/try-all-js-auto-merges` script.

As input to the `try-all-js-auto-merges` script, the results of
executing the `exp/bin/conflicting-merges` script on the good
benchmarks above (e.g. async, bluebird, ..., yargs) were filtered thru
`grep -v "no-conflict"|cut -f 1,3,4,5` into a
`try-all-js-merges-input` text file.  The file follows the format
expected by the existing `exp/bin/try-merge` script, as shown below:

```
<benchmark name> <merge base hash> <left branch hash> <right branch hash>
```

Example:
```
commander.js 41fdf892 ba74d56 4788f70
commander.js 534c63a5 d17cf57 ebdd789
classnames 0469d5f3 decb0ca 2a07e33
colors.js 9f3ace44 ca0c69f 7176fc3
moment d3f7978f 60bfa24f 0bcf8d1e
moment aea66a8c 2cbad13c 50beb2eb
async 914fa108 c64997e 2a13d08
moment 6ef3e64f 49700062 ae013f90
```

## Experiment 2020.02.01

This experiment was executed the with conflicts from the async,
bluebird, classnames, colors.js, commander.js, express,
moment, node-uuid, prop-types, request, underscore, and yargs
benchmarks.  There were 104 conflicted merges in total.

The following command line was utilized:

```
cat try-all-js-merges-input | \
exp/auto-merge-js/try-all-js-auto-merges js-benchmarks/ \
                                         --log-level info \
                                         --pop-size 128 \
                                         --evolve \
                                         --num-threads 28 \
                                         --max-time 43200 \
                                         --out-dir out
```

The results breakdown was as follows:

| Result Type                             | Count |
|-----------------------------------------|-------|
| Total no test suite                     |    22 |
| Total setup error                       |    63 |
| Total auto-merge error                  |     3 |
| Total no resolution found               |    12 |
| Total resolution found before evolution |     4 |
| Total resolution found during evolution |     0 |

## Experiment 2020.02.14

This experiment was conducted with same benchmarks as above.
The following summarizes the main differences between
executions (as discussed in meeting on 2/4):

- Conflicts in the test directory were ignored by auto-merge.
As before, the test directory from the merge resolution commit
was utilized to determine merge success.
- To determine setup success, the project was installed and
tested at (and only at) the commit hash representing the
merge resolution.
- The test script includes a call to `npm install` to handle
the case where dependencies were added.
- JSON files in projects are represented as JSON software
objects instead of text.
- acorn parameters were relaxed to improve the probablity
of parsing JavaScript source into AST and resolve
the auto-merge errors from above.
- A fixed random seed was utilized so future experiments
are more comparable.

The following command line was utilized:

```
cat try-all-js-merges-input | \
exp/auto-merge-js/try-all-js-auto-merges js-benchmarks/ \
                                         --log-level info \
                                         --pop-size 128 \
                                         --evolve \
                                         --num-threads 28 \
                                         --max-time 43200 \
                                         --out-dir out
                                         --seed exp/auto-merge-js/sbcl-seed
```

Commit hashes - SEL:e8bdef40,resolve:241b4e6
LISP - sbcl

The results breakdown was as follows:

| Result Type                             | Count |
|-----------------------------------------|-------|
| Total no test suite                     |    17 |
| Total setup error                       |    64 |
| Total auto-merge error                  |     5 |
| Total no resolution found               |    10 |
| Total resolution found before evolution |     8 |
| Total resolution found during evolution |     0 |

## Experiment 2020.02.15

This experiment was conducted with same benchmarks and properties
as 2020.02.14.  In this experiment, evolution was not utilized.

The following command line was utilized:

```
cat try-all-js-merges-input | \
exp/auto-merge-js/try-all-js-auto-merges js-benchmarks/ \
                                         --log-level info \
                                         --pop-size 1536 \
                                         --num-threads 28 \
                                         --out-dir out
                                         --seed exp/auto-merge-js/sbcl-seed
```

Commit hashes - SEL:e8bdef40,resolve:241b4e6
LISP - sbcl

The results breakdown was as follows:

| Result Type                             | Count |
|-----------------------------------------|-------|
| Total no test suite                     |    19 |
| Total setup error                       |    61 |
| Total auto-merge error                  |     8 |
| Total no resolution found               |     8 |
| Total resolution found before evolution |     8 |
| Total resolution found during evolution |     0 |

## Experiment 2020.02.16

This experiment was conducted using CCL and with some minor
SEL improvements for running CCL in a multi-threaded
environment to determine if the hard errors in auto-merge
reported previously were related to the use of SBCL.

The following command line was utilized:
```
cat try-all-js-merges-input | \
exp/auto-merge-js/try-all-js-auto-merges js-benchmarks/ \
                                         --log-level info \
                                         --pop-size 128 \
                                         --evolve \
                                         --num-threads 28 \
                                         --max-time 43200 \
                                         --out-dir out
                                         --seed exp/auto-merge-js/ccl-seed
```

Commit hashes - SEL:1e646ed7,resolve:94a34b1
LISP - ccl

In this experiment, there were no hard errors in auto-merge.

# Experiment 2020.03.31

This experiment was conducted with same benchmarks and properties
as 2020.02.14 to determine if recent changes may have caused regressions
to auto-merge.

The following command line was utilized:

```
cat try-all-js-merges-input | \
exp/auto-merge-js/try-all-js-auto-merges js-benchmarks/ \
                                         --log-level info \
                                         --pop-size 128 \
                                         --evolve \
                                         --num-threads 16 \
                                         --max-time 43200 \
                                         --out-dir out
                                         --seed exp/auto-merge-js/sbcl-seed
```

In this experiment, TMP\_DIR was also set to a non-standard location
as prior runs had issues with running out of disk space.  Additionally,
fewer threads were utilized.  Both of these seem to have had a positive
impact on reducing the number of hard errors.

Commit hashes - SEL:64a33dc,resolve:d5dcddc
LISP - sbcl

The results breakdown was as follows:

| Result Type                             | Count |
|-----------------------------------------|-------|
| Total no test suite                     |    19 |
| Total setup error                       |    62 |
| Total auto-merge error                  |     3 |
| Total no resolution found               |    11 |
| Total resolution found before evolution |     9 |
| Total resolution found during evolution |     0 |

# Thoughts for improvement

## Break test suites up into individual tests

Currently, when running the test suite, a single value
(the exit code) is returned by the test suite indicating
success or failure.  Ideally, we would like to have an
value representing success or failure for each test
to allow for leveraging lexicase selection and generational
evolution as currently implmented in the evolutionary loop.
Unfortunately, the test suites for each project are
each unique and they leverage different test frameworks,
which makes writing a general approach for this difficult.
