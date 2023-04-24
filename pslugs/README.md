# pslugs
A wrapper tool for adding pure-past subformulae to the input language of **slugs**.

## Installation instructions:
[ghc](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) are required.  
After having installed ghc and Cabal and cloned the repository, execute the command `cabal install` from the root directory (where the .cabal file is located).

To use **pslugs**, **slugs** must be installed and on the path. See slugs' GitHub [repository](https://github.com/VerifiableRobotics/slugs) for download links and documentation.  
In addition, **ppLTLTT** must be on the path.

## Usage instructions:
Given a GR(1) specification with pure-past subformulae in *slugsin* format, **pslugs** extracts the pure-past subformulae and calls **ppLTLTT** to convert these into temporal testers. These temporal testers will then be encoded into the specification, which is then sent to **slugs** for synthesis.

### Options
Available options:
```
  --include-boolean        Abstract subformulae with top-level boolean
                           connectives (if they contain past operators)
  --onehot                 Use one-hot encoding of temporal testers' states
  FILES|ARGS               Arguments to forward to SLUGS and files
  -h,--help                Show this help text```
```
By default, **pslugs** will abstract away maximal pure-past subformulae that do not include Boolean connectives. For example, the formula "Y p & Y q" will by default be converted into "z1 & z2", resulting in two temporal testers; one for "Y p" and one for "Y q". With the `--include-boolean` option, **pslugs** will abstract away maximal pure-past subformulae *including* Boolean connectives, so that the entire formula "Y p & Y q" will be replaced by a single monitor variable "z!".

Any arguments not recognized by **pslugs** are passed to **slugs** at the end of the process.

States of temporal testers are by default encoded with a binary encoding, so that a temporal tester with *n* states requires log_2(n) variables, rounded up. The user may opt for a one-hot encoding instead by passing the `--onehot` flag. In this case one variable per state is required.

### The slugsin format
The *slugsin* parser for **pslugs** is written to resemble slugs' *slugsin* parser as closely as possible.
The *slugsin* format is described [here](https://github.com/VerifiableRobotics/slugs/blob/master/doc/input_formats.md). 

Additionally, the following past temporal operators are supported (note that past operators may **not** occur inside of memory buffers):

| Symbol |  Description |
| ----------- | ----------- |
| H | Historically |
| O | Once |
| Y | Yesterday |
| T | Weak Yesterday |
| S | Since |
| Z | Weak Since |

### Example of usage
Below is an example of an arbiter specification with one client:
```
[INPUT]
r1

[OUTPUT]
g1

[SYS_TRANS]
| !g1 (Y S !g1 r1)

[SYS_LIVENESS]
Z !r1 g1

```

Assuming that **pslugs**, **ppLTLTT**, and **slugs** are on the path, and that the file `arbiter1` exists in the current directory, containing the above specification, the following command will check for realizability and return a winning strategy:
```
pslugs --explicitStrategy arbiter1
```
This will give the following output:
```
SLUGS: SmaLl bUt complete Gr(1) Synthesis tool (see the documentation for an author list).
RESULT: Specification is realizable.
State 0 with rank 0 -> <r1:0, g1:0, z1@0.0.1:0, z1:0, z2@0.0.1:0, z2:1>
	With successors : 0, 1
State 1 with rank 0 -> <r1:1, g1:0, z1@0.0.1:0, z1:0, z2@0.0.1:0, z2:0>
	With successors : 2, 3
State 2 with rank 0 -> <r1:0, g1:1, z1@0.0.1:1, z1:1, z2@0.0.1:1, z2:1>
	With successors : 0, 1
State 3 with rank 0 -> <r1:1, g1:1, z1@0.0.1:1, z1:1, z2@0.0.1:1, z2:1>
	With successors : 4, 5
State 4 with rank 0 -> <r1:0, g1:0, z1@0.0.1:1, z1:1, z2@0.0.1:0, z2:1>
	With successors : 4, 5
State 5 with rank 0 -> <r1:1, g1:0, z1@0.0.1:1, z1:1, z2@0.0.1:0, z2:0>
	With successors : 2, 3```
```
