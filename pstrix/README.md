# pstrix
A wrapper tool for adding pure-past subformulae to the input language of **Strix**.

## Installation instructions:
[ghc](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) are required.  
After having installed ghc and Cabal and cloned the repository, execute the command `cabal install` from the root directory (where the .cabal file is located).

To use **pstrix**, **Strix** must be installed and on the path. See the [website](https://strix.model.in.tum.de/) for Strix for download links and documentation.  
In addition, **ppLTLTT** must be on the path.

## Usage instructions:
Given an pLTL formula with pure-past subformulae, **pstrix** extracts the pure-past subformulae and calls **ppLTLTT** to convert these into temporal testers. These temporal testers will then be encoded into the specification, which is then sent to **Strix** for synthesis.

### Options
Available options:
```
  --include-boolean        Abstract maximal pure-past subformulae, including
                           Boolean connectives.
  --onehot                 Use one-hot encoding of temporal testers' states
  STRIX_ARGS               Arguments to forward to Strix
  -f,--formula FORMULA     Formula to process
  -F,--formula-file FILE   File to process
  -h,--help                Show this help text

```

By default, **pstrix** will abstract away maximal pure-past subformulae that do not include Boolean connectives. For example, the formula "Y p & Y q" will by default be converted into "z1 & z2", resulting in two temporal testers; one for "Y p" and one for "Y q". With the `--include-boolean` option, **pstrix** will abstract away maximal pure-past subformulae *including* Boolean connectives, so that the entire formula "Y p & Y q" will be replaced by a single monitor variable "z!".

Any arguments not recognized by **pstrix** are passed to **strix** at the end of the process.

States of temporal testers are by default encoded with a binary encoding, so that a temporal tester with *n* states requires log_2(n) variables, rounded up. The user may opt for a one-hot encoding instead by passing the `--onehot` flag. In this case one variable per state is required.


### Syntax of pLTL formulae
The formula parser for **pstrix** is written to resemble **Strix**'s formula parser as closely as possible.
See the following [documentation](https://gitlab.lrz.de/i7/owl/-/blob/main/doc/FORMATS.md) for a description.

The following past temporal operators are supported by **pstrix**:

| Symbol | Description |
| ----------- | ----------- |
| H | Historically |
| O | Once |
| Y | Yesterday |
| T | Weak Yesterday |
| S | Since |
| Z | Weak Since |

### Example of usage

Assuming that **pstrix**, **ppLTLTT**, and **strix** are on the path, the following command will generate an automaton representing a winning strategy for an arbiter specification with one client:
```
pstrix -f 'G (g1 -> Y (!g1 S r1)) & (G F r1 -> G F g1)' --ins='r1' --outs='g1'
```
This will give the following output:
```
REALIZABLE
HOA: v1
tool: "strix" "21.0.0"
States: 2
Start: 0
AP: 4 "r1" "g1" "z1" "z1_0"
controllable-AP: 1 2 3
acc-name: all
Acceptance: 0 t
--BODY--
State: 0 "[0]"
[(!0) & (!(1 | (2 | 3)))] 0
[(0) & (!(1 | (2 | 3)))] 1
State: 1 "[1]"
[(!0) & (!(1 | !(2 & 3)))] 1
[(!0) & (1 & (2 & 3))] 0
[(0) & (1 & (2 & 3))] 1
--END--
```
