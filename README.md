# ppLTLTT with auxiliary tools
A tool for converting pure-past LTL formulae to temporal testers, along with auxiliary tools that leverage **ppLTLTT** to extend the input language of certain existing tools from LTL to LTL+Past (LTL with atomically occurring past subformulae).

## Installation instructions:
All tools require the Haskell Cabal, available [here](https://www.haskell.org/cabal/).  
A Haskell compiler is also required, available [here](https://www.haskell.org/ghc/).

After having installed Cabal and cloned the repository, simply execute the command `cabal install` from the root directory (where the .cabal file is located).

For information specific to each tool, see the README in the corresponding directory.  
The *examples* directory contains arbiter specifications.  
The *scripts* directory contains Bash scripts required to replicate experiments in the tool paper for **ppLTLTT**.

All tools were written, compiled and tested on Linux.
