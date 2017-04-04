# markov
An AI library to encode, solve and simulate Markov decision processes in Haskell, both deterministically and stochastically, with examples and documentation.  Also includes an epsilon-greedy machine-learning algorithm.  Written in 2012, and updated in 2017 to make it compatible with more recent versions of the libraries.

## Usage
The current version contains a very simple test driver in `tests`, and can be compiled as a library.  Most of my usage of it, however was from `ghci -XBangPatterns ADP.hs`, which loads all the libraries into the Haskell REPL.  I advise compiling `MDP.ha`, `MDPSim.hs` and `ADP.hs` with `ghc -XBangPatterns -O -c` first.

The documentation in the `doc` directory, which come with LaTeX source, gives examples of usage, and there is documentation of each exported function in the source.

## Installation
The library depends on the MonadRandom package and the hmatrix package, which in turn depends on the BLAS and Lapack libraries.  This version compiles and runs with hmatrix-0.18, but the interface has changed and broken it before.  On Debian or Ubuntu Linux, this installs the dependencies:

    sudo apt-get install libblas-dev liblapack-dev
    stack init
    stack install hmatrix MonadRandom
    
On OS X, BLAS and Lapack should already be installed.  On Windows, see the documentation for hmatrix at https://github.com/AlbertoRuiz/hmatrix/blob/master/INSTALL.md to see multiple ways to get that package and its dependencies to work.
