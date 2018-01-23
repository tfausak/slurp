# Slurp

A single liberal unified registry of Haskell packages.
This is an implementation of the SLURP proposal, which can be found here:
<https://github.com/haskell/ecosystem-proposals/pull/4>.

This project comes with a few executables:

-   `slurp-server`: The SLURP server.
    It implements the package name registry described in the proposal.

-   `slurp-hackage`: A utility script to populate the SLURP server with packages from Hackage.
    It grabs the list of packages from Hackage and adds each of them to SLURP.
    It may take a while to run.
    The list of Hackage packages as of 2018-01-23 is available here:
    <https://gist.github.com/tfausak/de84b19e6d84c180130c00caf6fe8279>.
