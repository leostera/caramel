# Building from Source

Caramel is built with OCaml, so it needs a working opam installation.

Additionally, we use `make`, since it makes our CI steps pretty minimal.

### Getting Started

The next scripts show how to set up Caramel and create a release:

```zsh
# install ocaml first
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# clone the repository
git clone git@github.com:AbstractMachinesLab/caramel.git
cd caramel

# install all required dependencies
make setup

# compile projects and runs the tests
make test

# installs project from sources
make install
```

### Common Tasks

To bootstrap the repository, you can run `make setup build`.

To compile the manual, you can run `make manual`.

To run all the tests, you can run `make test`.

To install the local version, you can run `make install`.

To format all the code, you can run `make fmt`.

To create a release of Caramel, you can run `make release`, or `make release.win` on
Windows.
