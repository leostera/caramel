# Installation

Caramel works on macOS, Linux, and Windows. It is a single binary, and it has no
external dependencies.

### Using a package manager

If you are an **Elixir programmer**, Caramel can be installed with `mix`, include the mix plugin in your project as a dependency and enable the compiler:

```elixir
def project do
  [
    ...
    compilers: [:caramel] ++ Mix.compilers(),
    caramel_paths: ["src"],
    caramel_release: "v0.1.0"
  ]
end

defp deps do
  {:mix_caramel, github: "AbstractMachinesLab/mix_caramel"}
end
```

If you are an **Erlang programmer**, Caramel can be installed with `rebar3`, just include the rebar3 plugin in your project as a dependency:

```erlang
{plugins, [
  {rebar3_caramel, ".*", {git, "https://github.com/AbstractMachinesLab/rebar3_caramel.git", {branch, "main"}}}
]}.
```

If you are an **OCaml programmer**, Caramel can be installed with `opam`.

```sh
opam install caramel
```

**Other platforms.** If you'd like Caramel to be easier to install in your favorite platform, feel
free to [Open a Github
Issue](https://github.com/AbstractMachinesLab/caramel/issues/new) and we can
talk about making it happen.

### Manual binary installation

You can manuall install Caramel as well on Windows, macOS, or Linux by downloading
the zipped release from Github:
[github.com/AbstractMachines/caramel/releases](https://github.com/AbstractMachinesLab/caramel/releases/#user-content-assets).

### Install from Sources

Check the [Building from Source](../contrib/building.md) section for up to
date instructions.

If you have Rust and the source code of Caramel, you can install it by running `make install`.
