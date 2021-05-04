Caramel works on macOS, Linux, and Windows. It is a single binary, and it has no
external dependencies.

The only prerequisite to run the code you compile with Caramel, is Erlang or
Elixir. The [Elixir installation](https://elixir-lang.org/install.html) page
lists many ways to install it comfortably, we suggest you to follow that step
before continuing.

### Manual binary installation

You can manuall install Caramel as well on Windows, macOS, or Linux by downloading
the zipped release from Github:
[github.com/AbstractMachines/caramel/releases](https://github.com/AbstractMachinesLab/caramel/releases/#user-content-assets).

### Using a package manager

> **NOTE**: We are working on supporting these plugins but we could use a hand
> with them. If you're interested in contributing please [join us on
> Discord](http://discord.caramel.run)

<br />

If you are an **Elixir programmer**, Caramel can be installed with `mix`,
include the mix plugin in your project as a dependency and enable the compiler:

```elixir
def project do
  [
    ...
    compilers: [:caramel] ++ Mix.compilers(),
    caramel_paths: ["src"],
    caramel_release: "v0.1.1"
  ]
end

defp deps do
  {:mix_caramel, github: "AbstractMachinesLab/mix_caramel", branch: :main}
end
```

<br />

If you are an **Erlang programmer**, Caramel can be installed with `rebar3`,
but we are still working out the plugin. If you're interested in helping out
[please reach us out in
Discord](http://discord.caramel.run)!

```erlang
{plugins, [
  {rebar3_caramel, {git, "https://github.com/AbstractMachinesLab/rebar3_caramel.git", {branch, "main"}}}
]}.
```

<!--
<br />

If you are an **OCaml programmer**, Caramel can be installed with `opam`.

```sh
opam install caramel
```
-->

<br />

**Other platforms.** If you'd like Caramel to be easier to install in your favorite platform, feel
free to [Open a Github
Issue](https://github.com/AbstractMachinesLab/caramel/issues/new) and we can
talk about making it happen.

### Install from Sources

Check the [Building from Source](../contrib/building.md) section for up to
date instructions.

If you have OCaml (opam) and the source code of Caramel, you can install it by running `make install`.
