# `nix-dram`

![`nix flake check` status](https://github.com/dramforever/nix-dram/workflows/nix%20flake%20check/badge.svg)

*Nix with a modified frontend, by dramforever*

## Nix Flake

This repository contains a Nix Flake. To use it, use the following Flake URL:

```plain
github:dramforever/nix-dram
```

The modified Nix is available as `defaultApp`:

```console
$ nix run github:dramforever/nix-dram -- --version
nix (Nix) 2.4pre20201205_a5d85d0
```

Binaries of this flake is available on Cachix. Set it up with:

```console
$ cachix use dram
```

Check out [the contents of this flake](#contents-of-this-flake)

---

## What is this about?

As a [Nix Flakes][nix-flakes] user, I was constantly typing `nixpkgs` over and
over again in commands like:

[nix-flakes]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html

```console
$ nix run nixpkgs#hello
$ nix search nixpkgs hello
```

See the issue? The Flakes version of Nix does not have an idea of a 'default'
flake of any sort. At all. The best you can do is rename `nixpkgs` to `n`, but
still it frankly sucks.

Introducing `nix-dram`:

```console
$ nix run hello
$ nix search hello
```

 Now there's no `nixpkgs` in sight. (You do need to add a `default` Flake to
your registry though.)

## What's new?

There are two patches over Nix in this repository:

- `nix-flake-default.patch`: This changes the Nix CLI so that it parses
  `INSTALLABLE` arguments differently. The usage of the command `nix search` was
  also changed. See below for details.

- `nix-search-meta.patch`: This is not a very user-visible change. It basically
  alters `nix search` so that the `--json` option now dumps the entire `meta`
  attribute of the package.

  This is mainly used to support `nix-search-pretty` so that it has more
  information to work with, but could support other tooling as well.

These changes are *incompatible* but is predicted to minimally affect

The documentation has *not* yet been changed to reflect the changes.

### Changes to `INSTALLABLE`

An `INSTALLABLE` command line argument, such as that of `nix run`, now has and
additional rule when parsing:

- If an `INSTALLABLE` looks like an attribute path, e.g. `foo.bar`, it is looked
  up as if it were `flake:default#foo.bar`.

  'Looks like an attribute path' is
  defined as fully matching the regular expression
  `[a-zA-Z0-9-_][a-zA-Z0-9-._]*`.

- Otherwise it is treated as a Flake `INSTALLABLE` as before.

`flake:default` is an [indirect Flake reference to the registry][docs-indirect].
`flake:default` can be set using `nix registry` to refer to a personal flake
containing outputs such as packages or NixOS configurations. It is not
necessarily (but can be) the same as `flake:nixpkgs`. To avoid unnecessarily
prioritizing Nixpkgs, `flake:nixpkgs` is not used.

Command line completion of the new `INSTALLABLE` syntax *is* supported.

[docs-indirect]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#types

(In cases where a Flake URL is expected (such as the arguments of
`--override-flake`), the behavior is not changed, i.e. using `nixpkgs` there
still means `--override-flake flake:nixpkgs [...]` .)

The following table shows the incompatibilities. The 'Legacy
compatibility' columns shows a syntax to use that both `nix-dram` and
`nixFlakes` will interpret according to the column 'Meaning in `nixFlakes`'.

| Syntax | Meaning in `nix-dram` | Meaning in `nixFlakes` | Legacy compatibility |
|---|---|---|---|
| `blender` | `flake:default#blender` | `flake:blender` | `blender#` |
| `xorg.xclock` | `flake:default#xorg.xclock` | `./xorg.xclock/` (directory) | `./xorg.xclock` or `xorg.xclock/` |

Here are examples of `INSTALLABLE` syntax that is unaffected:

| Syntax | Meaning |
|---|---|
| `.` | Current directory |
| `flake:blender` | Flake registry reference to `blender` |
| `./nix-dram` | The directory `nix-dram` within the current directory |
| `github:dramforever/nix-dram` | The GitHub repository [dramforever/nix-dram][dramforever-nix-dram] |
| `nixpkgs#hello` | `flake:nixpkgs#hello` |

[dramforever-nix-dram]: https://github.com/dramforever/nix-dram

As one can see the change affects a relative small number of use cases, and
incompatibilities can be worked around by appending one single character, which
even improves readability (though arguably so).

### Changes to `nix search`

The `nix search` commandS used to have this syntax:

```console
$ nix search [options] INSTALLABLE [KEYWORD]
```

It now has this syntax:

```console
$ nix search [options] [KEYWORD]
```

With the `INSTALLABLE` argument moved into an option, defaulting to `flake:default`:

```plain
  -i, --installable INSTALLABLE     Search within this installable
```

The following table shows a comparison of the syntax.

| `nix-dram` | `nixFlakes` |
|---|---|
| `nix search hello` | `nix search default hello` |
| `nix search -i github:NixOS/nixpkgs hello` | `nix search github:NixOS/nixpkgs hello` |
| `nix search --expr foo hello` | `nix search --expr foo '' hello` |
| `nix search --file foo hello` | `nix search --file foo '' hello` |

### `nix-search-pretty`

The `nix search` command, when given the `--json` option, now dumps the `meta`
attribute of each search result into a JSON property called `meta`.

```jsonc
{
  "legacyPackages.x86_64-linux.hello": {
    "pname": "hello",
    "version": "2.10",
    "description": "A program that produces a familiar, friendly greeting",
    "meta": {
      "available": true,
      "broken": false,
      "changelog": "https://git.savannah.gnu.org/cgit/hello.git/plain/NEWS?h=v2.10",
      "description": "A program that produces a familiar, friendly greeting",
      // ...
    }
  },

  // ...
}
```

`nix-search-pretty` is an example program that takes in this JSON and produces
colorized search output (Recorded using [termtosvg]):

[termtosvg]: https://github.com/nbedos/termtosvg

![Demonstration of `nix-search-pretty`](images/nix-search-demo.svg)

## More on the design

In the NixOS version of `nixFlakes`, it is in fact a conscious design choice
that you have to type `nixpkgs` every time. I tried making [a feature
request][the-feature-request] but got immmediately shut down, with an
explanation that `flake:nixpkgs` should not be prioritized as a 'default'. I
guess `flake:default` it is? In any case, it seems that the main author is open
to improvements, but is not *that* keen on the idea.

[the-feature-request]: https://github.com/NixOS/nix/issues/4438

The basic idea of `nix-dram` is based on a slightly different prediction of how
Flakes will be used. Namely, it is predicted that users will create their own
personal Flake, referencing other Flakes as inputs. It will possibly provide a
package set for use in `nix` commands, various `nixosConfigurations`, and so on.

'Smaller' Flakes will exists and possibly even in great numbers, but each user
will have their own 'favorite' Flake to be used for most purposes. In `nix-dram`
that flake will be assigned `flake:default`. This indeed seems to be the use
case with popular demonstration repositories such as [nixflk] showing this
approach. Will this be how we use Flakes in the future? We will have to wait.

[nixflk]: https://github.com/nrdxp/nixflk

This project is called `nix-dram` with the specific intention that it will
*never* be official. This is just one person, me, trying out one point in the
design space. That's how free software works, isn't it?

---

## Contents of this flake

```console
$ nix flake show github:dramforever/nix-dram
github:dramforever/nix-dram/[...]
├───checks
│   ├───aarch64-linux
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   ├───i686-linux
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   ├───x86_64-darwin
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   └───x86_64-linux
│       ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│       ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│       └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
├───defaultApp
│   ├───aarch64-linux: app
│   ├───i686-linux: app
│   ├───x86_64-darwin: app
│   └───x86_64-linux: app
├───defaultPackage
│   ├───aarch64-linux: package 'nix-dram-2.4pre20201205_a5d85d0'
│   ├───i686-linux: package 'nix-dram-2.4pre20201205_a5d85d0'
│   ├───x86_64-darwin: package 'nix-dram-2.4pre20201205_a5d85d0'
│   └───x86_64-linux: package 'nix-dram-2.4pre20201205_a5d85d0'
├───overlay: Nixpkgs overlay
└───packages
    ├───aarch64-linux
    │   ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    ├───i686-linux
    │   ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    ├───x86_64-darwin
    │   ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    └───x86_64-linux
        ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
        ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
        └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
```
