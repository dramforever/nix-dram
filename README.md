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

The binary cache of this flake is available on Cachix. Set it up with:

```console
$ cachix use dram
```

Check out [a list of the contents of this flake](#contents-of-this-flake).

---

## What is this about?

The main reason of this fork is as follows

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

Well that's the first thing anyway. It turns out that there were several things
I really want to change about Nix, so what I did is just to dump them here. A
detailed explanation follows.

## What's new?

There are three patches over Nix in this repository:

- `nix-flake-default.patch`: This changes the Nix CLI so that it parses
  `INSTALLABLE` arguments differently. The usage of the command `nix search` was
  also changed. See below for details.

- `nix-search-meta.patch`: This is not a very user-visible change. It basically
  alters `nix search` so that the `--json` option now dumps the entire `meta`
  attribute of the package.

  This is mainly used to support `nix-search-pretty` so that it has more
  information to work with, but could support other tooling as well.

- `nix-flake-http-redirect.patch`: This arose from a thought on how we could
  keep using the good old channels in Nix Flakes. A quick thought is to just
  make Nix follow the redirect and save the final redirect target in
  `flake.lock`. This is an implementation of that idea.

These changes are *incompatible* but is predicted to minimally affect current
usage. See below for details.

The documentation has *not* yet been changed to reflect the changes.

### Changes to `INSTALLABLE`

An `INSTALLABLE` command line argument, such as that of `nix run`, now has and
additional rule when parsing:

- If an `INSTALLABLE` looks like an attribute path, e.g. `foo.bar`, it is looked
  up as if it were `flake:default#foo.bar`.

  'Looks like an attribute path' is
  defined as fully matching the regular expression
  `[a-zA-Z0-9_"-][a-zA-Z0-9_".-]`

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

One notable example is that if you want to refer to the `result` symlink from a
build output, you will need to specify `./result` or `result/`.

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

The `nix search` command used to have this syntax:

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

The following table shows a comparison of use cases, the first of which is
predicted to be the most common.

| `nix-dram` | `nixFlakes` |
|---|---|
| `nix search hello` | `nix search default hello` |
| `nix search -i github:NixOS/nixpkgs hello` | `nix search github:NixOS/nixpkgs hello` |
| `nix search --expr foo hello` | `nix search --expr foo '' hello` |
| `nix search --file foo hello` | `nix search --file foo '' hello` |

### `nix-search` and `nix-search-pretty`

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
colorized search output. `nix-search` is thin wrapper around the two. (Recorded
using [termtosvg]):

[termtosvg]: https://github.com/nbedos/termtosvg

![Demonstration of `nix-search-pretty`](images/nix-search-demo.svg)

`nix-search` is a wrapper around `nix-search-pretty` with similar usage to `nix
search`.

### Locked HTTP redirects in Flake inputs

(Refer to [a post on Discourse][http-redir] for discussion.)

[http-redir]: https://discourse.nixos.org/t/future-of-channels-and-channels-nixos-org-in-a-flakes-world/11563

How could we refer to good old channels in a Flake URL? Here's a possible way
that I thought of:

1. When a user specifies an `http`/`https` URL, and it leads to (possibly
   several) redirects, we instead record the *final redirection destination* in
   `flake.lock`.
2. When a `flake.lock` is consulted to download the tarball, the URL in
   `flake.lock` is used.

This way specifying:

```nix
inputs.nixpkgs.url = "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz";
```

Would actually just work, as instead of failing with an invalid hash whenever
`nixos-unstable` updates, it saves the redirected URL which points to a stable
version. `flake.nix` would look something like this:

```jsonc
{
  "nodes": {
    "nixpkgs": {
      "locked": {
        "narHash": "sha256-N1qI50AkeTSBp1ffUCHrcK2re52wrn6euFFGGvFa2iw=",
        "type": "tarball",
        "url": "https://releases.nixos.org/nixos/unstable/nixos-21.05pre269929.ff96a0fa563/nixexprs.tar.xz"
      },
      "original": {
        "type": "tarball",
        "url": "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz"
      }
    },
    // ...
}
```

This way, older versions of Nix seeing this new lock file would just behave as
if someone used `--override-input`, and newer versions of Nix seeing the old
lock file with the pre-redirection URL would simply migrate it over when `nix
flake update --update-input` is used.

A major concern would be whether this redirection is actually part of the
intended interface of channels.nixos.org.

## More on the design

In the usual version of `nixFlakes`, it is in fact a conscious design choice
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
approach. Will this be how we use Flakes in the future? We will have to wait and
see.

[nixflk]: https://github.com/nrdxp/nixflk

This project is called `nix-dram` with the specific intention that it will
*never* be official. This is just one person, me, trying out one point in the
design space. That's how free software works, isn't it?

## Why is this not a script or a few shell functions?

(Reference: <https://www.reddit.com/r/NixOS/comments/lbqsfg/_/glvs8zw?context=1>)

### `INSTALLABLE`

`nix-dram` handles everything where an `INSTALLABLE` is expected, so other
commands will also work, like `nix run`, `nix eval`, `nix develop`, `nix edit`.
(Yes I do use these all the time.) Even more obscure ones like and `nix copy`,
`nix bundle` also work.

A wrapper around `nix` would mean more subcommands I'd feel comfortable
implementing, honestly. On the other hand, if you do the C++ work, to modify
`INSTALLABLE` handling, there's just one function you need to touch:

```diff
@@ -20,6 +20,10 @@

 namespace nix {

+const static std::regex attrPathRegex(
+    "(?:[a-zA-Z0-9-_][a-zA-Z0-9-._]*)",
+    std::regex::ECMAScript);
+
@@ -626,7 +642,13 @@ std::vector<std::shared_ptr<Installable>> SourceExprCommand::parseInstallables(
-                auto [flakeRef, fragment] = parseFlakeRefWithFragment(s, absPath("."));
+                bool isAttrPath = std::regex_match(s, attrPathRegex);
+
+                auto [flakeRef, fragment] =
+                    isAttrPath
+                    ? std::make_pair(parseFlakeRef("flake:default", absPath(".")), s)
+                    : parseFlakeRefWithFragment(s, absPath("."));
+
```

Another thing is compatibility with the old syntax. In the case of a wrapper, it
could be possible to be allow for both 'implicit flake' and 'explicit flake'
`INSTALLABLE` to work together as described in ['Changes to
`INSTALLABLE`'](#changes-to-installable), but it is going to be much more
handling.

A script would need to look at *each* `INSTALLABLE` argument and translate those
that need translating. Figuring out which arguments are `INSTALLABLE` is
actually the hardest part. A wrapper would need to either understand all the
options or require something like a `--` marker, otherwise it could accidentally
change `--override-flake foo bar` into `--override-flake flake:default#foo
flake:default#bar`.

### Command line completion

`nix-dram` also handles command line completion, so if you type `nix search
wires` and press tab, you get `wireshark`. Not having to type `nixpkgs#wires`
makes it much smoother. Conceivably you can write your completion handlers as
well, but that's honestly way too much for me.

That's because `nix` the program itself handles command line completion. If you
write a wrapper, you need to to translate completion requests/responses (Yes,
responses as well if you don't want `wires` completing to
`flake:default#wireshark`). You may even need to call `nix` *twice* to generate
completion for both the registry and attributes. It seems *much* more work than
just patching whatever generates the completion. And the problem of handling
options also occurs here.

### Are you sure this is the best way?

I like how much I get from the moderate amount of patching I did. The
main drawback I find is that I need to spend quite a bit of time building
`nix-dram`. I honestly think it's worth it.

### But what if...

If you think `nix-dram` is too much, you can write your own little scripts.
`nix-dram` might just not be what you need.

If you like specifying the flake all the time, then by all means just use the
usual Nix Flakes CLI.

If you have any idea on how what `nix-dram` does could be done in a more
lightweight way, I'm happy to take suggestions.

---

## Contents of this flake

```console
$ nix flake show github:dramforever/nix-dram
github:dramforever/nix-dram/[...]
├───apps
│   ├───aarch64-linux
│   │   ├───nix-dram: app
│   │   └───nix-dram-progress: app
│   ├───i686-linux
│   │   ├───nix-dram: app
│   │   └───nix-dram-progress: app
│   ├───x86_64-darwin
│   │   ├───nix-dram: app
│   │   └───nix-dram-progress: app
│   └───x86_64-linux
│       ├───nix-dram: app
│       └───nix-dram-progress: app
├───checks
│   ├───aarch64-linux
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-dram-progress: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   ├───nix-search: derivation 'nix-search'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   ├───i686-linux
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-dram-progress: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   ├───nix-search: derivation 'nix-search'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   ├───x86_64-darwin
│   │   ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-dram-progress: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│   │   ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│   │   ├───nix-search: derivation 'nix-search'
│   │   └───nix-search-pretty: derivation 'nix-search-pretty-0.1.0.0'
│   └───x86_64-linux
│       ├───nix-dram: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│       ├───nix-dram-progress: derivation 'nix-dram-2.4pre20201205_a5d85d0'
│       ├───nix-nar-listing: derivation 'nix-nar-listing-0.1.0.0'
│       ├───nix-search: derivation 'nix-search'
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
    │   ├───nix-dram-progress: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   ├───nix-search: package 'nix-search'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    ├───i686-linux
    │   ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-dram-progress: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   ├───nix-search: package 'nix-search'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    ├───x86_64-darwin
    │   ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-dram-progress: package 'nix-dram-2.4pre20201205_a5d85d0'
    │   ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
    │   ├───nix-search: package 'nix-search'
    │   └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
    └───x86_64-linux
        ├───nix-dram: package 'nix-dram-2.4pre20201205_a5d85d0'
        ├───nix-dram-progress: package 'nix-dram-2.4pre20201205_a5d85d0'
        ├───nix-nar-listing: package 'nix-nar-listing-0.1.0.0'
        ├───nix-search: package 'nix-search'
        └───nix-search-pretty: package 'nix-search-pretty-0.1.0.0'
```
