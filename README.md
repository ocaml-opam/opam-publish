# opam-publish

A tool to ease contributions to opam repositories.

`opam-publish` takes package definition files of your projects, and submits
them to opam package repositories
([ocaml/opam-repository](https://github.com/ocaml/opam-repository) by default).

## Prerequisites

This version is for opam 2.0 or more recent, and should not be used with repositories in
older formats. A Github account is required. opam repository is cloned over
ssh, you need to have your *ssh keys* registered in your [Github
account](https://help.github.com/en/articles/connecting-to-github-with-ssh).

## Usage

### Basic case, assuming your project is on Github

The same works for initial publications, new releases, and updates.

The following should be run from the source directory of your project

1. Make sure you have an `opam` file, or `NAME.opam` files at the root of your
   project
2. Create a tag: `git tag -a TAG; git push origin TAG`
3. Run `opam publish`. This will install the tool if required.

You will be guided through the rest (creating a Github token the first time,
reviewing the patch, etc.)

### Additional possibilities

**If your project is not on Github**, you can specify the archive URL on the
command-line.

**If the opam files are not in the archive**, or outdated, you can specify them
on the command-line, or specify a directory where to look for them (_e.g._ `opam
publish URL .`).

**Submitting to other repositories** is possible, as long as they are on Github.
See the `--repo` option.

**Updating already published packages** using opam-publish is generally **prohibited** as it usually implies a change in the target archive via the git tag, which would break everyone trying to install the package. Please refer to [opam-repository's documentation](https://github.com/ocaml/opam-repository/blob/master/CONTRIBUTING.md) for more informations about how to update or fix packages.

See `opam publish --help` for more options.
