# opam-publish

A tool to ease contributions to opam repositories.

So you have followed the initial steps of [the Packaging
Guide](http://opam.ocaml.org/doc/Packaging.html) and have your package pinned
and installed through OPAM locally. Now you want to contribute to the official
or a third-party repository to make it widely available. This is where
`opam-publish` will come in handy.

## tl;dr

1. Host your package archive somewhere, eg. `http://someproject.com/someproject-1.0.tar.gz`
2. `opam-publish prepare someproject.1.0 "http://someproject.com/someproject-1.0.tar.gz"`
3. Edit `someproject.1.0/opam`, `someproject.1.0/descr`
4. `opam-publish submit someproject.1.0`
5. Follow pull-request on Github; repeat 3. and 4. if needed.

## Step 0: a hosted archive

Your package source needs to be available somewhere on the web for people to
download and install. Pack it as a `tar.gz`, `tar.bz2` or `zip` archive, and put
it on an https, http or ftp server.

If you're using a source hosting service, they probably handle serving archives
for you as well. For example, on Github, declaring a release of your software
will provide you with an address of the form
`https://github.com/<name>/<project>/archive/<tag>.tar.gz` which is fine for our
purpose.

## Step 1: `opam-publish prepare`

This command gathers all required information for a full OPAM package. With
`<url>` the url of your hosted archive, simply run:

```
opam-publish prepare <package>.<version> <url>
```

This will generate a `<package>.<version>` directory containing the full package
definition:
- A `descr` file with your package description. First line should be a short
  summary, and you can detail below. This is important, it is what people will
  see when they browse for your package.
- An `opam` file with package details and specs.
- A `url` file that was generated from the URL you provided.

On your first run, if the package didn't exist already, `descr` is just a
template and `opam` may need details. Make sure to write a clear description in
`descr`, and to provide contact information and details in the `opam` file. On
subsequent runs, or if you're updating an existing package, everything should be
filled in already.

That's it: once you're satisfied with the `descr` and `opam` files, you can go
on.

## Step 2: `opam-publish submit`

We'll suppose you want to submit to the
[official OPAM repository](https://opam.ocaml.org/packages/) on
[Github](https://github.com/ocaml/opam). If not, see the next paragraph.

Repository additions are handled through Github pull-requests, so you'll need a
Github account to track your request. You can create one quickly
[here](https://github.com/join). Opam-publish will ask for your login and
password the first time to generate an auth token, they won't be kept.

Simply run:

```
opam-publish submit <dir>
```

where `<dir>` is the directory of the form `<package>.<version>` from the step
above. This will perform a validation step on the package, just fix any errors
and try again if it doesn't pass.

You'll then be directed to the pull-request page submitted on Github. There,
you'll find the content of your pull-request, the result of the automated tests,
the discussion on this request and its status (open, merged into the repository,
or closed).

If any change is requested by the maintainers, you can simply update your files
and run `opam-publish submit` again to update the pull-request. It's also
possible to run `prepare` again e.g. to synchronize with a new archive.

## Using third-party repositories

While it will submit to the official OPAM repository by default, you can easily
add any other remote to submit to. At the moment, however, only Github is
supported as backend ; a simpler `git format-patch` and mail interface is
planned.

Use:
```
opam-publish repo add <name> <user>/<repo>
```

to add a new remote repository referring to the Github repo `<user>/<repo>`,
e.g. `ocaml/opam` for the default one. Then specify `--repo <name>` to the
`opam-publish submit` command.
