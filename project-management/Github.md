# Managing public (or private) projects using GitHub

We use a common set of labels across projects in GitHub. It is
possible to precisely control this set of labels using
the [settings GitHub app][github-app-settings] (needs to be enabled
for your org). Use the [`settings.yml`](../.github/settings.yml) from
this repository as a starting point.

[github-app-settings]: https://github.com/probot/settings

## Labels

We use labels to classify tasks by priority, from P0 to P4:

* **P0 (blocker)** Do not do anything else, this is a blocker. Major
  broken functionality that causes a release to be unusable. This
  includes regressions introduced in a new release that blocks
  a significant number of users, or an incompatible breaking change.
* **P1 (critical)** Must have. Critical feature request or bug fix
  which should be addressed in the next release, or a serious issue
  that impacts many users. Typically does not require immediate
  action.
* **P2 (major)** Should have. Important feature or defect that is tied
  to roadmaps or upcoming releases. Moderate live issue in a released
  version that is inconvenient for a user that needs to be addressed
  in a upcoming release.
* **P3 (minor)** Could have. Desirable enhancement or minor bug fix
  that will benefit a user. Not prioritized into roadmaps or any
  imminent release.
* **P4 (trivial)** Someday/maybe. Consider closing as "wontfix". Can
  be kept open for a potential re-prioritization if more users are
  impacted.

Labels are also useful to classify issues by type or by component. Use
colours to distinguish whether a label designates a type, a component
or something else.

## Milestones

We gate tickets with [milestones][github-milestones]. Milestones
always have an associated due date. This due date is sometimes used
for internal planning, and sometimes represent business constraints.
Use the milestone description to make the meaning clear.

[github-milestones]: https://help.github.com/articles/about-milestones/

## Project leads

Each project must have a project lead (maintainer). By convention, the
PL is whoever appears at the top
of [`CODEOWNERS` file][github-codeowners], e.g.:

```
* @mboes
```

This has the effect that @mboes will automatically be tagged as
a reviewer for all incoming PR's, unless later lines in the same file
override that.

[github-codeowners]: https://help.github.com/articles/about-codeowners/

## Change management

The default contribution process can be found in
the [`CONTRIBUTING.md` file](../CONTRIBUTING.md) of this repository.
Whatever the contribution process, this should
be [documented][github-contributing].

A corollary of the default contribution process is that all
significant changes (including bug fixes) should have an issue prior
to the pull request being opened. Also, if you have write permission
on a repository, don't fork to open pull requests.

## Releases

Every public release must include a `CHANGELOG.md` file, in the format
of [keepachangelog]. New entries to the changelog are integral parts
of bug fix and feature PR's. Always try to link entries to issue
numbers if a relevant issue exists. It's best to link issues as
follows:

```
* Frobnicated knobs. See [#NNN](https://github.com/foocorp/bar/issues/NNN).
```

Each release has an associated tag of the form `v$VERSION`, GPG-signed
by the project lead (use `git tag -s`).
See [this guide][keybase-gpg-github] for setting up an identify if you
don't already have one.

[keepachangelog]: https://keepachangelog.com/en/1.0.0/
[keybase-gpg-github]: https://github.com/pstadler/keybase-gpg-github

## Documentation

Every document has a *document owner*. The owner is usually specified
in the document itself. By default, the project lead is the owner for
all documentation in the project.

We do not use [wikis][github-wiki] for documentation. The reasons for
this are well explained [elsewhere][yesod-documentation-thoughts].
When using [GitHub pages][github-pages], we *do not* use a `gh-pages`
branch to hold documentation source code separately. The reason is
that this breaks the ability to update documentation atomically with
the associated code changes. We typically keep the documentation in
a `docs/` folder. The *source* for documentation should be kept in the
same branch than the code that it documents. This can readily be
deployed to a public site using [GitHub pages][github-pages-conf]
or [Read the Docs][readthedocs].

[github-pages]: https://pages.github.com/
[github-pages-conf]: https://help.github.com/articles/configuring-a-publishing-source-for-github-pages/
[github-wiki]: https://help.github.com/articles/about-github-wikis/
[readthedocs]: https://docs.readthedocs.io/en/latest/getting_started.html
[yesod-documentation-thoughts]: https://www.yesodweb.com/blog/2015/08/thoughts-on-documentation
