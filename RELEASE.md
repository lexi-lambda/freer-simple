Release Process
===============

* Check that all deprecations, and remove those deprecated definitions
  scheduled for removal.
* Check that ChangeLog (`changelog.md`) is up-to-date. Update last (top-most)
  entry in ChangeLog to contain correct version number, and release date, which
  can be generated using `LC_ALL=en_GB date +'%B %d, %Y'`. Make sure that
  version number references correct diff URL.
* Check that Cabal file (`freer-effects.cabal`) is up-to-date, and regenerate
  it from `package.yaml` if its not.
* Modify Cabal file (`freer-effects.cabal`) to contain `source-repository this`
  section with correct `tag:` value. Commit this to a separate branch (so
  called release branch), not `master`.
* Perform `cabal check` on a package, and resolve any issues. Those changes
  need to get to `master` branch eventually.
* Make sure that Travis build is green. This will check that all changes
  previously made on the release branch are viable.
* Create a source distribution package, and publish it on Hackage as a
  candidate. Check that everything looks right.
* Tag the commit from which the source distribution package, i.e. Cabal
  package, was created. Push the tag.
* Merge the release branch back to `master` so that the tags and commits are
  reachable, regenerate `freer-effects.cabal` from `package.yaml` to get rid
  of `source-repository this` and squash/amend this change into the merge
  commit (so that the spine of `master` never contains `source-repository
  this`). Don't push yet.
* Publish the candidate on Hackage.
* Add section named `Unreleased` into ChangeLog, and add a proper reference to
  a version diff. Push this change to the `master` branch.
