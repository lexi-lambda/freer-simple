Release Process
===============

* Check that ChangeLog (`changelog.md`) is up-to-date. Update last (top-most)
  entry in ChangeLog to contain correct version number, and release date, which
  can be generated using `LC_ALL=en_GB date +'%B %d, %Y'`. Make sure that
  version number references correct diff URL.
* Modify Cabal file (`freer-effects.cabal`) to contain `source-repository this`
  section with correct `tag:` value. Commit this to a separate branch (so
  called release branch), not `master`.
* Perform `cabal check` on a package, and resolve any issues. Those changes
  need to get to `master` branch. To do this perform interactive rebase on the
  release branch and reorder commits. Now checkout the `master` branch, and do
  `git merge --ff-only <commit>` where commit is the last commit from the
  release branch that should be on `master`. Go back to release branch.
* Make sure that Travis build is green. This will check that all changes
  previously made on the release branch are viable.
* Create a source distribution package, and publish it on Hackage as a
  candidate. Check that everything looks right.
* Tag the commit from which the source distribution package, i.e. Cabal
  package, was created. Push the tag, but not the branch on which it was
  residing, i.e. release branch, and remove the branch.
* Publish the candidate on Hackage.
* Add section named `Unreleased` into ChangeLog, and add a proper reference to
  a version diff. Push this change to the `master` branch.
