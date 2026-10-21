# dom

## CI is pinned

Every input to CI is pinned in the source (bats-lang/repository-prototype#269),
so a commit that passes keeps passing:

* `bats.lock` is committed: CI never runs `bats lock` for the package, so
  `bats check` and `bats build` fetch exactly the locked versions, and fail
  when the lock is missing or does not match `bats.toml`. A library's lock
  pins only its own CI and tests; its dependents still resolve their own.
* The compiler is the commit in `.github/bats-version`, read by every
  workflow that builds bats (and by the publish workflow).
* The package repository is fetched at the commit in
  `.github/repository-version`, so what the test packages under `tests/`
  lock (`bats lock --dev`) is pinned too.
* `publish.yml` and `relock-pins.yml` in bats-lang/repository-prototype are
  called by commit, never `@main`.

Pins move only through a reviewed pull request that runs the same CI. The
daily `relock.yml` (the shared `relock-pins.yml`) relocks against the
newest, moves the compiler and repository pins, pushes `relock/<date>`,
opens a pull request listing the old and new versions and dispatches
`check.yml` on it, so a breaking publish shows as a red relock pull
request and main stays green. GITHUB_TOKEN cannot change workflow files,
so without a `RELOCK_TOKEN` secret that pull request lists a workflow pin
that would move instead of moving it: move it in a pull request of its
own. A pull request that needs newer packages runs `bats lock
--repository <dir>` and commits `bats.lock` (and
`.github/repository-version`) with the change.

Which tool moves which pin (bats-lang/dom#93; the decision is
bats-lang/repository-prototype#367, comment 6285091040): the daily relock
moves the compiler (`.github/bats-version`), the package repository
(`.github/repository-version`) and the workflow pins of `publish.yml` and
`relock.yml`. Dependabot (`.github/dependabot.yml`, weekly, a 7-day
cooldown) moves the actions by commit with their release comments, except
the bats-lang ones, and the npm versions and lock of `tests/dynamic`: it
updates only "a dependency manifest or lock file for one of the supported
package managers" (its supported ecosystems page), so not `.nvmrc`. Node's
version in `.nvmrc` is moved by Renovate's nvm manager (its default pattern
is `.nvmrc`) once the Renovate app is installed (bats-lang/repository-prototype#369,
human only), with the `renovate.json` of dom#93, and by hand until then. The
bats setup action's commit and the runner label `ubuntu-24.04` are moved by
hand, each in a pull request of its own.
