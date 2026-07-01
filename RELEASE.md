# Releasing caretEnsemble to CRAN

Three `make` targets automate the mechanical work; the manual steps between them
are the human gates — review, the CRAN email, and the merge to `main` — that
shouldn't be automated.

## 1. Prepare — `make release`

Runs every check, then opens a GitHub *"Release caretEnsemble x.y.z"* checklist issue.

- **Blocks** (real pass/fail): `revdepcheck`, `check-many-preds`.
- **Fire-and-forget** (dispatched, not gated): `rhub` (a GitHub Actions run) and
  `check-win` (3 win-builder emails). Confirm their results before submitting.

## 2. Review & approve — *manual*

- Work the checklist issue; tick items off as you verify them.
- **Close the issue.** This is the approval gate that `make submit-cran` looks for.

## 3. Submit — `make submit-cran`

Verifies the checklist issue is closed, then opens R. In that session:

```r
devtools::submit_cran()
```

Answer its two confirmations, then **click the CRAN confirmation email**
(mandatory, CRAN-side — cannot be automated).

## 4. Wait for CRAN — *manual*

CRAN reviews over days. Watch for the acceptance email. If they request changes,
fix and resubmit (back to step 1 or 3).

## 5. After acceptance

1. **Merge the release PR into `main`** (squash) — on GitHub, or:

   ```sh
   gh pr merge <PR> --squash --delete-branch
   ```

2. Check out and pull `main`, then:

   ```sh
   make post-release
   ```

   which runs:
   - `usethis::use_github_release()` — publishes the GitHub release + git tag from `NEWS.md`.
   - `usethis::use_dev_version(push = TRUE)` — bumps to the next `.9000` dev version, commits, and pushes.

> **Order matters:** merge *before* `post-release`. A squash-merge rewrites the
> release branch into a single new commit on `main`, so the release tag must be
> created on `main` *after* the merge — not on the release branch.
