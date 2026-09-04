# tests/

testthat suite, edition 3.

## Files

| File | What | When to read |
| --- | --- | --- |
| `testthat.R` | Entry point invoked by `R CMD check` and `devtools::test()` | Changing suite bootstrapping |

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `testthat/` | Shared setup plus 47 test files, one per module or invariant | Adding tests, debugging a failure, checking which invariants are enforced |

## Test

```sh
make test       # devtools::test() in the rocker container
make coverage   # covr::package_coverage()
```
