# .ci/

Rscript entry points invoked by the Makefile's Docker targets. Excluded from the build tarball via `.Rbuildignore`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `restore.R` | Bootstraps renv from GitHub, restores `renv.lock` against a p3m.dev repo override | Debugging dependency restore, package-version 404s, or renv bootstrap failures |
| `check.R` | Creates the ephemeral `inst/pkg-src` mirror, then runs `rcmdcheck` with `error_on = "warning"` | Debugging `make check`, or changing what `R CMD check` treats as fatal |
| `test.R` | Restores dependencies, then runs `devtools::test()` | Debugging `make test` |
| `document.R` | Restores dependencies, then runs `roxygen2::roxygenise()` | Debugging `make document` or regenerated `man/`/`NAMESPACE` output |
| `coverage.R` | Restores dependencies, then runs `covr::package_coverage()` | Debugging `make coverage` |

## Run

Each script is invoked inside the container by its matching Makefile target; run them from the repository root only.

```sh
make check
make test
make document
make coverage
```
