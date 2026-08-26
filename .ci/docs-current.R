# Fails when the generated files in git do not match what roxygen2
# produces from the current R/ sources.
#
# This exists because master was once pushed with a NAMESPACE missing
# five exports and 54 man/ pages absent entirely. Every `make check` and
# `make test` run had been green, because the chain is
# `make document && make check`: document regenerated the files in the
# working tree, check then validated that repaired tree, and nothing ever
# compared it to what was actually committed. A fresh clone got a package
# whose NAMESPACE did not export functions the app calls.
#
# The comparison is therefore against git HEAD, not against roxygen's own
# output. A guard that only re-ran roxygenise() and asked "did anything
# change?" would have passed in exactly the situation that broke: after
# `make document`, the tree is self-consistent and still uncommitted.
source(".ci/restore.R")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")

# Generated outputs only. DESCRIPTION is deliberately not watched:
# roxygen2 stamps RoxygenNote there on a version change, which would fail
# this check for a container-image reason rather than a stale-docs one.
GENERATED <- c("NAMESPACE", "man")

if (!dir.exists(".git")) {
  cat("No .git here (build tarball or export); nothing to compare against.\n")
  quit(status = 0)
}

# The bind-mounted tree is owned by the host user while the container runs
# as root, which git rejects as dubious ownership unless told otherwise.
system2("git", c("config", "--global", "--add", "safe.directory", getwd()),
        stdout = NULL, stderr = NULL)

roxygen2::roxygenise()

dirty <- system2("git", c("status", "--porcelain", "--", GENERATED), stdout = TRUE)

if (length(dirty)) {
  cat("\nGenerated files are out of sync with the committed tree:\n\n")
  cat(paste0("  ", dirty, collapse = "\n"), "\n\n")
  cat("roxygen2 produced output differing from what is committed. Commit it:\n\n")
  cat("  git add NAMESPACE man && git commit -m 'Regenerate NAMESPACE and man/'\n\n")
  cat("Never hand-edit these; they are generated from the roxygen comments in R/.\n")
  quit(status = 1)
}

cat("Generated files are current.\n")
