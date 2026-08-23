# Computes the single build_revision digest shared by every lineage's
# fetch/build dispatch: hashed once here so a change to shared code or
# DDL changes what every lineage's build output looks like, not just
# one lineage's.
#' Compute the build revision for a lineage
#'
#' One digest::digest sha256 over a fixed ordering of: the lineage's own
#' build module bytes, every shared build and write module, every file
#' under inst/sql/schema, the lineage's schema-version constant and the
#' renv.lock hash. Applied identically to all five lineages so a change to
#' shared code or DDL forces a rebuild everywhere it can affect output --
#' narrowing this to git-mirror-only lineages would let a shared-code or
#' DDL change silently change nrdb/abr/rules output without a new revision.
#'
#' Inputs are read from pkg_root/pkg-src when it exists: a flat mirror of
#' R/ and renv.lock that the Dockerfile copies into inst/pkg-src before
#' R CMD INSTALL, since R CMD INSTALL compiles R/*.R into a lazy-load
#' database rather than leaving the source files on disk, and strips the
#' inst/ prefix from everything under inst/, so neither the raw R/*.R
#' bytes nor a top-level renv.lock are otherwise reachable once the
#' package is installed and running inside the sync container. Outside an
#' installed package (devtools::test(), R CMD check on the source tree),
#' pkg_root is the source root itself and R/*.R plus renv.lock are already
#' there, so pkg-src is skipped in favor of pkg_root directly.
#'
#' @param lineage A lineage object.
#' @param build_module_path Character. Path to the lineage's own build
#'   module file, relative to the package root (e.g. "R/build-abr.R").
#'
#' @return Character. A sha256 hex digest.
#' @export
build_revision <- function(lineage, build_module_path) {
  pkg_root <- find_package_root()
  pkg_src_dir <- file.path(pkg_root, "pkg-src")
  src_root <- if (fs::dir_exists(pkg_src_dir)) pkg_src_dir else pkg_root

  shared_modules <- c(
    "R/sync.R", "R/promote.R", "R/build-revision.R", "R/capture.R",
    "R/validate-helpers.R", "R/validate.R", "R/ledger.R", "R/release.R",
    "R/config.R"
  )

  # Installed layout strips the inst/ prefix (sql/schema); the source tree
  # keeps it (inst/sql/schema).
  schema_dir <- if (fs::dir_exists(file.path(pkg_root, "sql", "schema"))) {
    file.path(pkg_root, "sql", "schema")
  } else {
    file.path(pkg_root, "inst", "sql", "schema")
  }
  schema_files <- if (fs::dir_exists(schema_dir)) sort(fs::dir_ls(schema_dir, glob = "*.sql")) else character(0)

  inputs <- list(
    build_module = read_bytes_or_abort(file.path(src_root, build_module_path)),
    shared_modules = lapply(file.path(src_root, shared_modules), read_bytes_or_abort),
    schema_files = lapply(schema_files, read_bytes_or_abort),
    schema_version = lineage$schema_version,
    renv_lock_hash = renv_lock_hash(src_root)
  )

  digest::digest(inputs, algo = "sha256")
}

#' Read a build_revision() input's bytes, or abort loudly if it is missing
#'
#' A missing expected input must fail the build rather than silently
#' contributing raw(0) to the hash, which would let build_revision()
#' compute over an incomplete input set without any signal that a path
#' resolution bug is silently dropping bytes from the digest.
#' @keywords internal
read_bytes_or_abort <- function(path) {
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      "build_revision() input file not found: {path}",
      class = "netrunneR_build_revision_missing_input"
    )
  }
  readBin(path, "raw", n = fs::file_size(path))
}

#' @keywords internal
renv_lock_hash <- function(src_root) {
  lock_path <- file.path(src_root, "renv.lock")
  read_bytes_or_abort(lock_path)
  digest::digest(file = lock_path, algo = "sha256")
}

#' @keywords internal
find_package_root <- function() {
  pkg_root <- system.file(package = "netrunneR")
  if (nzchar(pkg_root)) return(pkg_root)
  getwd()
}
