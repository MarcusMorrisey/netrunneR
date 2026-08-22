# Lineage object construction and the built-in lineage registry.
# A lineage bundles a source_type-tagged S3 class with the static details
# (schedule, pacing, schema version, store_root) fetch/build dispatch needs.
#' Construct a lineage object
#'
#' Public extension point: a caller can register a lineage outside the
#' built-in registry with any store_root it chooses.
#'
#' @param name Character. Lowercase lineage identifier (e.g. "nrdb").
#' @param source_type Character. One of "api_poll", "git_mirror", "web_archive".
#' @param store_root Character. Filesystem path the lineage's release store lives under.
#' @param ... Additional named fields attached to the lineage object (schedule,
#'   pacing policy, schema version, etc.).
#'
#' @return An object with class `c(paste0("netrunneR_", source_type), "netrunneR_lineage")`.
#' @export
new_lineage <- function(name, source_type, store_root, ...) {
  structure(
    list(name = name, source_type = source_type, store_root = store_root, ...),
    class = c(paste0("netrunneR_", source_type), "netrunneR_lineage")
  )
}
#' The built-in lineage registry
#'
#' The single source of truth for the five built-in lineage names and
#' their static details: BUILTIN_LINEAGES and lineage() both read this
#' registry rather than each independently enumerating the same names,
#' so a name can never be known to one and not the other.
#' @keywords internal
.LINEAGE_REGISTRY <- list(
  nrdb = list(source_type = "api_poll", schedule = "daily", schema_version = 1L,
              pacing = list(min_delay_s = 1, max_delay_s = 2),
              build_module_path = "R/build-nrdb.R"),
  abr = list(source_type = "api_poll", schedule = "daily", schema_version = 1L,
             pacing = list(min_delay_s = 2, max_delay_s = 2),
             build_module_path = "R/build-abr.R"),
  cardpool = list(source_type = "git_mirror", schedule = "daily", schema_version = 1L,
                  pacing = NULL, build_module_path = "R/build-cardpool.R"),
  rules = list(source_type = "web_archive", schedule = "monthly", schema_version = 1L,
               pacing = NULL, build_module_path = "R/build-rules.R"),
  implementation = list(source_type = "git_mirror", schedule = "daily", schema_version = 1L,
                        pacing = NULL, build_module_path = "R/build-implementation.R")
)

#' The five built-in lineage names
#'
#' Derived from .LINEAGE_REGISTRY rather than typed out separately, so
#' this list and lineage()'s dispatch table can never drift apart.
#' @export
BUILTIN_LINEAGES <- names(.LINEAGE_REGISTRY)

#' Resolve one of the five built-in lineages by name
#'
#' Each entry's store_root is file.path("/data", name) with no case
#' transform on the lowercase name, so the path R opens is byte-identical
#' to the container path compose.yaml bind-mounts for that lineage.
#' (ref: DL-009)
#'
#' @param name Character. One of BUILTIN_LINEAGES.
#'
#' @return A lineage object as constructed by [new_lineage()], carrying the
#'   lineage's source_type, schedule, pacing policy, schema-version constant
#'   and build_module_path.
#' @export
lineage <- function(name) {
  if (!name %in% BUILTIN_LINEAGES) {
    rlang::abort(
      sprintf("Unknown lineage '%s'. Built-in lineages are: %s", name, paste(BUILTIN_LINEAGES, collapse = ", ")),
      class = "netrunneR_unknown_lineage"
    )
  }

  entry <- .LINEAGE_REGISTRY[[name]]
  do.call(new_lineage, c(list(
    name = name,
    source_type = entry$source_type,
    store_root = file.path("/data", name),
    schedule = entry$schedule,
    schema_version = entry$schema_version,
    pacing = entry$pacing,
    build_module_path = entry$build_module_path
  )))
}
