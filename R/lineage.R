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
#' Environment variable that overrides the store base path
#'
#' Named without a netrunneR prefix to match the bare NRDB_CONTACT /
#' LINEAGE / MODE convention the rest of the package already reads.
#' @keywords internal
STORE_BASE_ENV <- "NETRUNNER_STORE_BASE"

#' The base directory every lineage's store_root sits under
#'
#' Defaults to "/data", unchanged and byte-identical to what compose.yaml
#' bind-mounts (ref: DL-009) -- so a container that sets nothing behaves
#' exactly as before this seam existed.
#'
#' The override exists because store_root was otherwise unreachable: a
#' test could construct its own lineage via new_lineage(), but anything
#' going through lineage() -- which is everything the Shiny app touches,
#' via resolve_active_release() -- was pinned to /data and could not be
#' pointed at a temporary directory. That is what kept
#' tests/testthat/test-integration-app.R stubbed.
#'
#' Intended for tests and local development. A deployment should leave it
#' unset and rely on the bind mount.
#' @return Character. The base path, with no trailing separator.
#' @keywords internal
store_base <- function() {
  base <- Sys.getenv(STORE_BASE_ENV, unset = "")
  if (!nzchar(base)) return("/data")
  # A trailing separator would make file.path() produce "base//cardpool",
  # which resolves the same but is not byte-identical to the path the
  # ledger and the resolved-store_root log line report.
  sub("[/\\]+$", "", base)
}

#' The built-in lineage registry
#'
#' The single source of truth for the six built-in lineage names and
#' their static details: BUILTIN_LINEAGES and lineage() both read this
#' registry rather than each independently enumerating the same names,
#' so a name can never be known to one and not the other.
#' @keywords internal
.LINEAGE_REGISTRY <- list(
  nrdb = list(source_type = "api_poll", schedule = "daily", schema_version = 1L,
              pacing = list(min_delay_s = 1, max_delay_s = 2),
              build_module_path = "R/build-nrdb.R",
              base_url = "https://netrunnerdb.com/api/2.0/public"),
  abr = list(source_type = "api_poll", schedule = "daily", schema_version = 1L,
             pacing = list(min_delay_s = 2, max_delay_s = 2),
             build_module_path = "R/build-abr.R",
             base_url = "https://alwaysberunning.net/api"),
  cardpool = list(source_type = "git_mirror", schedule = "daily", schema_version = 2L,
                  pacing = NULL, build_module_path = "R/build-cardpool.R",
                  repo_url = "https://github.com/Null-Signal-Games/netrunner-cards-json.git"),
  rules = list(source_type = "web_archive", schedule = "monthly", schema_version = 1L,
               pacing = NULL, build_module_path = "R/build-rules.R",
               hub_url = "https://nullsignal.games/rules/comp-rules/"),
  implementation = list(source_type = "git_mirror", schedule = "daily", schema_version = 2L,
                        pacing = NULL, build_module_path = "R/build-implementation.R",
                        repo_url = "https://github.com/mtgred/netrunner.git", ref = "master"),
  # Cobra (tournaments.nullsignal.games) is NSG's own official tournament
  # platform, also community-run -- same conservative pacing abr uses,
  # since fetch_cobra() (R/fetch-cobra.R) issues many small per-tournament
  # requests across its discovery/tail-probe/backfill-walk crawl, not one
  # paginated call the way abr's tournament list is.
  cobra = list(source_type = "api_poll", schedule = "daily", schema_version = 1L,
               pacing = list(min_delay_s = 2, max_delay_s = 2),
               build_module_path = "R/build-cobra.R",
               base_url = "https://tournaments.nullsignal.games")
)

#' The six built-in lineage names
#'
#' Derived from .LINEAGE_REGISTRY rather than typed out separately, so
#' this list and lineage()'s dispatch table can never drift apart.
#' @export
BUILTIN_LINEAGES <- names(.LINEAGE_REGISTRY)

#' Resolve one of the six built-in lineages by name
#'
#' Each entry's store_root is file.path(store_base(), name) with no case
#' transform on the lowercase name, so the path R opens is byte-identical
#' to the container path compose.yaml bind-mounts for that lineage.
#' (ref: DL-009) store_base() is "/data" unless NETRUNNER_STORE_BASE is
#' set, which only tests and local development should do.
#'
#' @param name Character. One of BUILTIN_LINEAGES.
#'
#' @return A lineage object as constructed by [new_lineage()], carrying the
#'   lineage's source_type, schedule, pacing policy, schema-version constant,
#'   build_module_path, and any source-specific fields (base_url for
#'   api_poll lineages, repo_url/ref for git_mirror lineages, hub_url for
#'   web_archive lineages).
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
    store_root = file.path(store_base(), name),
    schedule = entry$schedule,
    schema_version = entry$schema_version,
    pacing = entry$pacing,
    build_module_path = entry$build_module_path,
    base_url = entry$base_url,
    repo_url = entry$repo_url,
    ref = entry$ref,
    hub_url = entry$hub_url
  )))
}
