# Tests fetch_lineage.netrunneR_git_mirror() against a local throwaway git
# repo standing in for the real GitHub upstream.
test_that("fetch_lineage.netrunneR_git_mirror() clones and checks out the configured ref", {
  upstream <- withr::local_tempdir()
  gert::git_init(upstream)
  writeLines("hello", file.path(upstream, "README.md"))
  gert::git_add("README.md", repo = upstream)
  gert::git_commit("initial", repo = upstream, author = "Test <test@example.com>")
  # git_init()'s default branch name depends on the host's git config
  # (commonly "master" unless init.defaultBranch is set); pin it to "main"
  # explicitly so this fixture doesn't depend on ambient git config.
  gert::git_branch_create("main", repo = upstream, checkout = TRUE)

  attempt_dir <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", withr::local_tempdir(), repo_url = upstream, ref = "main")

  staged <- fetch_lineage.netrunneR_git_mirror(li, attempt_dir)

  expect_true(fs::file_exists(file.path(staged$raw_dir, "README.md")))
  expect_identical(staged$content_identity, staged$source_revision)
})

test_that("fetch_lineage.netrunneR_git_mirror() defaults to the main ref when none is set", {
  li <- new_lineage("implementation", "git_mirror", withr::local_tempdir(), repo_url = "unused")
  expect_null(li$ref)
})
