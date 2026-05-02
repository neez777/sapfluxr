# R/zzz.R
# Package-level hooks.

# Reset the cached analysis configuration every time the package is loaded.
# This prevents stale options(sapfluxr.analysis_config) from hiding YAML
# changes between devtools::load_all() calls or package upgrades.
.onLoad <- function(libname, pkgname) {  # nolint: object_name_linter
  reset_analysis_config()
}
