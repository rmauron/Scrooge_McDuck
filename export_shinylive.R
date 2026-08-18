# Pre-render step for analysis_4.qmd, wired via `project: pre-render:` in
# _quarto.yml. Builds the WebAssembly bundle that the iframe in that document
# embeds.
#
# Why a pre-render script and not a chunk in the .qmd: Quarto enumerates the
# paths under `project: resources:` before it renders any document, so a
# `shinylive_app/` created halfway through a render is found too late and never
# reaches _site/. The symptom is a blank white frame -- index.html gets copied
# (Quarto discovers it from the iframe src) while the 72 MB runtime beside it
# does not.

app_dir <- "shinyMapApp"
out_dir <- "shinylive_app"
index   <- file.path(out_dir, "index.html")

# Inputs live in the repo: shinyMap.R is the app, and both data files are
# committed, so this works on a fresh checkout without rendering anything first.
stopifnot(file.exists("shinyMap.R"),
          file.exists("data_wide.csv"),
          file.exists("borders.geojson"))

# `freeze: auto` does not apply to pre-render scripts -- they run on every
# render -- so guard on mtime or every render pays for a full wasm export.
# Only shinyMap.R is compared: data_wide.csv and borders.geojson are rewritten
# by chunks in analysis_4.qmd on every render, so including them here would mark
# the bundle stale every single time. Delete shinylive_app/ to force a rebuild
# after a data change.
if (file.exists(index) && file.mtime("shinyMap.R") <= file.mtime(index)) {
  message("shinylive: bundle is current, skipping export.")
} else {
  message("shinylive: exporting the wasm bundle, this is the slow step ...")

  # shinylive wants a directory holding app.R plus whatever the app reads.
  # shinyMap.R stays the single source of truth and runs unchanged under both
  # plain R and webR.
  dir.create(app_dir, showWarnings = FALSE)
  invisible(file.copy("shinyMap.R", file.path(app_dir, "app.R"), overwrite = TRUE))
  invisible(file.copy(c("data_wide.csv", "borders.geojson"), app_dir,
                      overwrite = TRUE))

  shinylive::export(app_dir, out_dir)
}

# The runtime, not just the page: index.html alone renders as a blank frame.
stopifnot(file.exists(index),
          file.exists(file.path(out_dir, "shinylive-sw.js")),
          dir.exists(file.path(out_dir, "shinylive")))
