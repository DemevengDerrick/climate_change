# ---- Config ----
base_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/CEMS-GLOFAS/flood_hazard/RP100/"
out_dir  <- "data/flood/flood_layers_RP100/"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Read directory listing HTML ----
html <- readLines(base_url, warn = FALSE)

# ---- Extract .tif links ----
# This matches href="something.tif" or href='something.tif'
tif_files <- unique(gsub('.*href=["\']([^"\']+\\.tif)["\'].*', "\\1",
                         html[grepl("\\.tif", html, ignore.case = TRUE)]))

# Defensive: keep only .tif and drop weird entries
tif_files <- tif_files[grepl("\\.tif$", tif_files, ignore.case = TRUE)]
if (length(tif_files) == 0) stop("No .tif files found. The server listing format may have changed.")

message("Found ", length(tif_files), " GeoTIFF tiles.")

# ---- Download files (skips existing) ----
for (f in tif_files) {
  url <- paste0(base_url, f)
  dest <- file.path(out_dir, f)

  if (file.exists(dest)) {
    message("[SKIP] ", f)
    next
  }

  message("[GET ] ", f)
  ok <- tryCatch({
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("[FAIL] ", f, " -> ", conditionMessage(e))
    FALSE
  })
}
message("Done. Files saved in: ", normalizePath(out_dir))
