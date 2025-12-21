tiles_dir <- "data/flood/flood_layers_RP100/"
vrt_out   <- "data/flood/RP100_mosaic.vrt"

tifs <- list.files(tiles_dir, pattern = "depth\\.tif$", full.names = TRUE)

# Requires gdalbuildvrt available on PATH
gdalbuildvrt <- "C:/Program Files/QGIS 3.40.11/bin/gdalbuildvrt.exe"
cmd <- paste(
  shQuote(gdalbuildvrt),
  shQuote(vrt_out),
  paste(shQuote(tifs), collapse = " ")
)

system(cmd)

# Optional: check
file.exists(vrt_out)
