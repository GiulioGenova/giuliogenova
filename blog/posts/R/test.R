library(terra)
mydf <- data.frame(x = 10, y = 10, value = 42)

rast_singlecoord_safe <- function(
    x, resolution = NULL, type = "xyz", crs = "",
    digits = 6, extent = NULL) {
    if ((length(unique(x$x)) == 1) | (length(unique(x$y)) == 1)) {
        if ( is.null(resolution)) {
            stop("provide the resolution of the raster")
        }
        additional_coords <- data.frame(
            x = min(unique(x$x), na.rm = TRUE) + resolution,
            y = min(unique(x$y), na.rm = TRUE) + resolution
        )
        additional_coords[setdiff(names(x), names(additional_coords))] <- NA
        x <- rbind(x, additional_coords)
        rst <- terra::rast(x,
            type = type, crs = crs, digits = digits, extent=extent
        )
        rst <- terra::trim(rst)
    } else {
        rst <- terra::rast(x,
            type = type, crs = crs, digits = digits, extent=extent
        )
    }
    return(rst)
}

plot(rast_singlecoord_safe(mydf,resolution = 100))
