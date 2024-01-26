#
# dem <- elevatr::get_elev_raster(locations = data.frame(y=
#   40.63229590683869,x= -105.56689195350452), z = 11, prj=4326)
# plot(dem)
#
#
#
# ti <- topmodel::topidx(terra::as.matrix(dem), res = terra::xres(dem))
# a <- raster::setValues(dem, topidx$area)
# if (log) {
#   a <- log(a)
# }
# if (atb) {
#   atb <- raster::setValues(dem, topidx$atb)
#   a <- raster::addLayer(a, atb)
#   names(a) <- c("a", "atb")
# }
