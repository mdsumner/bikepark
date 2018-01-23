#' @importFrom magrittr %>%
#' @export
read_location <- function(path, ...) {
  na.omit(exifr::read_exif(path, ...)[c("GPSLongitude", "GPSLatitude")] %>%
    dplyr::mutate(file = path))
}
#' @export
read_loc_sf <- function(path, ...) {
sf::st_set_crs(  sf::st_as_sf(read_location(path), coords = c("GPSLongitude", "GPSLatitude")), 4326)
}
#' @export
map_loc <- function(loc, ...) {
  ## modify the basemap with
  # tasmapr::listTasmapLayers()
  # [1] "Orthophoto"   "Tasmap25k"    "Tasmap100k"   "Tasmap250k"
  # [5] "TasmapRaster" "Hillshade"    "TopoGrey"     "Topographic"
  #
  m <- mapview::mapview(loc, popup = mapview::popupImage(loc$file, height = 300, width = 200))
  m@map = m@map %>% leaflet::addWMSTiles(group="TasBase",baseUrl="http://services.thelist.tas.gov.au/arcgis/services/Basemaps/Topographic/ImageServer/WMSServer?",layers = "0") %>% mapview:::mapViewLayersControl(names = c("TasBase"))

  m
}
