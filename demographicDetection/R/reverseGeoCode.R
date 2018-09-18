#' Get county FIPS from geocode
#'
#' This code inputs geotags and outputs county FIPS codes using the FCC Area and Census Block API.
#' @param lat Latitude [-90 90] in decimal or DMS (degrees:minutes:seconds) Examples: 38.26 or 38:15:36N
#' @param lon Longitude [-180 180] in decimal or DMS (degrees:minutes:seconds) Examples: -77.51 or 77:30:36W
#' @keywords geolocation, county, FIPS, API
#' @return fips The county FIPS code, where applicable.
#' @export
#' @examples 
#' reverseGeoCode(47.61, -122.33)
#' @seealso For more information on the FCC Area and Census Block API, see: https://geo.fcc.gov/api/census/#!/block/get_block_find


reverseGeoCode <- function(lat, lon){
  connectStr <- connectStr<-paste0("http://geo.fcc.gov/api/census/block/find?latitude=", lat, "&longitude=", lon, "&showall=TRUE&format=json")
  raw.result <- GET(connectStr)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  fips <- this.content$County[1]
  return(fips)
}
