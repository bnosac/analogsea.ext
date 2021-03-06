% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_ggmap.R
\name{do_ggmap_geocode}
\alias{do_ggmap_geocode}
\title{Geocode a dataset on a digitalocean droplet using ggmap}
\usage{
do_ggmap_geocode(data, droplet)
}
\arguments{
\item{data}{a data.frame with addresses to geocode. This data.frame should contains a column called address which will be used to geocode.}

\item{droplet}{a droplet where ggmap is installed upon. See the examples.}
}
\description{
Geocode a dataset on a digitalocean droplet using ggmap. 
Note that when using Google you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}
}
\examples{
\dontrun{
## Create a snapshot of a machine with ggmap on in order to have a starting image
ggmapimage <- do_ggmap_image(sleep = 10)

## Create a machine from the snapshot in order to use it to geocode
mymachine <- droplet_create(image = ggmapimage$id)
mymachine

## Geocode
x <- data.frame(
  id = c(1, 2), 
  address = c("Wetstraat 16, 1000 Brussel", "57 Rue de Varenne, Paris"),
  stringsAsFactors = FALSE)
latlon <- do_ggmap_geocode(data = x, droplet = mymachine)
str(latlon)

## Clean up droplet and the snapshot
droplet_delete(mymachine)
image_delete(ggmapimage)
}
}

