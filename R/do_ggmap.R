#' @title Launch a machine on digitalocean with ggmap installed on and store it as a snapshot
#' @description Launch an Ubuntu 14.04 (Trusty) machine on digitalocean with ggmap installed on and store it as a snapshot.
#' 
#' @param name_snapshot the name of the digitalocean snapshot to create. Defaults to 'analogsea-ext-ggmap-holodeck'.
#' @param name the name of the digitalocean image which will be created to take a snapshot. Defaults to 'analogsea-ext-ggmap'. Will be deleted at the end.
#' @param sleep a number of seconds to sleep after the droplet has been created before we ssh to it. Defaults to 1 second.
#' @return the image snapshot you created
#' @export
#' @examples 
#' \dontrun{
#' mymachine <- do_ggmap_image(sleep = 10)
#' mymachine
#' 
#' ## launch a new machine from the image
#' anothermachine <- droplet_create(image = mymachine$id)
#' 
#' ## clean up
#' droplet_delete(anothermachine)
#' image_delete(mymachine)
#' }
do_ggmap_image <- function(name_snapshot = "analogsea-ext-ggmap-holodeck", name = "analogsea-ext-ggmap", sleep = 1){
  geocodecore <- droplet_create(name, image = "ubuntu-14-04-x64")
  Sys.sleep(sleep)
  on.exit(droplet_delete(geocodecore))
  geocodecore %>% droplet_ssh("cat /etc/apt/sources.list ", verbose=TRUE)  
  geocodecore %>% droplet_ssh('echo "deb http://cran.r-project.org/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list', verbose=TRUE)
  geocodecore %>% droplet_ssh('sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9', verbose=TRUE)
  geocodecore %>% droplet_ssh("apt-get update", verbose=TRUE)
  geocodecore %>% droplet_ssh("apt-get -y install r-base r-base-dev", verbose=TRUE)
  geocodecore %>% droplet_ssh("apt-get -y install r-cran-ggplot2 r-cran-scales r-cran-reshape2 r-cran-Rcpp", verbose=TRUE)
  geocodecore %>% install_r_package("ggmap")
  geocodecore %>% droplet_power_off() %>% droplet_snapshot(name = name_snapshot) %>% action_wait()
  shot <- droplet_snapshots_list(geocodecore)[[name_snapshot]]  
  geocodecore %>% droplet_delete()  
  on.exit(NULL)
  shot
}

#' @title Geocode a dataset on a digitalocean droplet using ggmap
#' @description Geocode a dataset on a digitalocean droplet using ggmap. 
#' Note that when using Google you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}
#' 
#' @param data a data.frame with addresses to geocode. This data.frame should contains a column called address which will be used to geocode.
#' @param droplet a droplet where ggmap is installed upon. See the examples.
#' @export
#' @examples 
#' \dontrun{
#' ## Create a snapshot of a machine with ggmap on in order to have a starting image
#' ggmapimage <- do_ggmap_image(sleep = 10)
#' 
#' ## Create a machine from the snapshot in order to use it to geocode
#' mymachine <- droplet_create(image = ggmapimage$id)
#' mymachine
#' 
#' ## Geocode
#' x <- data.frame(
#'   id = c(1, 2), 
#'   address = c("Wetstraat 16, 1000 Brussel", "57 Rue de Varenne, Paris"),
#'   stringsAsFactors = FALSE)
#' latlon <- do_ggmap_geocode(data = x, droplet = mymachine)
#' str(latlon)
#' 
#' ## Clean up droplet and the snapshot
#' droplet_delete(mymachine)
#' image_delete(ggmapimage)
#' }
do_ggmap_geocode <- function(data, droplet){
  stopifnot(is.data.frame(data))
  stopifnot('address' %in% colnames(data))
  
  ## Upload dataset to the server
  filename <- tempfile()
  saveRDS(data, filename)
  droplet %>% droplet_upload(filename, "addresses.rds")
  
  ## Geocode
  droplet %>% droplet_execute({
    x <- readRDS("addresses.rds")
    out <- list()
    for(i in seq_len(nrow(x))){
      out[[i]] <- list(input = x[i, ])
      try(out[[i]]$output <- ggmap::geocode(x$address[i], output = "all", source = "google"))
    }
    saveRDS(out, "addresses_geocoded.rds")
  }, verbose = FALSE)
  
  ## Download dataset from the server
  filename <- tempfile()
  droplet %>% droplet_download("addresses_geocoded.rds", filename, overwrite = TRUE)
  result <- readRDS(filename)  
  result
}