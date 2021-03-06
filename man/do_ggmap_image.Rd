% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_ggmap.R
\name{do_ggmap_image}
\alias{do_ggmap_image}
\title{Launch a machine on digitalocean with ggmap installed on and store it as a snapshot}
\usage{
do_ggmap_image(name_snapshot = "analogsea-ext-ggmap-holodeck",
  name = "analogsea-ext-ggmap", sleep = 1)
}
\arguments{
\item{name_snapshot}{the name of the digitalocean snapshot to create. Defaults to 'analogsea-ext-ggmap-holodeck'.}

\item{name}{the name of the digitalocean image which will be created to take a snapshot. Defaults to 'analogsea-ext-ggmap'. Will be deleted at the end.}

\item{sleep}{a number of seconds to sleep after the droplet has been created before we ssh to it. Defaults to 1 second.}
}
\value{
the image snapshot you created
}
\description{
Launch an Ubuntu 14.04 (Trusty) machine on digitalocean with ggmap installed on and store it as a snapshot.
}
\examples{
\dontrun{
mymachine <- do_ggmap_image(sleep = 10)
mymachine

## launch a new machine from the image
anothermachine <- droplet_create(image = mymachine$id)

## clean up
droplet_delete(anothermachine)
image_delete(mymachine)
}
}

