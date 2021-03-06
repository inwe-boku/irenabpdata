% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{plot_bp_electricity_generation}
\alias{plot_bp_electricity_generation}
\title{Plot electricity generation per country from BP database}
\usage{
plot_bp_electricity_generation(bp, region)
}
\arguments{
\item{bp}{tibble with bp database}

\item{region}{Region to plot data}
}
\value{
A plot of the region
}
\description{
Plot electricity generation per country from BP database
}
\examples{
download_clean_save_bp()
bp_db<-load_db(get_bp_db_files()[1])
plot_bp_electricity_generation(bp_db, "World")
}
