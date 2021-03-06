% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{join_bp_irena}
\alias{join_bp_irena}
\title{Join bp and irena database for wind and pv capacities and generation}
\usage{
join_bp_irena(bp_db, irena_db)
}
\arguments{
\item{bp_db}{A bp database}

\item{irena_db}{An irena database}
}
\value{
A join of the bp and irena database of wind and pv capacities and generation
}
\description{
Join bp and irena database for wind and pv capacities and generation
}
\examples{
download_clean_save_irena()
download_clean_save_bp()
irena_db<-load_db(get_irena_db_files()[1])
bp_db<-load_db(get_bp_db_files()[1])
join_bp_irena<-join_bp_irena(bp_db, irena_db)
}
