% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{join_wb_bp}
\alias{join_wb_bp}
\title{Full join of worldbank database with BP database}
\usage{
join_wb_bp(indicator, db)
}
\arguments{
\item{db}{BP Database}

\item{wb_db}{Worldbank database}
}
\value{
A tibble with full_join of the two databases
}
\description{
Full join of worldbank database with BP database
}
\examples{
download_clean_save_bp()
bp_db<-load_latest_db_bp()
join_wb_bp("NY.GDP.MKTP.PP.KD", bp_db)
}
