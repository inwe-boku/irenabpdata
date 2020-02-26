# irenabpdata

This R-package downloads the IRENA renewable energy and capacity database and the BP Statistical Review of World Energy and provides an interface to it.

## Dependencies
Dependencies are automatically installed:
dplyr, readxl, stringr, feather, tidyr, magrittr

## Installation
Works with package devtools (install before usage!). Works on nora in RStudio. Should work without problems locally.
<pre><code>
devtools::install_github("inwe-boku/irenabpdata")
</code></pre>

## How it works
The package is able to download data, clean it and save it to a feather file.
Different versions may be downloaded. File names are differentiated by date and possibly a file version (if the file version is changed by the provider).
If the download url changes in the future, you have to provide it to the tool.

## Usage
library(irenabpdata)

Download data: 

<pre><code>
bp_data_file<-irenabpdata::download_clean_save_bp()
irena_data_file<-irenabpdata::download_clean_save_irena()
</code></pre>

Load database as tibble:

<pre><code>
bp_db<-load_db(bp_data_file)
irena_db<-load_db(irena_data_file)
</code></pre>

Show database:
<pre><code>
view(bp_db)
view(irena_db)
</pre></code>

Print all variables and regions in database:

<pre><code>
db_regions(bp_db)
db_variables(bp_db)
db_regions(irena_db)
db_variables(irena_db)
</code></pre>

Join BP and IRENA data for comparison and compare:
<pre><code>
bp_irena_join<-join_bp_irena(bp_db, irena_db)
plot_bp_vs_irena(bp_irena_join)
</code></pre>






