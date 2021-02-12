# irenabpdata

This R-package downloads the IRENA renewable energy and capacity database and the BP Statistical Review of World Energy and provides an 
interface to it. It also integrates with wbstats, a package used to read data from the World Bank Indicators Database.

## Dependencies
Dependencies are automatically installed:
dplyr, readxl, stringr, feather, tidyr, magrittr, ggplot2, wbstats

## Installation
Works with package devtools (install before usage!).
<pre><code>
devtools::install_github("inwe-boku/irenabpdata")
</code></pre>

## How it works
The package is able to download data, clean it and save it to a feather file.
Different versions may be downloaded. File names are differentiated by date and possibly a file version (if the file version is changed by the provider).
If the download url changes in the future, you have to provide it to the tool.

## Usage
<pre><code>
library(irenabpdata)
library(tidyverse)
library(ggplot2)
</code></pre>

Download data: 

<pre><code>
bp_data_file<-download_clean_save_bp()
irena_data_file<-download_clean_save_irena()
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

Use BP Database to print electricity mix over time for countries:
<pre><code>
plot_bp_electricity_generation(bp_db, "World")
</code></pre>

Join BP and IRENA data for comparison and compare:
<pre><code>
bp_irena_join<-join_bp_irena(bp_db, irena_db)
plot_bp_vs_irena(bp_irena_join)
</code></pre>

Plot per capita GDP vs. Per Capita energy consumption:
<pre><code>
bp_db_per_capita<-get_per_capita_values(bp_db, "BP")

#download gdp per capita and join with bp data
j_gdp_bp_cap<-join_wb_db("NY.GDP.PCAP.PP.KD", 
                         bp_db_per_capita,
                         "BP")
						 
# plot data
j_gdp_bp_cap %>% 
  na.omit() %>%
  filter(!(Country %in% country_regions$Region)) %>%   
  filter(Variable_db =="Primary Energy Consumption") %>% 
  ggplot(aes(x=Value_wb, y=Value_db *10^6)) + 
  geom_point() +
  xlab("GDP (PPP $2011 / Capita)") +
  ylab("Primary Energy Consumption (toe/Capita)") +
  scale_color_manual(values = COLORS10)

</code></pre>




We gratefully acknowledge support from the European Research Council (“reFUEL” ERC2017-STG 758149).


