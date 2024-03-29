# irenabpdata

This R-package downloads the BP Statistical Review of World Energy and the IRENA renewable energy and capacity database and provides an interface to it. It also integrates with wbstats, a package used to read data from the World Bank Indicators Database.

## Dependencies
Dependencies are automatically installed:
```dplyr```, ```readxl```, ```stringr```, ```feather```, ```tidyr```, ```magrittr```, ```ggplot2```, ```wbstats```

## Installation
Works with package devtools (install before usage!).
<pre><code>
devtools::install_github("inwe-boku/irenabpdata")
</code></pre>

## How it works

The package downloads the respective databases from BP and Irena and saves them to feather files locally. This download has to be done only once. Afterwards, the package can load the converted files and provides functions to plot values from the database and to join them with data from the world bank.

## Example
<pre><code>
library(irenabpdata)
library(tidyverse)
library(ggplot2)
</code></pre>

Download data: 

<pre><code>
# This has to be executed only once - will save the bp file in your local directory
guess_url(2021) %>% download_clean_save_bp()
</code></pre>

Load BP database as tibble:

<pre><code>
bp <- load_latest_db_bp()
</code></pre>

Plot primary energy consumption and electricity generation for some regions:

<pre><code>
plot_bp_primary_energy_mix(bp, c("World"))

plot_bp_electricity_generation(bp, c("World"))

plot_bp_electricity_generation(bp, c("Brazil",
                                    "Germany",
                                    "United Kingdom"))

</code></pre>

Plot pipeline gas imports of Russia:

<code><pre>
bp %>% 
  filter(Country=="Russian Federation") %>% 
  filter(str_detect(Variable,"Pipeline" )) %>% 
  ggplot(aes(x=Year,y=Value))+
  geom_line(aes(col=Variable))+
  scale_color_manual(values=COLORS10)+
  theme_bw()+
  ylab("Imports/Exports (bcm)")
</code></pre>
           


Join BP Data with world bank data on GDP and plot GDP vs. Carbon Dioxide Emissions:

<pre><code>

gdp_bp <- join_wb_db("NY.GDP.MKTP.PP.KD", bp, "BP") %>% 
  na.omit()

gdp_bp %>% 
  filter(Variable_db=="Carbon Dioxide Emissions") %>% 
  filter(Country %in% c("North America",
                        "China",
                        "European Union",
                        "South Africa",
                        "Brazil",
                        "World")) %>% 
  ggplot(aes(x=Value_wb/10^12,y=Value_db/1000)) + 
  geom_point() +
  xlab("GDP (Constant 2017 international bn$)") +
  ylab("Carbon Dioxide Emissions \n(Gt CO2)") +
  facet_wrap(.~Country,scales="free") +
  theme_bw()

</pre></code>



## Acknowledgements
We gratefully acknowledge support from the European Research Council (“reFUEL” ERC2017-STG 758149).


