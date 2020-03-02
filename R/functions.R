# Functions for irenabpdata package
# Author: Johannes Schmidt

COLORS3<-c("#c72321", "#0d8085", "#efc220")
COLORS5<-c("#c62220", "#fbd7a8", "#7a6952", "#0d8085", "#f0c220")
COLORS10<-c("#c72321","#861719","#fbd7a9","#ba9f7c","#7a6952","#6e9b9e","#0d8085","#19484c","#f0c320","#af8f19")

FILTER_IRENA<-c("Total renewable energy",
                "Bagasse",
                "Liquid biofuels",
                "Concentrated solar power",
                "Geothermal energy",
                "Solar energy",
                "Biogas",
                "Renewable hydropower (including mixed plants)",
                "Wind energy",
                "Marine energy",
                "Other solid biofuels",
                "Pure pumped storage",
                "Renewable municipal waste")


#' Download, clean and save BP World Energy Review Database to feather format
#'
#' @param url Url to latest BP World Energy Review Database. Latest known is provided as default.
#' @return Name of feather file were final cleaned database is stored
#' @examples
#' download_clean_save_bp()
download_clean_save_bp <- function(url="https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2019-all-data.xlsx") {
  ### download file, get version, save download under name with date and version
  dst_file_xlsx<-paste0("bp-",
                        Sys.Date(),
                        ".xlsx")

  dst_file_feather<-paste0("bp-",
                           Sys.Date(),
                           ".feather")


  download.file(url,
                destfile=dst_file_xlsx,
                mode="wb")


  sheets<-excel_sheets(dst_file_xlsx)

  total_tab<-NULL

  ###drop first sheet
  for(sheet in sheets[2:(length(sheets))]){

    print(sheet)

    tab<-readxl::read_excel(dst_file_xlsx,
                            sheet=sheet)

    #if cell B2 is not a number, we cannot process that sheet and skip it

    if(is.na(as.numeric(tab[2,2]))){

      if(!is.na(startsWith(unlist(tab[1,1]), "Cumulative")) & startsWith(unlist(tab[1,1]), "Cumulative"))
      {

        tab<-tab[-1,]

      }else{


        print("skipping")
        next
      }
    }

    if(sheet == "Gas - Inter-regional trade" |
       sheet == "Gas - LNG imports" |
       sheet == "Gas - LNG exports" |
       sheet == "Coal - Trade movements"){
      print("skipping")
      next
    }


    ###remove rows where first column is NA
    tab_clean<-tab[!is.na(tab[,1]),]

    ###remove rows that start with "Mine production"
    tab_clean<-tab_clean[tab_clean[,1]!="Mine production",]

    unit<-tab_clean[1, 1] %>%
      unlist()


    variable<-sheet

    names(tab_clean)<-tab_clean[1, ]
    names(tab_clean)[1]<-"Country"
    tab_clean<-tab_clean[-1,]

    ###column heading after last real data year contains "-"
    max_column<-names(tab_clean) %>%
      stringr::str_detect("-") %>%
      which() - 2

    tab_clean<-tab_clean[,1:max_column]

    tab_clean_gather<-tab_clean %>%
      tidyr::gather(Year, Value, -Country) %>%
      dplyr::mutate(Variable = variable) %>%
      dplyr::mutate(Unit = unit) %>%
      dplyr::mutate(Value = as.numeric(Value))

    ####remove footnotes
    footnote_removes<-tab_clean_gather %>%
      dplyr::group_by(Variable, Country) %>%
      dplyr::summarize(sum=sum(Value, na.rm=TRUE)) %>%
      dplyr::mutate(to_remove = sum==0) %>%
      dplyr::filter(to_remove)

    tab_clean_gather<-tab_clean_gather %>%
      dplyr::filter(!(Country %in% footnote_removes$Country))


    no_countries<-c("Canadian Oil Sands: Total",
                    "of which: Under active development",
                    "Venezuela: Orinoco Belt",
                    "Light distillates",
                    "of which: gasoline",
                    "Middle distillates",
                    "of which: diesel/gasoil",
                    "of which: jet/kerosene",
                    "Fuel oil")
    ###remove strange "Countries"
    tab_clean_gather<-tab_clean_gather %>%
      dplyr::filter(!(Country %in% no_countries))


    total_tab<-dplyr::bind_rows(total_tab,
                         tab_clean_gather)

  }




  total_tab<-total_tab %>%
    dplyr::select(Country,
                  Year,
                  Variable,
                  Unit,
                  Value
    ) %>%
    dplyr::mutate(Year=as.numeric(Year))

  total_tab_corrected<-total_tab %>%
    dplyr::mutate(Country = str_replace(Country, " &", "")) %>%
    dplyr::mutate(Country = str_replace(Country, "[1234*]", "")) %>%
    dplyr::mutate(Country = str_replace(Country, "Total ", "")) %>%
    dplyr::mutate(Country = str_replace(Country, " #", "")) %>%
    dplyr::mutate(Country = ifelse(Country == "US", "USA", Country)) %>% 
    dplyr::mutate(Country = ifelse(Country == "Russian Fed", "Russian Federation", Country)) %>% 
    dplyr::mutate(Country = ifelse(Country == "Russia", "Russian Federation", Country)) %>% 
    dplyr::mutate(Country = ifelse(Country == "West Africa", "Western Africa", Country)) %>% 
    dplyr::mutate(Country = ifelse(Country == "European Union ", "European Union", Country)) 
  
    




  ######differentiate between regions and countries


  feather::write_feather(total_tab_corrected, dst_file_feather)
  return(dst_file_feather)


}

#' Download, clean and save irena database to feather format
#'
#'
#' @param url Url to latest IRENA Database. Latest known is provided as default.
#' @return Name of feather file were final cleaned database is stored
#' #' @examples
#' download_clean_save_irena()
download_clean_save_irena<-function(url="https://www.irena.org/IRENADocuments/IRENA_RE_electricity_statistics_-_Query_tool.xlsm"){

  ### download file, get version, save download under name with date and version
  dst_file<-paste0("irena-",Sys.Date(),".xlsm")
  download.file(url,
                destfile=dst_file,
                mode="wb")

  version<-readxl::read_excel(path=dst_file,sheet=23) %>%
    tail(1) %>%
    dplyr::select(Version) %>%
    unlist()

  dst_file_version_xlsm<-paste0("irena-",Sys.Date(),"-",version,".xlsm")
  dst_file_version_feather<-paste0("irena-",Sys.Date(),"-",version,".feather")

  file.copy(dst_file,dst_file_version_xlsm)
  file.remove(dst_file)

  sheets<-readxl::excel_sheets(dst_file_version_xlsm)

  total_tab<-NULL

  ###drop first three sheets
  for(sheet in sheets[4:(length(sheets)-1)]){
    tab<-readxl::read_excel(dst_file_version_xlsm, sheet=sheet)

    ###which variable is read?
    var<-names(tab)[1]

    ###cleaning of variable name
    var<-stringr::str_replace_all(var, "\\.", "")
    var<-stringr::str_replace_all(var, "1", "")

    ###remove rows where first column is NA
    tab_clean<-tab[!is.na(tab[,1]),]

    ###remove additionally first two lines
    tab_clean<-tab_clean[c(-1,-2),]

    ###remove columns where first row is NA
    tab_clean<-tab_clean[,as.vector(!is.na(tab_clean[1,]))]

    ###split table (2 tables on one sheet!)
    table_split<-which(tab_clean[1,]=="PROD (GWh)")

    tab_clean_cap_MW<-tab_clean[,1:table_split]
    names(tab_clean_cap_MW)<-tab_clean_cap_MW[1,]
    names(tab_clean_cap_MW)[1]<-"Country"

    tab_clean_cap_MW<-tab_clean_cap_MW[-1,] %>%
      tidyr::gather(Year, Value, -Country) %>%
      dplyr::mutate(Variable=var) %>%
      dplyr::mutate(Indicator="Capacity") %>%
      dplyr::mutate(Unit="MW") %>%
      dplyr::mutate(Value=as.numeric(Value))

    tab_clean_prod_GWh<-tab_clean[,table_split:ncol(tab_clean)]
    names(tab_clean_prod_GWh)<-tab_clean_prod_GWh[1,]
    names(tab_clean_prod_GWh)[1]<-"Country"

    tab_clean_prod_GWh<-tab_clean_prod_GWh[-1,] %>%
      tidyr::gather(Year, Value, -Country) %>%
      dplyr::mutate(Variable=var) %>%
      dplyr::mutate(Indicator="Generation") %>%
      dplyr::mutate(Unit="GWh") %>%
      dplyr::mutate(Value=as.numeric(Value))

    total_tab<-dplyr::bind_rows(total_tab, tab_clean_cap_MW) %>%
      dplyr::bind_rows(tab_clean_prod_GWh)
  }


  total_tab<-total_tab %>%
    dplyr::select(Country,
                  Year,
                  Variable,
                  Indicator,
                  Unit,
                  Value
    ) %>%
    dplyr::mutate(Year=as.numeric(Year))

  total_tab_corrected<-total_tab %>%
    dplyr::mutate(Country = str_replace(Country, " &", "")) %>%
    dplyr::mutate(Country = str_replace(Country, "[1234*]", "")) %>%
    dplyr::mutate(Country = str_replace(Country, " #", "")) %>%
    dplyr::mutate(Country = ifelse(Country == "N America", "North America", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "S America", "South America", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Syrian AR", "Syria", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Czechia", "Czech Republic", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "UK", "United Kingdom", Country))  %>%
    dplyr::mutate(Country = ifelse(Country == "Iran IR", "Iran", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "C America + Carib", "Central America", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "United Arab Em", "United Arab Emirates", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Korea Rep", "South Korea", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Viet Nam", "Iran IR", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Congo Rep", "Republic of Congo", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Eq Guinea", "Equatorial Guinea", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Brunei Darsm", "Brunei", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Papua N Guin", "Papua New Guinea", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "Congo DR", "DR Congo", Country)) %>%
    dplyr::mutate(Country = ifelse(Country == "New Caledonia", "New Caledon", Country))


  feather::write_feather(total_tab_corrected, dst_file_version_feather)
  return(dst_file_version_feather)

}

#' Plot electricity generation per country from BP database
#'
#' @param bp tibble with bp database
#' @param region Region to plot data
#' @return A plot of the region
#' @examples
#' download_clean_save_bp()
#' bp_db<-load_db(get_bp_db_files()[1])
#' plot_bp_electricity_generation(bp_db, "World")
plot_bp_electricity_generation<-function(bp, region){
  generation_vars<-c("Elec Gen from Coal",
                     "Elec Gen from Gas",
                     "Elec Gen from Oil",
                     "Nuclear Generation - TWh",
                     "Renewables - TWh",
                     "Hydro Generation - TWh")
  
  mapply(get_values_of_variable,
         list(bp),
         list(region),
         as.list(generation_vars))

  p<-bp %>%
    filter(Variable %in% generation_vars) %>%
    filter(Country %in% c(region)) %>%
    filter(Year>1985) %>%
    ggplot(aes(x=Year, y=Value)) +
    geom_area(aes(fill=Variable,linetype=Country), size=2) +
    scale_fill_manual(values=COLORS10) +
    ylab("Electricity generation (TWh)")
  p
  return(p)
}


#' Returns a list of bp database files in the current directory
#'
#' @return vector with bp database files
#' @examples
#' download_clean_save_bp()
#' irena_db<-load_db(get_bp_db_files()[1])
#' get_bp_db_files()

get_bp_db_files<-function(){

  file_list<-list.files(".")
  bp_files<-file_list %>% grep("bp-.*feather", .)
  return(file_list[bp_files])


}

#' Returns a list of irena database files in the current directory
#'
#' @return vector with irena database files
#' @examples
#' download_clean_save_irena()
#' irena_db<-load_db(get_irena_db_files()[1])
#' get_irena_db_files()
get_irena_db_files<-function(){

  file_list<-list.files(".")
  irena_files<-file_list %>% grep("irena-.*feather", .)
  return(file_list[irena_files])

}

#' loads one of the database from a feather file
#'
#' @param filename_db Filename of the database to load
#' @return A tibble with the database
#' #' @examples
#' download_clean_save_irena()
#' irena_db<-load_db(get_irena_db_files()[1])
#' 
load_db<-function(filename_db){

  return(feather(filename_db) %>% as_tibble())

}

#' plots irena variables
#'
#' @param country The country
#' @param variable_filter Variables that should _not_ be shown
#' @param indicator Which indicator should be shown? Either "Generation" or "Capacity"
#' @return an image object
#' @examples
#' download_clean_save_irena()
#' irena_db<-load_db(get_irena_db_files()[1])
#' plot_irena_variables(irena_db, "World")
#'
#' irena_db<-load_db(get_irena_db_files()[1])
#' plot_irena_variables(irena_db, "World", variable_filter=FILTER_IRENA)
#'
plot_irena_variables<-function(irena_db, country, variable_filter=c(), indicator="Generation"){

  unit<-irena_db %>%
    filter(Indicator == indicator) %>%
    dplyr::select(Unit) %>%
    unique() %>% unlist()

  p<-irena_db %>%
    filter(Country == country) %>%
    filter(Indicator == indicator) %>%
    filter(!(Variable %in% variable_filter)) %>%
    ggplot(aes(x=Year, y=Value)) +
    geom_area(aes(fill=Variable)) +
    ylab(paste0(indicator, " (", unit, ")"))
  #+
  #  scale_fill_manual(values=COLORS10)
  p
  return(p)
}


#' Join bp and irena database for wind and pv capacities and generation
#'
#'
#' @param bp_db A bp database
#' @param irena_db An irena database
#' @return A join of the bp and irena database of wind and pv capacities and generation
#' @examples
#' download_clean_save_irena()
#' download_clean_save_bp()
#' irena_db<-load_db(get_irena_db_files()[1])
#' bp_db<-load_db(get_bp_db_files()[1])
#' join_bp_irena<-join_bp_irena(bp_db, irena_db)
join_bp_irena<-function(bp_db, irena_db){

  irena <- irena_db %>%
    mutate(Variable=ifelse(Variable == "Wind energy" & Indicator == "Capacity","Wind Capacity",Variable)) %>%
    mutate(Variable=ifelse(Variable == "Solar photovoltaic" & Indicator == "Capacity","Solar Capacity",Variable)) %>%
    mutate(Variable=ifelse(Variable == "Wind energy" & Indicator == "Generation","Wind Generation - TWh ",Variable)) %>%
    mutate(Variable=ifelse(Variable == "Solar photovoltaic" & Indicator == "Generation","Solar Generation - TWh",Variable)) %>%
    mutate(Value=ifelse(Indicator == "Generation",Value/1000,Value)) %>%
    mutate(Unit=ifelse(Indicator == "Generation","TWh",Unit))

  bp_irena_joined<-left_join(bp_db,
                           irena,by=c("Year"="Year","Country"="Country","Variable"="Variable")) %>%
    filter(Variable %in% c("Wind Capacity", "Solar Capacity", "Wind Generation - TWh ", "Solar Generation - TWh"))


  return(bp_irena_joined)
}

#' Plots Irena vs BP data related to wind and solar capacities and generation
#'
#' @param bp_irena_db Database that joins bp and irena data
#' @return Scatterplot of variables
#' @examples
#' download_clean_save_irena()
#' download_clean_save_bp()
#' irena_db<-load_db(get_irena_db_files()[1])
#' bp_db<-load_db(get_bp_db_files()[1])
#' join_bp_irena<-join_bp_irena(bp_db, irena_db)
#' plot_bp_vs_irena(join_bp_irena)
plot_bp_vs_irena<-function(bp_irena_db){
  p<-bp_irena_db %>%
    mutate(Variable = paste(Variable, Unit.x)) %>%
    na.omit() %>%
    ggplot(aes(x=Value.x, y=Value.y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    xlab("BP") +
    ylab("IRENA") +
    facet_wrap(.~Variable,scales = "free")
  p
  return(p)
}

#' Returns a list of regions from a database
#'
#' @param db Database from which regions should be listed
#' @return Vector with regions in the database
#' @examples
#' download_clean_save_irena()
#' irena_db<-load_db(get_irena_db_files()[1])
#' db_regions(irena_db)
db_regions<-function(db){
  return(db$Country %>% unique())

}

#' Returns a list of variables from a database
#'
#' @param db Database from which variables should be listed
#' @return Vector with regions in the database
#' @examples
#' download_clean_save_irena()
#' irena_db<-load_db(get_irena_db_files()[1])
#' db_variables(irena_db)
db_variables<-function(db){
  return(db$Variable %>% unique())

}

#' Returns values of variables from db
#'
#' @param db Database from which values should be extracted
#' @param country Country for which values should be extracted
#' @param variable Variable for which values should be extracted
#' @return Vector with values in the database. Issues a warning if the number of rows in this table are 0.
#' @examples
#' download_clean_save_bp()
#' bp_db<-load_db(get_bp_db_files()[1])
#' db_get_values_of_variables(bp_db, "Austria", "Oil - Refining capacity")
get_values_of_variable<-function(db, country, variable){
  res<-db %>% 
    filter(Country == country) %>% 
    filter(Variable == variable) 
  
  if(nrow(res) == 0){
    warning(paste0(variable, " was not found for ", country))
  }
    
  return(res)
}

#' Full join of worldbank database with either IRENA or BP database
#'
#' @param wb_db Worldbank database
#' @param db IRENA or BP Database
#' @param db_type Indicates if this is Irena or BP Database (Put "Irena" or "BP")
#' @return A tibble with full_join of the two databases
#' @examples
#' download_clean_save_bp()
#' bp_db<-load_db(get_bp_db_files()[1])
#' join_wb_db("NY.GDP.MKTP.PP.KD", bp_db, "BP")
join_wb_db<-function(indicator, db, db_type){

  wb_db<-wbstats::wb(indicator=indicator)
  
  wb_db <- wb_db %>% mutate(date = as.numeric(date)) %>% 
    full_join(country_merge_wb_bp_irena, by=c("country" = "World Bank"))
  
  db <- db %>% 
    full_join(country_merge_wb_bp_irena, by=c("Country" = db_type))
  
  full_join(wb_db, db, by=c("country" = "World Bank", "date" = "Year")) %>% 
    mutate(Country = country) %>% 
    mutate(Variable_wb = indicator) %>% 
    mutate(Variable_db = Variable) %>% 
    mutate(Year = date) %>%
    mutate(Db_type = db_type) %>% 
    dplyr::select(iso3c,
                  Country,
                  Year,
                  Variable_wb,
                  Value_wb = value,
                  Db_type, 
                  Variable_db,
                  Unit_db = Unit,
                  Value_db = Value,
                  Country_bp = BP) %>% 
    as_tibble() %>% 
    return()
}


#' Returns per capita value of either IRENA or BP database
#'
#' @param db IRENA or BP Database
#' @param db_type Indicates if this is Irena or BP Database (Put "Irena" or "BP")
#' @return A tibble with per capita values of variables in database
#' @examples
#' download_clean_save_bp()
#' bp_db<-load_db(get_bp_db_files()[1])
#' get_per_capita_values(bp_db, "BP")

get_per_capita_values<-function(db, db_type){

  j_pop_db<-join_wb_db("SP.POP.TOTL", db, db_type) 

  j_pop_db<-j_pop_db %>% mutate(PerCap = Value_db/Value_wb)
  j_pop_db %>% 
    dplyr::select(Country = Country_bp, Year, Variable = Variable_db, Unit = Unit_db, Value = PerCap) %>% 
    return()
}


