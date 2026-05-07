library(rio)
library(survival)
library(dplyr)
library(survminer)
library(ggsurvfit)
library(jskm)
library(superheat) #heatmap package
library(sf)
library(tigris)
library(tmap)

options(datatable.na.strings=c('NULL',''));
demographics <- import("output/csv/patients.csv") %>% mutate(BIRTHDATE=as.Date(BIRTHDATE), DEATHDATE=as.Date(DEATHDATE), 
                                                             timetoevent=coalesce(DEATHDATE,max(DEATHDATE, na.rm=TRUE)) - BIRTHDATE,
                                                             timetoevent=(as.numeric(timetoevent)/365.25),
                                                             censor=!is.na(DEATHDATE), 
                                                             survival=Surv(timetoevent,event=censor))
demo_categoric_columns <- c('FIPS','ZIP');
demo_id_columns <- c('Id','SSN','DRIVERS','PASSPORT','PREFIX','FIRST','MIDDLE','LAST','SUFFIX','MAIDEN');
demo_numeric_columns <- demographics %>% select(where(is.numeric)) %>% names %>% 
  setdiff(c('survival',demo_categoric_columns,demo_id_columns));
demo_demog_columns <- names(demographics) %>% 
  setdiff(c(demo_categoric_columns,demo_id_columns,demo_numeric_columns));
demo_group_columns <- c(demo_categoric_columns,demo_demog_columns);
demo_columns <- c(demo_group_columns,demo_numeric_columns);

default_survival_group <- 'GENDER'
default_relationship_x <- demo_demog_columns[1]
default_relationship_y <- demo_numeric_columns[1]
default_relationship_color <- demo_demog_columns[2]

survivalmodel <-survfit(survival~STATE, demographics)

fit<- survfit(Surv(timetoevent,event=censor)~STATE, data=demographics)

lungfit<- surv_fit(Surv(time, status)~sex, data=survival::lung)

geodata_source_file <- "GeoDATA.R"
geodata_cache_file <- "geodata.RData"

if (!file.exists(geodata_cache_file)) {
  message(geodata_cache_file,' not found');
  source(geodata_source_file);
}
load(geodata_cache_file)