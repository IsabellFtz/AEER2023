# credits  ---------------------------------------------------------------------

# 1 initialization -------------------------------------------------------------
rm(list=ls(all=TRUE))         # clear environment
graphics.off()                # clear console
# set working directory 
setwd( "/Users/homefolder/AEER")
path <-"data" # set path
# install & load packages
libraries = c("dplyr", "tidyverse", "tidytable","labelled", "Hmisc", 
              "here", "devtools", "stringr") 
lapply(libraries, function(x) if (!(x %in% installed.packages())) 
{ install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# 2 load data ------------------------------------------------------------------
# house price data from RWI
# wk <- read.csv(file = file.path(path, "CampusFile_WK_cities.csv")) # WohnungsKauf
hk <- read.csv(file = file.path(path, "CampusFile_HK_cities.csv")) # HausKauf
# wm <- read.csv(file = file.path(path, "CampusFile_WM_cities.csv")) # WarmMiete
# house price data from ImmoScout24
# load("is24_students_addresses.RData")

# 3 prepare datasets -----------------------------------------------------------
## 3.1 hk dataset --------------------------------------------------------------
hkmain <- hk # create dublicate
hkmain <- hkmain %>% mutate(plz = as.double(plz)) # change plz column to double format
# Reconvert time span variables: edat and adat
hkmain$adat <- stringr::str_replace(hkmain$adat, "m(?=\\d$)", "0") # converts 2007m1 to 200701
hkmain$adat <- stringr::str_replace(hkmain$adat, "m", "") # converts 2007m10 to 200710
hkmain$edat <- stringr::str_replace(hkmain$edat, "m(?=\\d$)", "0") # converts 2007m1 to 200701
hkmain$edat <- stringr::str_replace(hkmain$edat, "m", "") # converts 2007m10 to 200710
df_in <- hkmain
hkmain<- hkmain %>% select(plz, obid, adat, edat, spell) 
# Apply first filters to hkmain: 
hkmain <- hkmain %>% 
  distinct() %>% # (1) delete identical rows 
  drop_na(plz, obid) %>% # (2) drop objects without id or plz
  filter(edat <=201912) %>% # (3) restrict time period to 200701 to 201912
  group_by(obid) %>% 
  filter(edat == max(edat), spell == max(spell), n_distinct(plz) == 1) %>% 
  # (4) drop double entries per object id according to: 
   # the last time span
   # the highest spell counter within one object id 
   # Furthermore drop all obid for which are more than pone plz are listed, e.g. obid 36682266
  ungroup() %>% left_join(df_in, by = c("plz", "obid", "adat", "edat", "spell")) # merge rest of hk variables back onto our filtered hk version, hkmain 

# Check: 
# Is every obid listed only once in hkmain? 
nrow(hkmain) == length(unique(hkmain$obid))
# Which objects are listed more than once? 
non_unique_obid <- hkmain %>%
  group_by(obid) %>%
  filter(n() > 1) 
rm(non_unique_obid)
# Is object with obid gone? 
x <- hk %>% filter(obid == 36682266) 
x <- hkmain %>% filter(obid == 36682266) 
rm(x) # yes! 

# Same data period as energy dataset (200701 - 201912)?
min(hkmain$adat)  # "200701"
max(hkmain$edat) # "201912"



