library(dplyr)
library(tidyverse)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)


######## code ########

## COVID-19 DATA FOR MSOAS
#Reading in COVID-19 London cases/rates data
covid_msoa <- read_csv("data/raw/msoa_E12000007_2020-12-13.csv")

#Filtering to make sure all area codes are E02
covid_msoa <- covid_msoa %>%
  filter(str_detect(`areaCode`, "E02"))%>%
  dplyr::select(c(areaCode, areaName, date, newCasesBySpecimenDateRollingSum)) #Fields we want

#Converting date field to date specification
covid_msoa$date <- as.Date(covid_msoa$date,"%d/%m/%y")

#Removing data from weeks in the first wave
#The beginning of the second wave of COVID-19 in the UK is defined from...
#...the week commencing 22 September 2020 where new restrictions were put in...
#...place and following Johnson's announcement of a 2nd wave on 18 September

covid_msoa <- covid_msoa%>%
  subset(., date >= "2020-09-22")

#Making na values 0 so sum function works
covid_msoa <- covid_msoa %>%
  mutate(newCasesBySpecimenDateRollingSum = if_else(is.na(newCasesBySpecimenDateRollingSum), 0, newCasesBySpecimenDateRollingSum))

#Creating new df with aggregated total of covid-19 cases in the second wave
covid_msoa_totals <- covid_msoa%>% 
  dplyr::group_by(areaCode) %>% 
  dplyr::summarise(newCasesBySpecimenDateRollingSum = sum(newCasesBySpecimenDateRollingSum))%>%
  rename(total_cases = newCasesBySpecimenDateRollingSum,
         area_code = areaCode)

#Descriptive stats for covid totals
summary(covid_msoa_totals)


## ADDITIONAL MSOA DATA - to use as confounding variables
#Reading in additional MSOA ethnicity, household, age structure etc. data
#Data derived from 2011 census
additional <- read_csv("data/raw/msoa-data.csv")

#984 obs instead of 983
#Filtering by london msoa area code "E02" to see if anomaly is removed
#Anomaly removed
additional <- additional%>%
  filter(str_detect(`Middle Super Output Area`, "E02"))

#Clean field names using janitor
additional <- clean_names(additional)

#Get index numbers for each column  
colnames(additional)

#Keeping the following fields
#Ethnic group, household composition %, age structure, tenure, dwelling type...
#... land area, med house price, qualification, economic activity, household income...
#... income deprivation, health, obesity %, life expectancy
additional <- additional%>%
  dplyr::select(c(1:9, 43, 49:53, 60:65, 96:99, 106:111, 112, 120, 132:139, 143:145, 148:151, 159:161, 167:171, 176, 182:183))

colnames(additional)

#Converting age structure into ratios
additional <- replace(additional, 4, additional$age_structure_2011_census_0_15 / additional$age_structure_2011_census_all_ages * 100)
additional <- replace(additional, 5, additional$age_structure_2011_census_16_29 / additional$age_structure_2011_census_all_ages * 100)
additional <- replace(additional, 6, additional$age_structure_2011_census_30_44 / additional$age_structure_2011_census_all_ages * 100)
additional <- replace(additional, 7, additional$age_structure_2011_census_45_64 / additional$age_structure_2011_census_all_ages * 100)
additional <- replace(additional, 8, additional$age_structure_2011_census_65 / additional$age_structure_2011_census_all_ages * 100)

##Converting level 4 or above qualification field to ratio of total pop
additional <- replace(additional, 39, additional$qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above / additional$age_structure_2011_census_all_ages * 100)

#Removing additional fields
additional <- additional%>%
  dplyr::select(-c(9, 12, 15, 23, 25, 26, 27, 33, 34:38, 40:41, 43, 46:48))

#Renaming fields - .shp files have 10 character limits 
#The less intuitive ones are listed below
#pct_hh_wc = percentge of couple households with dependent child
#pct_lp_hh = percentage of lone parent household
#pct_1p_hold = percentage of 1 person household
#pct_mm = percentaged of mixed multiple ethnic groups
#pct_ownout = percentage of owned outright tenure
#pct_srent = percentage of social rented tenure
#pct_ql4_ab = percentage of level 4 qualifications and above
#pct_eco_a = percentage of economically active
#pct_eco_ia =percentage of economically inactive
#mn_ahh_in = mean annual household income (£)
#pct_limlot = percentage of day to day activities limited a lot
#pct_limltt = percentage of day to day activities limited a little
#pct_limnot = percentage of day to day activities not limited
#pct_16p_ob = percentage of 16 plus population obese (determined by bmi above 30)

additional <- additional%>%
  rename(pop = age_structure_2011_census_all_ages,
         pct_0_15 = age_structure_2011_census_0_15,
         pct_16_29 = age_structure_2011_census_16_29,
         pct_30_44 = age_structure_2011_census_30_44,
         pct_45_64 = age_structure_2011_census_45_64,
         pct_65 = age_structure_2011_census_65,
         hholds_tot = households_2011_all_households,
         pct_hh_wdc = household_composition_2011_percentages_couple_household_with_dependent_children,
         pct_lp_hh = household_composition_2011_percentages_lone_parent_household,
         pct_1p_hh = household_composition_2011_percentages_one_person_household,
         pct_white = ethnic_group_2011_census_white_percent,
         pct_mm = ethnic_group_2011_census_mixed_multiple_ethnic_groups_percent,
         pct_asian = ethnic_group_2011_census_asian_asian_british_percent,
         pct_black = ethnic_group_2011_census_black_african_caribbean_black_british_percent,
         pct_other = ethnic_group_2011_census_other_ethnic_group_percent,
         pct_bame = ethnic_group_2011_census_bame_percent,
         pct_ownout = tenure_2011_owned_owned_outright_percent,
         pct_srent = tenure_2011_social_rented_percent,
         pct_dtchd = dwelling_type_2011_whole_house_or_bungalow_detached_percent,
         pct_semi = dwelling_type_2011_whole_house_or_bungalow_semi_detached_percent,
         pct_trrced = dwelling_type_2011_whole_house_or_bungalow_terraced_including_end_terrace_percent,
         pct_flat = dwelling_type_2011_flat_maisonette_or_apartment_percent,
         land_ha = land_area_hectares,
         pct_ql4_ab = qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above,
         pct_eco_a = economic_activity_2011_census_economically_active_percent,
         pct_eco_ia = economic_activity_2011_census_economically_inactive_percent,
         mn_ahh_in = household_income_estimates_2011_12_total_mean_annual_household_income,
         pct_limlot = health_2011_census_day_to_day_activities_limited_a_lot_percent,
         pct_limltt = health_2011_census_day_to_day_activities_limited_a_little_percent,
         pct_notlim = health_2011_census_day_to_day_activities_not_limited_percent,
         pct_h_vg = health_2011_census_very_good_health_percent,
         pct_h_g = health_2011_census_good_health_percent,
         pct_h_f = health_2011_census_fair_health_percent,
         pct_h_b = health_2011_census_bad_health_percent,
         pct_h_vb = health_2011_census_very_bad_health_percent,
         pct_16p_ob = obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
         le_male = life_expectancy_males,
         le_female = life_expectancy_females)


## MSOA BOUNDARY DATA
#Reading in London MSOA boundary shapefile
msoa_boundaries <- st_read("data/raw/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp")%>%
  st_transform(., 27700)

#Exploring fields
colnames(msoa_boundaries)

#Selecting fields 
#Keeping pop density and avg hhold size
msoa_boundaries <- msoa_boundaries%>%
  dplyr::select(c(1, 2, 10, 12, 13))

#Join COVID cases to msoa_boundaries
msoa_boundaries <- merge(msoa_boundaries, covid_msoa_totals, 
                         by.x=1,
                         by.y=1)

#Join additional msoa data to msoa boundaries
#Some cleaning
msoas <- merge(msoa_boundaries, additional,
               by.x=1,
               by.y=1)%>%
  mutate(area_m2 = st_area(.,),
         area_ha = area_m2 / 10000)%>% #Creates msoa land areas
  dplyr::select(-c("msoa_name"))%>% #drops the duplicate msoa name field
  rename(name = MSOA11NM,
         pop_den = POPDEN,
         av_hhsz = AVHHOLDSZ)


## OS GREENSPACES and ACCESS POINTS
#OS greenspaces and access polygons & points
green_spaces <- st_read("data/raw/OS Open Greenspace (ESRI Shape File) TQ/data/TQ_GreenspaceSite.shp")%>%
  st_transform(., 27700)

access_points <- st_read("data/raw/OS Open Greenspace (ESRI Shape File) TQ/data/TQ_AccessPoint.shp")%>%
  st_transform(., 27700)

#Clipping greenspaces and access points to London msoa layer
#This doesn't completely work, when plotted you can see access points and gs...
#...Falling outside the London MSOA bounding box however when the final... 
#...intersections are completed this resolves the issue
green_spaces <- green_spaces[msoa_boundaries,]
access_points <- access_points[msoa_boundaries,]

#Removing several fields from green_spaces
#Then cleaning field names
green_spaces <- green_spaces%>%
  dplyr::select(-c(4:6))%>%
  rename(function_class = function.) #janitor will rename to function which will cause problems

green_spaces <-clean_names(green_spaces)

#Having a look at the different greenspace 'functions'
#There are 10 classifications
#Play space has the highest count
green_spaces %>%
  st_drop_geometry() %>%
  count(function_class, sort = TRUE)

#Creating new fields with calculated area of each greenspace in m2 and ha
#Can then subset by the London Plan's greenspace size and proximity policy (they used hectares)
#Creating new fields for gs area in m and ha
green_spaces <- green_spaces%>%
  mutate(area_m2 = st_area(.),
         area_ha = area_m2/10000)

#Looking at the distribution of green space sizes in ha
summary(green_spaces$area_ha)

# The London Plan sizes of green spaces (in ha) and proximity:
# Linear Open Spaces - variable - wherever feasible
# Pocket Parks - under 0.4 - < 400m
# Small Open Spaces - under 2 - < 400m
# Local Parks and Open Spaces - 2 - 400m
# District Parks - 20 - 1200m
# Metropolitan Parks - 60 - 3200m
# Regional Parks - 400 - 3200-8000m

#Need to convert the area columns from units object to numeric
#Can then subset the greenspaces by ha size
green_spaces$area_m2 <- as.numeric(green_spaces$area_m2)
green_spaces$area_ha <- as.numeric(green_spaces$area_ha)

#less 0.4ha
gs_less_0.4 <- green_spaces%>% #400m proximity policy
  filter(area_ha<=0.4)

#less than or equal to 2ha & more than 0.4
gs_less_2 <- green_spaces%>% #400m proximity policy
  filter(area_ha<=2.0 & area_ha>0.4)

#less 20ha & more than 2
gs_less_20 <- green_spaces%>% #min 400m - 1200m proximity policy
  filter(area_ha<=20 & area_ha>2.0)

#less 60ha & more than 20
gs_less_60 <- green_spaces%>% #1200m - 3200m proximity policy
  filter(area_ha<=60 & area_ha>20)

#Less 400ha & more than 60 
gs_less_400 <- green_spaces%>% #3200m - 8000m proximity policy
  filter(area_ha<=400 & area_ha>60)

#More than 400ha
gs_more_400 <- green_spaces%>% #3200m+ proximity policy
  filter(area_ha>400)

#Also going to make a subset for all greenspaces >2ha
#May be useful for point density analysis using access points
gs_more_2 <- green_spaces%>% #min 400m proximity policy
  filter(area_ha>2)

#Plot of greenspaces atleast 2ha (min 400m distance proximity for > 2ha)
#RICS definition of local green spaces found here:
#https://www.rics.org/uk/wbef/megatrends/urbanisation/the-role-of-green-space-in-londons-covid-19-recovery/
tm_shape(msoa_boundaries) +
  tm_polygons(col = "white", alpha = 0.2) +
  tm_shape(gs_more_2)+
  tm_polygons(col= "green")


#Now we need to subset the access points to match the greenspace area ha subsets
#Need to find the greenspace ID and then match to the GSiteRef in the access points
#We'll be using the access points for creating the isochrones
access_points <- access_points%>%
  rename(aid = id) #avoids confusion

#Subset access points for each greenspace ha subsets
ap_for_0.4 <- gs_less_0.4%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_0.4 <- merge(access_points, ap_for_0.4, 
                    by.x=3,
                    by.y=1)

ap_for_2 <- gs_less_2%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_2 <- merge(access_points, ap_for_2, 
                  by.x=3,
                  by.y=1)

ap_for_20 <- gs_less_20%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_20 <- merge(access_points, ap_for_20, 
                   by.x=3,
                   by.y=1)

ap_for_60 <- gs_less_60%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_60 <- merge(access_points, ap_for_60, 
                   by.x=3,
                   by.y=1)

ap_for_400 <- gs_less_400%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_400 <- merge(access_points, ap_for_400, 
                    by.x=3,
                    by.y=1)

ap_for_more_400 <- gs_more_400%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_for_more_400 <- merge(access_points, ap_for_more_400, 
                         by.x=3,
                         by.y=1)

#This is for all access points associated with greenspaces >2ha
ap_more_2_all <- gs_more_2%>%
  dplyr::select(1)%>%
  st_drop_geometry()

ap_more_2_all <- merge(access_points, ap_more_2_all,
                       by.x=3,
                       by.y=1)


#Point density visualisation
#create new sf with X and Y variables for all access points
#create new sf for all access points for greenspaces >2ha
access_points_pd <- access_points%>%
  st_transform(27700)%>%
  cbind(st_coordinates(.))

ap_more_2_pd <- ap_more_2_all%>%
  st_transform(27700)%>%
  cbind(st_coordinates(.))

# plot the density visualisation
#This indicates the spatial pattern of the greenspace accessibility points
library(ggplot2) # to create visualisations
library(ggthemes)

#for all access points
ggplot() +
  geom_bin2d(data = access_points_pd, aes(X, Y), bins=50) + # heatmap of 2d bin counts
  theme_tufte() +
  scale_fill_viridis_c(option = "plasma") +
  labs(x="", y="")

#for all access points >2ha
ggplot() +
  geom_bin2d(data = ap_more_2_pd, aes(X, Y), bins=50) + # heatmap of 2d bin counts
  theme_tufte() +
  scale_fill_viridis_c(option = "plasma") +
  labs(x="Longitude", y="Latitude")


## Now the access points have been subsetted and matched to the subset of...
#...greenspaces defined by ha size
#These access points can now be used for the isochrone creations using the mapbox api
#Isochrones will then be created for greenspaces >0.4ha
#Isochrones are determined by 'walking' mode and average time it takes to walk 400m
#Due to the amount of points being used, this is very computationally intensive...
#...the mapbox api processes 300 requests/minute
#5min for 2ha greenspace policy recommendation found here by Friends of the Earth
#https://policy.friendsoftheearth.uk/print/pdf/node/190
#5 min walk time used for 400m proximity determined by this study: https://core.ac.uk/download/pdf/82652123.pdf
#Policy recommendation of Friends of the Earth

#Isochrones made and saved in first cleaning round, so will just read in here

#READING IN ISOCHRONES MADE (Exists for all greenspaces >0.4ha)
#Reading in access point isochrones for >0.4ha greenspaces
#Analysis will focus on >2ha but reading in the >0.4-<2 iso anyway

ap_for_2_iso <-st_read("data/isochrones/for_04_to_2ha_iso_5min.shp")%>%
  st_transform(.,27700)

ap_for_20_iso <-st_read("data/isochrones/for_2_to_20ha_iso_5min.shp")%>%
  st_transform(.,27700)

ap_for_60_iso <- st_read("data/isochrones/for_20_to_60ha_iso_5min.shp")%>%
  st_transform(.,27700)

ap_for_400_iso <- st_read("data/isochrones/for_60_to_400ha_iso_5min.shp")%>%
  st_transform(.,27700)

ap_for_more_400_iso <- st_read("data/isochrones/more_400ha_iso_5min.shp")%>%
  st_transform(.,27700)

#First making an area field for all isochrones
#Geometry of each isochrone kept
ap_for_400_iso$iso_area <- st_area(ap_for_400_iso)
ap_for_2_iso$iso_area <- st_area(ap_for_2_iso)
ap_for_20_iso$iso_area <- st_area(ap_for_20_iso)
ap_for_60_iso$iso_area <- st_area(ap_for_60_iso)
ap_for_more_400_iso$iso_area <- st_area(ap_for_more_400_iso)

##Dissolving the isochrone polygons
##Methods credit to Phil Mike Jones
##https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/
#Dissolving the polygons
iso_400ha_d <- ap_for_400_iso%>%
  st_union(.,)

iso_2ha_d <- ap_for_2_iso%>%
  st_union(.,)

iso_20ha_d <- ap_for_20_iso%>%
  st_union(.,)

iso_60ha_d <- ap_for_60_iso%>%
  st_union(.,)

iso_m400ha_d <- ap_for_more_400_iso%>%
  st_union(.,)

#This determines the object type
#They come out as "sfc_MULTIPOLYGON" "sfc" (as lists)
class(iso_20ha_d)

##*The next step will be to carry out another union with each of the dissolved
##*isochrones to create one list of all isochrones for access points that are
##*associated with greenspaces >2ha

all_m2ha_iso <- iso_20ha_d%>%
  st_union(., iso_60ha_d)
all_m2ha_iso <- all_m2ha_iso%>%
  st_union(., iso_400ha_d)
all_m2ha_iso <- all_m2ha_iso%>%
  st_union(., iso_m400ha_d)

#Checking the class 
#"sfc_MULTIPOLYGON" "sfc"
#A list
class(all_m2ha_iso)

#*The final greenspace variable we want in the analysis is the proportion
#*of greenspace >2ha within a 5min walk. This will be calculated by its
#*proportion to the total area for each msoa, however when plotting the unionised
#*isochrones for greenspaces >2ha some of the isochrones don't cover all the greenspace
#*i.e. you can see this for Hyde Park. We will need to do a final union with
#*the all_m2ha_iso and the respective greenspaces to make sure all the greenspaces are
#*accounted for in the area of 'accessibility'. If this step was ignored then the
#*proportion calculations would be inaccurate as the areas which are greenspaces
#*would be assumed as areas in the msoas that aren't greenspaces.


#We'll use the gs_more_2 sf object and the all_m2ha_iso
#We need to dissolve gs_more_2 first, this was the only way I could get the union to work
#Dissolve the greenspaces
gs_more_2_d <- gs_more_2%>%
  st_union(.,)

#*Union using all_m2ha_iso and newly dissolved greenspaces >2ha
final_gs_area <- all_m2ha_iso%>%
  st_union(., gs_more_2_d)

class(final_gs_area) #"sfc_MULTIPOLYGON" "sfc"

#*So now what we'll do is convert the final_gs_area into a sf object and create an area m2 field
#*just to keep track
final_gs_area <- final_gs_area%>%
  st_cast(., "POLYGON")%>%
  enframe(.)%>%
  st_as_sf(.)%>%
  mutate(area_m2 = st_area(.))

#*Intersecting with the msoas to get proportion of greenspace for each msoa
#*Use lwgeom when geometry isn't valid
library(lwgeom)
msoa_gs <- st_intersection(st_make_valid(msoas), final_gs_area) #Intersection
msoa_gs <- msoa_gs%>%
  mutate(gs_area_m2 = st_area(.)) #create greenspace area field in m2

#*We need to aggregate the area for each msoa
#*Currently we have 1600+objs in the msoa_gs tibble
#*We need 983 objs (number of london msoas) with the area of greenspace 
#*summed and aggregated
#*What it has done is drop the msoa boundary geometries and replaced with the
#* geometries of the greenspace intersections for example, some msoas have multiple
#* versions with the different geometries and areas of the intersected greenspaces
#* The following code chunk sorts this:
#*It groups the msoas by their code
#*Then it summarises the green space areas
#*Then the geometry is dropped
#*This leaves us with a tibble
msoa_gs <- msoa_gs%>%
  group_by(MSOA11CD)%>%
  summarise(gs_area_m2 = sum(gs_area_m2))%>%
  st_drop_geometry(.)

#Joining the greenspace areas for the msoas to the msoas sf object
#Works!
msoas <- merge(msoas, msoa_gs, by="MSOA11CD", all.x=T)

#Now have an sf object with all msoas and their areas of greenspace defined by >2ha within 5 mins walk
#Creating a greenspace ha field
#Converting the areas as numeric instead of units
#Drop the land_ha field as it is a duplicate of the area_m2 field
msoas <- msoas%>%
  mutate(gs_area_ha = gs_area_m2 / 10000,
         gs_area_m2 = as.numeric(gs_area_m2),
         gs_area_ha = as.numeric(gs_area_ha),
         area_m2 = as.numeric(area_m2),
         area_ha = as.numeric(area_ha))%>%
  dplyr::select(-28)

#We have some NA values in the gs_area fields
#Convert these to 0
msoas <- msoas%>%
  mutate(gs_area_ha = if_else(is.na(gs_area_ha), 0, gs_area_ha),
         gs_area_m2 = if_else(is.na(gs_area_m2), 0, gs_area_m2))

##Saving the msoas
st_write(msoas, "data/msoas/msoas_final.shp")

#The final dataset is ready for analysis (transformation of variables in analysis section)

