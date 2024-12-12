#MAPPING NYC'S IMMIGRANT NEIGHBORHOOD: CLUSTERS, ECONOMIC AND 
#AND RENT AFFORDABILITY  CHARACTERISTICS
###############################################################################


###########This section installs necessary packages for the analysis###########
#Load the required libraries to aid in the analysis and visualiation
library(tidyverse)       # Data manipulation and visualization
library(sf)              # Spatial data handling
library(spdep)           # Spatial analysis
library(ggplot2)         # Visualization
library(corrplot)        # Correlation plots
library(readr)           # Reading CSV files
library(viridisLite)     # Color palettes for visuals
library(leaflet)         # Interactive maps
library(viridis)         # Advanced color scales
library(htmltools)       # HTML content for visuals
library(dplyr)           # Data wrangling
################################################################################


#########This section sets the working directory for the Analysis###############
#Set working directory, r folder  
setwd("C:/Users/bkumi/Downloads/Final Project")
data_path <- "C:/Users/bkumi/Downloads/Final Project"
################################################################################


##########This section loads into R studio, the already downloaded data#########
#Load datasets  

#CAN ALSO FIND DATASETS HERE ONLINE 
#NYC NTA SHAPEFILE DATASET: https://data.cityofnewyork.us/City-Government/NTA-map/d3qk-pfyz
#IMMIGRATION, HOUSING AND ECONOMIC DATASETS - https://data.cityofnewyork.us/City-Government/Demographic-Profiles-of-ACS-5-Year-Estimates-at-th/8cwr-7pqn/about_data

#NYC NTA shapefile data
nta_shapefile <- st_read(paste0(data_path,"/NTA map/geo_export_d42402da-f341-4eec-b843-74e562c5e841.shp"))

#American Communty Survey Immigration Dataset
immigration_data <- read_csv(paste0(data_path,"/American Community Survey 5yrNTA/soc_2016acs5yr_nta.csv"))

#American Community Survey - Housing Dataset
housing_data <- read_csv(paste0(data_path, "/American Community Survey 5yrNTA/hous_2016acs5yr_nta.csv"))

#American Community Survey - Economic Dataset
economic_data <- read_csv(paste0(data_path,"/American Community Survey 5yrNTA/econ_2016acs5yr_nta.csv"))
################################################################################


##########This section cleans the dataset loaded into R studio ###############
# Data Cleaning and Aggregation
# Clean immigration dataset
immigration_clean <- immigration_data %>%
  rename(ntaname = `GeogName`, ntacode = `GeoID`)

# Clean housing dataset
housing_clean <- housing_data %>%
  rename(ntaname = `GeogName`, ntacode = `GeoID`)

# Clean economic dataset
economic_clean<- economic_data %>% 
  rename(ntaname = `GeogName`, ntacode = `GeoID`)
################################################################################


#############This section joins all the dataset by a common column##############
# Merge datasets with spatial data
NYC_nta_immig <- nta_shapefile %>%
  left_join(immigration_clean, by = "ntacode") %>%
  left_join(housing_clean, by = "ntacode") %>% 
  left_join(economic_clean, by = "ntacode")
summary(NYC_nta_immig)
################################################################################


###Segregate and clean only the needed data for analyzing  POVERTY LEVEL by NTA#
#The columns in the data are over 3000, therefore need, to segregate#
NYC_nta_immig_poverty_data <- NYC_nta_immig %>%
  select(ntacode, geometry, PvU50E, Pv50t74E, Pv75t99E, Pv100t124E, Pv125t149E,
         Pv150t174E, Pv175t184E, Pv185t199E, Pv200t299E,
         Pv300t399E, Pv400t499E, Pv500plE)

#Clean segregated data and remove commas in the numbers for easy computations
NYC_nta_immig <- NYC_nta_immig %>%
  mutate(across(
    .cols = where(is.character) & !all_of("geometry"),  # Exclude "geometry"
    .fns = ~ gsub(",", "", .)
  ))
write.csv(NYC_nta_immig_poverty_data,"NYC_for_povertyratio_analysis.csv")
st_write(NYC_nta_immig_poverty_data, "NYC_for_povertyratio_analysis.shp")


######Segregate only the needed data for analysing housing affordability by NTA#####
NYC_nta_housing <- NYC_nta_immig %>% select(ntacode, geometry, OcHUPRnt2E, GRPIU15E,  
                                            GRPI15t19E, GRPI20t24E, GRPI25t29E, GRPI30t34E, 
                                            GRPI35plE, GRPI50plE, GRPINtCE
)

#Clean segragated data and remove commas in the numbers for easy computations for housing analysis
NYC_nta_housing <- NYC_nta_housing %>%
  mutate(across(
    .cols = where(is.character) & !all_of("geometry"),  # Exclude "geometry"
    .fns = ~ gsub(",", "", .)
  ))
st_write(NYC_nta_housing, "NYC_nta_housing_analysis.shp")
################################################################################


#OBJECTIVE 1: WHERE ARE THE CLUSTERS OF IMMIGRANT NEIGHBORHOODS IN NYC?

###########CLUSTER OF IMMIGRANT NEIGHBORHOOD ANALYSIS##############
#######This section does a Descriptive Statistical Analyzing the dataset 
#to analyze the NYC population based on nativity and citizenship status from ACS#

#PLACE OF BIRTH				             column name                  excel column
#Total population	                 Pop_4	            DP02	          RV	Persons
#Native	                           NtvE	              DP02	          SA	Persons
#Born in United States	           NtvUSE	            DP02	          SF	Persons
#Born in New York State	           NtvNYSE	          DP02	          SK	Persons
#Born outside New York State	     NtvNotNYSE	        DP02	          SP	Persons
#Born in Puerto Rico,
#U.S. Island areas, or born abroad 
#to American parent(s)	           NtvPRUSAbE	        DP02	          SU	Persons
#Foreign-born	                     Fb1E	              DP02	          SZ	Persons
#Naturalized U.S. citizen	         FbNtlzdE	          DP02	          AYD	Persons
#Not a U.S. citizen	               FbNotCznE	        DP02	          AYI	Persons

NYC_demog_by_nta <- NYC_nta_immig %>%
  group_by(ntacode) %>%
  summarize(
    total_population = sum(NtvE + Fb1E, na.rm = TRUE),  # Total population
    native_population = sum(NtvE, na.rm = TRUE),        # Native population
    foreign_born_population = sum(Fb1E, na.rm = TRUE),  # Foreign-born population
    naturalized_citizens = sum(FbNtlzdE, na.rm = TRUE), # Naturalized U.S. citizens
    non_us_citizens = sum(FbNotCznE, na.rm = TRUE)      # Non-U.S. citizens
 
  
    ) %>%
  mutate(
    percent_native = (native_population / total_population) * 100,
    percent_foreign_born = (foreign_born_population / total_population) * 100,
    percent_naturalized = (naturalized_citizens / foreign_born_population) * 100,
    percent_non_us_citizens = (non_us_citizens / foreign_born_population) * 100
  )

# Display the summary
print(NYC_demog_by_nta)

# Create centroids from the polygons (NTA boundaries)
centroids_sf <- NYC_demog_by_nta[, "geometry"]

NYC_demog_by_nta$centroid <- st_centroid(NYC_demog_by_nta$geometry)
################################################################################


###This section plots a chloropleth map Native Born New Yorkers vs Foreign Born#
ggplot(data = NYC_demog_by_nta) +
  geom_sf(aes(fill = percent_foreign_born)) +
  scale_fill_viridis_c(option = "magma",name = "Foreign-Born (%)") +  
  labs(title = "LANDSCAPE OF IMMIGRANT NEIGHBORHOODS",
       subtitle = "Concentrations: Bronx,Queens and Brooklyn - is rent price a reason?",
       caption = "Source: NYC ACS 5-Year Data (2016)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )+ 
  guides(fill = guide_colorbar(title = "Foreign-Born (%)", 
                               title.position = "top", 
                               barwidth = 1, 
                               barheight = 10))
################################################################################


##################Stacked Bar Graph to Show Immigration Breakdown###############
# Calculate total population and proportions
NYC_demog_summary <- NYC_demog_by_nta %>%
  summarize(
    native_population = sum(native_population, na.rm = TRUE),
    naturalized_citizens = sum(naturalized_citizens, na.rm = TRUE),
    foreign_born_population = sum(foreign_born_population, na.rm = TRUE)
  ) %>%
  mutate(
    citizen_total = native_population + naturalized_citizens,
    non_citizen_total = foreign_born_population,
    total_population = citizen_total + non_citizen_total,
    citizen_percentage = (citizen_total / total_population) * 100,
    non_citizen_percentage = (non_citizen_total / total_population) * 100
  )

# Prepare data for plotting
NYC_demog_long <- data.frame(
  group = c("Citizens", "Citizens", "Non-Citizens"),
  category = c("Native", "Naturalized", "Non-Citizen"),
  population = c(
    NYC_demog_summary$native_population,
    NYC_demog_summary$naturalized_citizens,
    NYC_demog_summary$non_citizen_total
  ),
  group_total = c(
    NYC_demog_summary$citizen_total,
    NYC_demog_summary$citizen_total,
    NYC_demog_summary$non_citizen_total
  )
) %>%
  mutate(
    group_percentage = population / NYC_demog_summary$total_population * 100,
    category_percentage_within_group = ifelse(
      group == "Citizens",
      (population / group_total) * 100,
      NA
    ) # Only calculate within-group percentages for Citizens
  )

# Bar Graph Visualization
ggplot(NYC_demog_long, aes(x = group, y = group_percentage, fill = category)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.6) +
  labs(
    title = "Citizens vs Non-Citizens New Yorkers",
    subtitle = "Substantial Immigrant Population",
    x = "Population Groups",
    y = "Percentage of Total Population",
    fill = "Subcategory"
  ) +
  geom_text(
    aes(
      label = ifelse(
        !is.na(category_percentage_within_group),
        paste0(round(category_percentage_within_group, 1), "%"),
        paste0(round(group_percentage, 1), "%")
      ),
      group = category
    ),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.5
  ) +
  scale_fill_manual(values = c("blue", "magenta", "darkblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +
  annotate(
    "text",
    x = 1,
    y = NYC_demog_summary$citizen_percentage + 5,
    label = paste0("Citizens = ", round(NYC_demog_summary$citizen_percentage, 1), "%"),
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 2,
    y = NYC_demog_summary$non_citizen_percentage + 5,
    label = paste0("Non-Citizens = ", round(NYC_demog_summary$non_citizen_percentage, 1), "%"),
    size = 4,
    fontface = "bold"
  )
################################################################################


#########This section tests if there is spatial clustering of immigrants##########
######This is to back the findings from the cloropleth map######################
#Mix of Multipolygons and polygons creating errors therefore convert multipolygons to polygons#####
Nta_immig_cluster <- st_make_valid(NYC_demog_by_nta)

# Convert MULTIPOLYGON to POLYGON
Nta_immig_cluster$geometry <- lapply(st_geometry(Nta_immig_cluster), function(geom) {
  if (st_geometry_type(geom) == "MULTIPOLYGON") {
    st_cast(geom, "POLYGON")
  } else {
    geom
  }
}) %>% st_sfc(crs = st_crs(Nta_immig_cluster))

nta_Immig_Centroid <- st_centroid(Nta_immig_cluster)
################################################################################


####################MORAN'S I ANALYSIS#############################
# 1.Extract coordinates from the geometry column
# 2.Create spatial weights matrix based on 4 nearest neighbors
# 3.Convert to listw format for spatial weights
# 4.Perform Moran's I test for non-US citizens percentage as numeric
# 5.Print Moran's I test results

nta_Immig_Centroid <- st_sf(geometry = st_geometry(nta_Immig_Centroid), data = NYC_demog_by_nta)
coords <- st_coordinates(nta_Immig_Centroid)
nb <- knn2nb(knearneigh(coords, k = 4))  # Adjust k as needed
listw <- nb2listw(nb, style = "W")
moran_test <- moran.test(as.numeric(nta_Immig_Centroid$data.non_us_citizens), listw)
print(moran_test)
################################################################################


#####OBJECTIVE 2a: IMMIGRANT NEIGHBOURHOOD ECONOMIC CHARACTERISTICS############
#######This section does a Descriptive Statistics to analyze the NYC Immigrant 
#economic characteristics using Dataset from the American Community survey#######
# RATIO OF INCOME TO POVERTY LEVEL				
# Population for whom poverty status is determined	     PopPvU2		VW	Persons
# Under .50	                                             PvU50E		  WB	Persons
# .50 to .74	                                           Pv50t74E		WG	Persons
# .75 to .99	                                           Pv75t99E		WL	Persons
# 1.00 to 1.24	                                         Pv100t124E	WQ	Persons
# 1.25 to 1.49	                                         Pv125t149E	WV	Persons
# 1.50 to 1.74	                                         Pv150t174E	XA	Persons
# 1.75 to 1.84	                                         Pv175t184E	XF	Persons
# 1.85 to 1.99	                                         Pv185t199E	XK	Persons
# 2.00 to 2.99	                                         Pv200t299E	XP	Persons
# 3.00 to 3.99	                                         Pv300t399E	XU	Persons
# 4.00 to 4.99	                                         Pv400t499E	XZ	Persons
# 5.00 and over	                                         Pv500plE		YE	Persons

# Analyze income-to-poverty ratio by NTA and filter outliers
NYC_nta_immig_poverty_reduced <- NYC_nta_immig_poverty_data %>%
  group_by(ntacode) %>%
  summarize(
    geometry = st_union(geometry), st_sf(geometry = geometry, crs = 4326),
    total_population = sum(PvU50E + Pv50t74E + Pv75t99E + Pv100t124E + Pv125t149E +
                             Pv150t174E + Pv175t184E + Pv185t199E + Pv200t299E +
                             Pv300t399E + Pv400t499E + Pv500plE, na.rm = TRUE),
    below_poverty = sum(PvU50E + Pv50t74E + Pv75t99E, na.rm = TRUE), # Ratio < 1.0
    above_poverty = sum(Pv100t124E + Pv125t149E +
                          Pv150t174E + Pv175t184E + Pv185t199E + Pv200t299E +
                          Pv300t399E + Pv400t499E + Pv500plE, na.rm = TRUE) # Ratio ≥ 1.0
  ) %>%
  mutate(
    percent_below_poverty = (below_poverty / total_population) * 100,
    percent_above_poverty = (above_poverty / total_population) * 100,
    poverty_ratio = above_poverty / below_poverty # Calculate the ratio
  ) %>%
  filter(ntacode != "MN99") # Filter out NTAs with outlier values especially for parks

# Print the filtered dataset
print(NYC_nta_immig_poverty_reduced)



################################################################################  
# Plotting Chloropleth Map of Poverty Analysis by NTA
ggplot(data = NYC_nta_immig_poverty_reduced) +
  geom_sf(aes(geometry = geometry, fill = percent_below_poverty), color = "black", size = 0.2) +  # Polygons with borders
  scale_fill_viridis_c(option = "plasma", name = "Percent Below Poverty") +  
  labs(
    title = "NYC POVERTY ANALYSIS",
    subtitle = "Percentage Ratio of Income to Poverty level < 1 -
    Concentation: Harlem, Bronx and Parts of Brooklyn, 
    Any Correlation to Immigration?",
    caption = "Source:ACS (2016)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 10),
    legend.position = "bottom"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )
################################################################################


#####OBJECTIVE 2b: IMMIGRANT NEIGHBOURHOOD RENT CHARACTERISTICS#################
#######This section does a Descriptive Statistics to analyze the NYC rent 
#characteristic using Dataset from the American Community survey##########

# GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME 				
# Occupied units paying rent (excluding units where GRAPI cannot be computed)	OcHUPRnt2	DP04	RL	Households
# Less than 15.0 percent	GRPIU15E	  DP04	  RQ	Households
# 15.0 to 19.9 percent	 GRPI15t19E	  DP04	  RV	Households
# 20.0 to 24.9 percent	 GRPI20t24E	  DP04	  SA	Households
# 25.0 to 29.9 percent	 GRPI25t29E	  DP04	  SF	Households
# 30.0 to 34.9 percent	 GRPI30t34E	  DP04	  SK	Households
# 35.0 percent or more	 GRPI35plE	  DP04	  SP	Households
# 50.0 percent or more	 GRPI50plE	  B25070	SU	Households
# Not computed	         GRPINtCE	    DP04	  SZ	Households

print(NYC_nta_housing)

housing_affordability <-NYC_nta_housing %>%
  group_by(ntacode) %>% 
  summarize(
    total_occupied_units = sum(NYC_nta_housing$GRPIU15E + NYC_nta_housing$GRPI15t19E + 
                                 NYC_nta_housing$GRPI20t24E + NYC_nta_housing$GRPI25t29E + 
                                 NYC_nta_housing$GRPI30t34E + NYC_nta_housing$GRPI35plE +
                                 NYC_nta_housing$GRPI50plE, na.rm = TRUE),      # Total occupied units paying rent
    affordable_units = sum(GRPIU15E + GRPI15t19E + GRPI20t24E + GRPI25t29E, na.rm = TRUE), # Units paying < 30%
    unaffordable_units = sum(GRPI30t34E + GRPI35plE, na.rm = TRUE),  # Units paying ≥ 30%
    severely_unaffordable_units = sum(GRPI50plE, na.rm = TRUE),  # Units paying ≥ 50%
    not_computed_units = sum(GRPINtCE, na.rm = TRUE)            # Units where GRAPI cannot be computed
  ) %>%
  mutate(
    percent_affordable = (affordable_units / total_occupied_units) * 100,
    percent_unaffordable = (unaffordable_units / total_occupied_units) * 100,
    percent_severely_unaffordable = (severely_unaffordable_units / total_occupied_units) * 100
  )

# View summarized data
print(housing_affordability)
################################################################################


######################BAR GRAPH- NYC HOUSEHOLD AFFORDABILITY#####################
# Somewhat good visualisation, may not use in story
#Create a simple data frame for visualization
affordability_summary <- housing_affordability %>%
  pivot_longer(
    cols = c(percent_affordable, percent_unaffordable, percent_severely_unaffordable),
    names_to = "Percentage of Income to Rent",   
    values_to = "Percentage of Population"      
  )

# Plot Bar graph of NYC Household income to rent affordability
ggplot(affordability_summary, aes(x = `Percentage of Income to Rent`, y = `Percentage of Population`, fill = `Percentage of Income to Rent`)) +
  geom_bar(stat = "identity") +
  labs(
    title = "NYC Household Income to Rent Affordability",               
    subtitle = "Gross Rent as Percent of Household Income\nWhich NTA falls within the unaffordable and severely unaffordable category?\nDoes Immigration have a correlation with this?",
    x = "Affordability",
    y = "Percentage of Households",
    fill = "Affordability Category"                    
  ) +
  scale_fill_manual(
    values = c("darkmagenta", "purple", "orange"),
    labels = c("Affordable (<30%)", "Unaffordable (30-49%)", "Severely Unaffordable (≥50%)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),            
    plot.subtitle = element_text(hjust = 0)           
  )
################################################################################


######################CHLOROPLETH MAP OF NYC HOUSEHOLD RENT UNAFFORDABILITY#####
ggplot(data = housing_affordability) +
  geom_sf(aes(fill = percent_unaffordable), color = NA) +
  scale_fill_viridis_c(
    name = "% of Unaffordable Households by NTA",
    option = "magma",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "Proportion of Households that are 'Rent-Burdened'",
    subtitle = "Percentage of total occupied units that are unaffordable (>30% income)",
    caption = "Source: NYC 5YR ACS NTA Housing Data"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_blank(),         # Corrected syntax
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

##############CHLOROPLETH MAP OF NYC HOUSEHOLD RENT SEVERE UNAFFORDABILITY######

ggplot(data = housing_affordability) +
  geom_sf(aes(fill = percent_severely_unaffordable), color = NA) +
  scale_fill_viridis_c(
    name = "% of Severely Unaffordable Households by NTA",
    option = "magma",
    direction = -1
  ) +
  theme_minimal() +
  labs(
    title = "'Proportion of Households that are 'Severely Rent-Burdened'",
    subtitle = "Percentage of total occupied units that are severely unaffordable (>50% income)",
    caption = "Source: NYC 5YR ACS NTA Housing Data (2016)"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),    # Adjusted subtitle size for consistency
    axis.title = element_blank(),              # Corrected syntax
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )
################################################################################


###OBJECTIVE 3: HOW DOES AFFORDABLE HOUSING AVAILABILITY VARY BY IMMIGRANT DENSITY?###
##########################CORRELATION ANALYSIS###################################
# Correlation analysis between NTAs with high immigration and NTAs below poverty line
high_immigration_poverty_correlation <- cor(
  NYC_demog_by_nta$percent_foreign_born,
  NYC_nta_immig_poverty_reduced$percent_below_poverty,
  use = "complete.obs"
)

print(paste("Correlation between high immigration and poverty below the poverty line: ", 
            high_immigration_poverty_correlation))

# Correlation analysis between NTAs with high immigration and NTAs with percent_unaffordable and percent_severely_unaffordable
immig_unaffordable_correlation <- cor(
  NYC_demog_by_nta$percent_foreign_born,
  housing_affordability$percent_unaffordable,
  use = "complete.obs"
)

severe_immig_unaffordable_correlation <- cor(
  NYC_demog_by_nta$percent_foreign_born,
  housing_affordability$percent_severely_unaffordable,
  use = "complete.obs"
)

print(paste("Correlation between high immigration and unaffordable housing: ", immig_unaffordable_correlation))
print(paste("Correlation between high immigration and severely unaffordable housing: ", severe_immig_unaffordable_correlation))
################################################################################


#############VISUAL PLOTTING OF CORRELATION THROUGH SCATTER PLOT ###############
library(ggplot2)

# Scatter Plot: High Immigration vs. Poverty Below Poverty Line
ggplot(
  data = data.frame(
    percent_foreign_born = NYC_demog_by_nta$percent_foreign_born,
    percent_below_poverty = NYC_nta_immig_poverty_reduced$percent_below_poverty
  ),
  aes(x = percent_foreign_born, y = percent_below_poverty)
) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "High Immigration vs. Poverty Below Poverty Line",
    subtitle = paste(
      "Correlation:", round(high_immigration_poverty_correlation, 3), 
      "- Very Weak Positive Correlation, Minimal Association"
    ),
    x = "Immigrant Population",
    y = "Below Poverty Line"
  ) +
  theme_minimal()


# Scatter Plot: High Immigration vs. Unaffordable Housing
ggplot(
  data = data.frame(
    percent_foreign_born = NYC_demog_by_nta$percent_foreign_born,
    percent_unaffordable = housing_affordability$percent_unaffordable
  ),
  aes(x = percent_foreign_born, y = percent_unaffordable)
) +
  geom_point(color = "orange", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "brown") +
  labs(
    title = "High Immigration vs. Unaffordable Housing",
    subtitle = paste(
      "Correlation:", round(immig_unaffordable_correlation, 3), 
      "- Moderate Positive Correlation, Minimal Association"
    ),
    x = "Percentage of Foreign-Born Population",
    y = "Percentage of Unaffordable Housing"
  ) +
  theme_minimal()


# Scatter Plot: High Immigration vs. Severely Unaffordable Housing
ggplot(
  data = data.frame(
    percent_foreign_born = NYC_demog_by_nta$percent_foreign_born,
    percent_severely_unaffordable = housing_affordability$percent_severely_unaffordable
  ),
  aes(x = percent_foreign_born, y = percent_severely_unaffordable)
) +
  geom_point(color = "orange", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "brown") +
  labs(
    title = "High Immigration vs. Severely Unaffordable Housing",
    subtitle = paste(
      "Correlation:", round(severe_immig_unaffordable_correlation, 3), 
      "- Moderate Positive Correlation, Minimal Association"
    ),
    x = "Percentage of Foreign-Born Population",
    y = "Percentage of Severely Unaffordable Housing"
  ) +
  theme_minimal()

##################################################################################################################

# End of script
