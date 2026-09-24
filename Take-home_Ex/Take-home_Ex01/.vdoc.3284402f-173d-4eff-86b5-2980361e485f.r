#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
pacman::p_load(tidyverse, sf, tmap, spNetwork, spatstat, plotly, gtsummary, sparr)
#library(tidyverse, sf, tmap, readr) #, spNetwork, spatstat, plotly, gtsummary, sparr) 
#
#
#
pacman::p_load(readr, dplyr, lubridate, ggplot2, tmap, sf, spNetwork, spatstat, plotly, gtsummary, sparr)
#
#
#
#
#
#
#
#
#
set.seed(1234)
#
#
#
#
#
#
#
bmr_acc <- read_csv(
  "../../data/takehome01/thai_road_accident_2019_2022.csv",
  show_col_types = FALSE
)
#
#
#
bmr_acc

#
#
#
#
#
null_counts <-   sum(is.na(bmr_acc))
null_counts
#
#
#
#
#
data.frame(
  Column = colnames(bmr_acc),
  Missing_Values = colSums(is.na(bmr_acc))
)
#
#
#
#
#
#
#

bmr_acc_sf <- bmr_acc %>%
  filter(province_en == "Bangkok" | province_en == "Nonthaburi" | province_en == "Pathum Thani" | province_en == "Samut Prakan" | province_en == "Nakhon Pathom" | province_en == "Samut Sakhon") %>%
  #filter(year(incident_datetime) == 2022) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32647) # UTM Zone 47N (EPSG:32647) for Bangkok Metropolitan Region
#
#
#
#
#
null_counts_after <- sum(is.na(bmr_acc_sf))
null_counts_after
#
#
#
#
#
# Check for duplicate records based on the 'acc_code' column
duplicates <- bmr_acc_sf[duplicated(bmr_acc_sf$acc_code), ]
nrow(duplicates)
#
#
#
#
#
#
#
#| echo: false
#| eval: false
bmr_acc_sf <- bmr_acc_sf[!duplicated(bmr_acc_sf$acc_code), ]
#
#
#
# Saving the cleaned and filtered road traffic accident data for the Bangkok Metropolitan Region in RDS format for future use.
#| echo: false
#| eval: false
saveRDS  (bmr_acc_sf, file = "../../data/takehome01/bmr_road_traffic_accidents_2019-2022.rds")
#
#
#
bmr_acc_sf <- readRDS("../../data/takehome01/bmr_road_traffic_accidents_2019-2022.rds")

#
#
#
#
#
adm_boundary <- st_read(dsn = "../../data/takehome01/tha_admin2.shp", # layer 2 for district level boundaries
                layer = "tha_admin2") %>% 
  st_transform(crs = 32647)
#
#
#
#
#
st_crs(adm_boundary) 
#
#
#
#
#
#
#
glimpse(adm_boundary) # 
#
#
#
#
#
#
#
bmr_adm_boundary <- adm_boundary %>%
  select("adm1_name") %>%
  filter(adm1_name == "Bangkok" | adm1_name == "Nonthaburi" | adm1_name == "Pathum Thani" | adm1_name == "Samut Prakan" | adm1_name == "Nakhon Pathom" | adm1_name == "Samut Sakhon")
#
#
#
#
#
#| echo: false
#| eval: false
saveRDS(bmr_adm_boundary, file = "../../data/takehome01/bmr_adm_boundary.rds")
#
#
#
bmr_adm_boundary <- read_rds("../../data/takehome01/bmr_adm_boundary.rds")
#
#
#
bmr_adm_boundary
#
#
#
#
#
tmap_mode("plot")
tm_shape(bmr_adm_boundary) +
  tm_fill(col = "adm1_name", title = "Region") + 
  tm_borders() +
  tm_layout(main.title = "BMR Administrative Boundaries",
            main.title.position = "center",
            main.title.size = 3,
            #title.position = c("center", "top"),
            legend.position = c("left", "bottom")
           )
#
#
#
#
#
#
#
thailand_roads <- st_read(dsn = "../../data/takehome01/hotosm_tha_roads_lines_shp.shp", layer = "hotosm_tha_roads_lines_shp") %>%
  st_set_crs(4326) %>% # Set the coordinate reference system to WGS 84 (EPSG:4326)
  st_transform(crs = 32647) # UTM Zone 47N (EPSG:32647) for Bangkok Metropolitan Region
#
#
#
#
#
bmr_roads_orig <- thailand_roads %>%
  st_intersection(bmr_adm_boundary) # Filter road segments within the BMR
#
#
#
#
#
#
#
#
#
bmr_roads <- bmr_roads_orig %>%
  st_cast("LINESTRING") # Convert MULTILINESTRING to LINESTRING geometries
#
#
#
#
#
st_crs(bmr_roads) 
#
#
#
#
#
tmap_mode("plot") 
tm_shape(bmr_roads) +
  tm_lines() +
  tm_layout(main.title = "BMR Open Street Map",
            main.title.position = "center",
            main.title.size = 3,
            legend.position = c("left", "bottom")
           )
#
#
#
#
#
glimpse(bmr_roads) 
#
#
#
#
#
#
#
unique(bmr_roads$highway)
#
#
#
#
#
hw_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified")

bmr_roads_filtered <- bmr_roads %>%
  select("highway") %>% 
  filter(highway %in% hw_types)
#
#
#
glimpse(bmr_roads_filtered)
#
#
#
#
#
tmap_mode("plot")
tm_shape(bmr_roads_filtered) +
  tm_lines(col = "highway", title = "Road Type") +
  tm_layout(main.title = "BMR Primary Road Network",
            main.title.position = "center",
            main.title.size = 3,
            legend.position = c("left", "bottom")
           )
#
#
#
#
#
#
#
#
#
# save the filtered primary road network data for the Bangkok Metropolitan Region (BMR) in RDS format for future use.
saveRDS(bmr_roads_filtered, file = "../../data/takehome01/bmr_primary_roads.rds")
#
#
#
bmr_primary_roads <- read_rds("../../data/takehome01/bmr_primary_roads.rds")
#
#
#
bmr_acc_sf <- read_rds("data/takehome01/bmr_acc_sf.rds")
#
#
#
#
#
#
#
#
#
#
#
tmap_mode('plot')

tm_shape(bmr_adm_boundary) +
  tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=1, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.1, alpha = 0.5) +                                                
  tm_layout(
    main.title = "Road Traffic Accidents in Bangkok Metropolitan Region (2022)",
    main.title.position = c("center", "top"), 
    main.title.size = 0.9,
    frame = FALSE,
    legend.outside = TRUE,               
    legend.outside.position = "left", 
    legend.outside.size = 0.25,     
    legend.text.size = 0.55, 
    legend.title.size = 0.7 
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
bmr_acc_sf <- bmr_acc_sf %>%
  mutate(year = year(incident_datetime))
#
#
#
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.2, alpha = 0.6) +                                                
  tm_facets(by = "year") +
  tm_layout(
    main.title = "Road Traffic Accidents in Bangkok Metropolitan Region (2019-2022)",
    main.title.position = c("center", "top"), 
    main.title.size = 0.9,
    frame = FALSE,
    #legend.outside = TRUE,               
    #legend.outside.position = "left", 
    #legend.outside.size = 0.25,     
    #legend.text.size = 0.55, 
    #legend.title.size = 0.7 
  )
#
#
#
#
#
# calculate total accidents by year
bmr_acc_by_year <- bmr_acc_sf %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(total_accidents = n())

# Calculate total accidents by province and year
bmr_acc_by_province_year <- bmr_acc_sf %>%
  st_drop_geometry() %>%
  group_by(year, province_en) %>%
  summarise(total_accidents = n()) %>%
  ungroup()

# Summarise total accidents for each year (trend)
total_accidents_by_year <- bmr_acc_by_province_year %>%
  group_by(year) %>%
  summarise(total_accidents = sum(total_accidents))

# create stacked bar chart for accidents by province and year
fig_acc_by_province_year <- ggplot(bmr_acc_by_province_year, aes(x = factor(year), y = total_accidents, fill = province_en)) +
  geom_bar(stat = "identity") +
  labs(title = "Road Traffic Accidents by Province and Year",
       x = "Year",
       y = "Total Accidents",
       fill = "Province_en") +
  theme_minimal() +
  # add trend line for total accidents by year
  geom_line(data = total_accidents_by_year, 
  aes(x = factor(year), y = total_accidents, group = 1), 
  color = "black", size = 1,
  linetype = "dashed",
  inherit.aes = FALSE) 

fig_acc_by_province_year
#
#
#
#
#
#
#
#
#
#
#
#
#
# mutate month column
bmr_acc_sf <- bmr_acc_sf %>%
  mutate(month = lubridate::month(incident_datetime, label = TRUE)) 
#
#
#
# Visualization Geographic Distribution of Accidents by month
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.2, alpha = 0.6) +                                                
  tm_facets(by = "month") +
  tm_layout(
    main.title = "Road Traffic Accidents by month in Bangkok Metropolitan Region (2019-2022)",
    main.title.position = c("center", "top"), 
    main.title.size = 0.9,
    frame = FALSE
  )

#
#
#
#
# create a summary of accidents by month and province
bmr_acc_by_month_province <- bmr_acc_sf %>%
  st_drop_geometry() %>%
  group_by(month, province_en) %>%
  summarise(total_accidents = n()) %>%
  ungroup()

# create stacked bar chart for accidents by month and province
fig_acc_by_month_province <- ggplot(bmr_acc_by_month_province, aes(x = month, y = total_accidents, fill = province_en)) +
  geom_bar(stat = "identity") +
  labs(title = "Road Traffic Accidents by Month and Province",
       x = "Month",
       y = "Total Accidents",
       fill = "Province_en") +
  theme_minimal()

fig_acc_by_month_province
#
#
#
#
#
#
#
#
#
#
#
# mutate day of the week column
bmr_acc_sf <- bmr_acc_sf %>%
  mutate(day_of_week = lubridate::wday(incident_datetime, label = TRUE)) 
#
#
#
#
# Visualization Geographic Distribution of Accidents by day of the week
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.2, alpha = 0.6) +                                                
  tm_facets(by = "day_of_week") +
  tm_layout(
    main.title = "Road Traffic Accidents by day of the week in Bangkok Metropolitan Region (2019-2022)",
    main.title.position = c("center", "top"), 
    main.title.size = 0.9,
    frame = FALSE
  )
#
#
#
#compute accidents by day of the week 
accidents_by_day_of_week <- bmr_acc_sf %>%
  st_drop_geometry() %>%
  mutate(
    day_type = case_when(
      day_of_week %in% c("Sat", "Sun") ~ "Weekend",
      TRUE ~ "Weekday"
    )
  ) %>%
  group_by(day_of_week, day_type) %>%
  summarise(total_accidents = n()) %>%
  ungroup()
#
#
#
# create a bar chart for accidents by day of the week and day type in different colors
ggplot(accidents_by_day_of_week, aes(x = day_of_week, y = total_accidents, fill = day_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Road Traffic Accidents by Day of the Week and Day Type",
       x = "Day of the Week",
       y = "Total Accidents",
       fill = "Day Type") +
  theme_minimal()
#
#
#
#
#
#
#
#
#
#
#
#
# mutate peak hour column
bmr_acc_sf <- bmr_acc_sf %>%
  st_drop_geometry() %>%
  mutate(hour = lubridate::hour(incident_datetime)) %>%
  mutate(
    peak_hour = case_when(
      hour %in% 6:9 ~ "Morning Peak",
      hour %in% 16:19 ~ "Afternoon Peak",
      TRUE ~ "Off-Peak"
    )
  ) 
```
#
bmr_acc_sf$peak_hour
#
#
#
# Visualization Geographic Distribution of Accidents by peak hours
tmap_mode('view')
tm_shape(bmr_adm_boundary) +
  
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.2, fill_alpha = 0.6) +                                                
  tm_facets(by = "peak_hour") +
  tm_title (
    main.title = "Road Traffic Accidents by Peak Hours in Bangkok Metropolitan Region (2019-2022)",
    main.title.position = c("center", "top"), 
    main.title.size = 1.5,
  )
#
#
#

# For each object, look at:
st_geometry_type(bmr_acc_sf) |> table()
st_crs(bmr_acc_sf)
glimpse(bmr_acc_sf)

st_geometry_type(bmr_adm_boundary) |> table()
st_crs(bmr_adm_boundary)
glimpse(bmr_adm_boundary)

st_geometry_type(bmr_primary_roads) |> table()
st_crs(bmr_primary_roads)
glimpse(bmr_primary_roads)

#
#
#
#
#
#
#
# 1. Filter to study year (2022, per the brief's mandatory restriction 
#    for this dataset)
acc_2022 <- bmr_acc_sf |>
  filter(lubridate::year(incident_datetime) == 2022)

nrow(acc_2022)  # sanity check on sample size

# 2. Apply jitter to break coincident points
JITTER_RADIUS <- 100 # ~ 100 meters
set.seed(1234)  # reproducibility - document this in your report
acc_jittered <- st_jitter(acc_2022, amount = JITTER_RADIUS)

# confirm duplicates resolved
sum(duplicated(st_coordinates(acc_jittered)))

# 3. Build owin from the BMR union, and convert to ppp
library(spatstat)

bmr_owin <- as.owin(st_union(bmr_adm_boundary))
acc_ppp  <- as.ppp(st_coordinates(acc_jittered), W = bmr_owin)

acc_ppp        # summary: check n, window area, units
plot(acc_ppp, main = "BMR 2022 Accidents")
#
#
#
#
#
# Re-run as.ppp with a check, and explicitly report + drop rejects
acc_ppp <- as.ppp(st_coordinates(acc_jittered), W = bmr_owin, check = TRUE)
n_rejected <- length(attr(acc_ppp, "rejects")$x)
n_rejected  # confirm it's 4

# The rejects are already excluded from the main point set (3589 is correct,
# not 3593) — attr(,"rejects") is just a stored record for transparency.
# To plot cleanly without the warning:
plot(acc_ppp, main = "BMR 2022 Accidents", clipwin = bmr_owin)
#
#
#
#
library(spatstat)
clark_evans_test <- clarkevans.test(acc_ppp,
  correction="none",
  clipregion = bmr_owin,
  alternative = "clustered"
  )
clark_evans_test
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Calculate the kernel density estimate
acc_kde <- density(acc_ppp, sigma = 0.1)
plot(acc_kde, main = "BMR 2022 Accidents - KDE")

# add the regional boundary on top for context
plot(bmr_owin, add = TRUE, lwd = 1.5, border = "yellow")

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Define the number of quadrats along x and y directions
nx <- 5
ny <- 5

# Perform quadrat count
quadrat_counts <- quadratcount(acc_ppp, nx = nx, ny = ny)
quadrat_counts

# Plot the quadrat counts
plot(acc_ppp, pch = 20, cex = 0.5, main = "BMR 2022 Accidents - Quadrat Analysis")
plot(quadrat_counts, add = TRUE, col = "red")

# Chi-squared test for CSR
quadrat_test <- quadrat.test(acc_ppp, nx = nx, ny = ny)
quadrat_test

#
#
#
quadrat.test(acc_ppp, nx = 10, ny = 10)
#
#
#
#
# Compare a few standard bandwidth selectors
bw_diggle <- bw.diggle(acc_ppp)
bw_ppl    <- bw.ppl(acc_ppp)
bw_scott  <- bw.scott(acc_ppp)

bw_diggle
bw_ppl
bw_scott

# Plot density surfaces side by side using each bandwidth
par(mfrow = c(1, 3))
plot(density(acc_ppp, sigma = bw_diggle), main = paste("bw.diggle =", round(bw_diggle)))
plot(density(acc_ppp, sigma = bw_ppl), main = paste("bw.ppl =", round(bw_ppl)))
plot(density(acc_ppp, sigma = bw_scott[1]), main = paste("bw.scott"))
par(mfrow = c(1, 1))
#
#
#
#
plot(density(acc_ppp, sigma = bw_ppl), main = "bw.ppl")
#
#
#
#
#
#
#
#
pacman::p_load(terra)
#
#
#
# Convert the KDE to a raster for better plotting
acc_kde_bw_terra <- rast(acc_kde)
plot(acc_kde_bw_terra, main = "Cartographic Quality KDE of Accidents")
#
#
#
#
#
#
#
Bangkok <- bmr_adm_boundary %>%
  filter(adm1_name == "Bangkok") %>%  
  st_transform(crs = 4326)

Nonthaburi <- bmr_adm_boundary %>%
  filter(adm1_name == "Nonthaburi") %>%
  st_transform(crs = 4326)

Pathum_Thani <- bmr_adm_boundary %>%
  filter(adm1_name == "Pathum Thani") %>%
  st_transform(crs = 4326)

Samut_Prakan <- bmr_adm_boundary %>%
  filter(adm1_name == "Samut Prakan") %>%
  st_transform(crs = 4326)

Nakhon_Pathom <- bmr_adm_boundary %>%
  filter(adm1_name == "Nakhon Pathom") %>%
  st_transform(crs = 4326)

Samut_Sakhon <- bmr_adm_boundary %>%
  filter(adm1_name == "Samut Sakhon") %>%
  st_transform(crs = 4326)  

#
#
#
#

# Create owin objects for each province after projecting to the appropriate CRS
Bangkok_owin <- Bangkok %>%
  st_transform(crs = 32647) %>%
  as.owin()

Nonthaburi_owin <- Nonthaburi %>%
  st_transform(crs = 32647) %>%
  as.owin()
Pathum_Thani_owin <- Pathum_Thani %>%
  st_transform(crs = 32647) %>%
  as.owin()
Samut_Prakan_owin <- Samut_Prakan %>%
  st_transform(crs = 32647) %>%
  as.owin()
Nakhon_Pathom_owin <- Nakhon_Pathom %>%
  st_transform(crs = 32647) %>%
  as.owin()
Samut_Sakhon_owin <- Samut_Sakhon %>%
  st_transform(crs = 32647) %>%
  as.owin()
#
#
#
#
#
# Combine point events and study window for each province
Bangkok_ppp <- acc_ppp[Bangkok_owin]
Nonthaburi_ppp <- acc_ppp[Nonthaburi_owin]
Pathum_Thani_ppp <- acc_ppp[Pathum_Thani_owin]
Samut_Prakan_ppp <- acc_ppp[Samut_Prakan_owin]
Nakhon_Pathom_ppp <- acc_ppp[Nakhon_Pathom_owin]
Samut_Sakhon_ppp <- acc_ppp[Samut_Sakhon_owin]
#
#
#
#
#
# Convert the study window and distance to road to kilometers
Bangkok_owin <- spatstat.geom::rescale(Bangkok_owin, 1000, "km")
Nonthaburi_owin <- spatstat.geom::rescale(Nonthaburi_owin, 1000, "km")
Pathum_Thani_owin <- rescale(Pathum_Thani_owin, 1000, "km")
Samut_Prakan_owin <- rescale(Samut_Prakan_owin, 1000, "km")
Nakhon_Pathom_owin <- rescale(Nakhon_Pathom_owin, 1000, "km")
Samut_Sakhon_owin <- rescale(Samut_Sakhon_owin, 1000, "km")


#
#
#
#
#
#
# Convert roads to a psp (planar segment pattern) in the same window
roads_psp <- as.psp(st_geometry(bmr_primary_roads))

# Distance from every pixel in the study window to the nearest road segment
dist_to_road <- distfun(roads_psp)

# Visualize it - sanity check: near-zero (dark) along roads, increasing away from them
plot(dist_to_road, main = "Distance to Nearest Primary Road")
#
#
#
#
# Run the rhohat function
rho_result <- rhohat(acc_ppp, dist_to_road)
rho_result

# Plot the result
plot(rho_result, main = "Rhohat: Accidents vs Distance to Nearest Road")
#
#
#
#
#
#
#
