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
pacman::p_load(readr, dplyr, lubridate, ggplot2, tmap, sf, spNetwork, spatstat, plotly, gtsummary, sparr, terra, tidyr)
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
#| eval: false
bmr_acc <- read_csv(
  "data/thai_road_accident_2019_2022.csv",
  show_col_types = FALSE
)
#
#
#
#
#
#| eval: false
null_counts <-   sum(is.na(bmr_acc))
null_counts
#
#
#
#
#
#
#
#
#
#| eval: false
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
#| eval: false
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
#| eval: false
null_counts_after <- sum(is.na(bmr_acc_sf))
null_counts_after
#
#
#
#
#
#
#
#
#
#| eval: false
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
#
#
#
#
#| echo: false
#| eval: false
# Removing duplicated records
bmr_acc_sf <- bmr_acc_sf[!duplicated(bmr_acc_sf$acc_code), ]
#
#
#
# Saving the cleaned and filtered road traffic accident data for the Bangkok Metropolitan Region in RDS format for future use.
#| echo: false
#| eval: false
#saveRDS(bmr_acc_sf, file = "data/bmr_road_traffic_accidents_2019-2022.rds")
#
#
#
bmr_acc_sf <- readRDS("data/bmr_road_traffic_accidents_2019-2022.rds")
#
#
#
#
#
#| eval: false
adm_boundary <- st_read(dsn = "../../data/takehome01/tha_admin2.shp", # layer 2 for district level boundaries
                layer = "tha_admin2") %>% 
  st_transform(crs = 32647)
#
#
#
#
#
#| eval: false
st_crs(adm_boundary) 
#
#
#
#
#
#
#
#| eval: false
glimpse(adm_boundary) # 
#
#
#
#
#
#
#
#| eval: false
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
saveRDS(bmr_adm_boundary, file = "data/bmr_adm_boundary.rds")
#
#
#
bmr_adm_boundary <- read_rds("data/bmr_adm_boundary.rds")
#
#
#
#
#
#| fig-width: 10
#| fig-height: 8
tmap_mode("plot")
tm_shape(bmr_adm_boundary) +
  tm_fill(col = "adm1_name", title = "Region") + 
  tm_text("adm1_name", size=0.6) +
  tm_borders() +
  tm_layout(main.title = "BMR Administrative Boundaries",
            main.title.position = "center",
            main.title.size = 3,
            #title.position = c("center", "top"),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.6
           )
#
#
#
#
#
#
#
#| eval: false
thailand_roads <- st_read(dsn = "data/hotosm_tha_roads_lines_shp.shp", layer = "hotosm_tha_roads_lines_shp") %>%
  st_set_crs(4326) %>% # Set the coordinate reference system to WGS 84 (EPSG:4326)
  st_transform(crs = 32647) # UTM Zone 47N (EPSG:32647) for Bangkok Metropolitan Region
#
#
#
#
#
#
#
#
#
#| eval: false
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
#| eval: false
bmr_roads <- bmr_roads_orig %>%
  st_cast("LINESTRING") # Convert MULTILINESTRING to LINESTRING geometries
#
#
#
#
#
#| eval: false
st_crs(bmr_roads) 
#
#
#
#
#
#
#
#
#
#| eval: false
glimpse(bmr_roads) 
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
#| eval: false
unique(bmr_roads$highway)
#
#
#
#
#
#
#
#
#
#| eval: false
hw_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified")

bmr_roads_filtered <- bmr_roads %>%
  select("highway") %>% 
  filter(highway %in% hw_types)
#
#
#
#| eval: false
glimpse(bmr_roads_filtered)
#
#
#
#
#
#
#
#
#
#| eval: false
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
#| fig-width: 10
#| fig-height: 8
#| echo: false
bmr_roads <- readRDS("data/bmr_primary_roads.rds")
tm_shape(bmr_roads) +
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
#| eval: false
# save the filtered primary road network data for the Bangkok Metropolitan Region (BMR) in RDS format for future use.
saveRDS(bmr_roads_filtered, file = "data/bmr_primary_roads.rds")
#
#
#
#| echo: false
bmr_primary_roads <- read_rds("data/bmr_primary_roads.rds")
#
#
#
#| echo: false
bmr_acc_sf <- read_rds("data/bmr_road_traffic_accidents_2019-2022.rds")

bmr_adm_boundary <- read_rds("data/bmr_adm_boundary.rds")
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
#| fig-width: 10
#| fig-height: 8
tmap_mode('plot')

tm_shape(bmr_adm_boundary) +
  tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=1, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.3, alpha = 0.5) +                                     
  tm_layout(main.title = "Road Traffic Accidents in Bangkok Metropolitan Region (2019 - 2022)",
            main.title.position = "center",
            main.title.size = 3,
            #title.position = c("center", "top"),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.6
            
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
#| fig-width: 10
#| fig-height: 8
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, alpha = 0.6) +                                                
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
#| fig-width: 10
#| fig-height: 8
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
# mutate month column
bmr_acc_sf <- bmr_acc_sf %>%
  mutate(month = lubridate::month(incident_datetime, label = TRUE)) 
#
#
#
#| fig-width: 10
#| fig-height: 8
# Visualization Geographic Distribution of Accidents by month
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7, alpha = 0.8) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, alpha = 0.6) +                                                
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
#
#| fig-width: 10
#| fig-height: 8
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
# mutate day of the week column
bmr_acc_sf <- bmr_acc_sf %>%
  mutate(day_of_week = lubridate::wday(incident_datetime, label = TRUE)) 
#
#
#
#| fig-width: 10
#| fig-height: 8
# Visualization Geographic Distribution of Accidents by day of the week
tm_shape(bmr_adm_boundary) +
  #tm_polygons(col='adm1_name', alpha=0.6, border.col="black", lwd=0.7, title = "Region") +
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, alpha = 0.6) +                                                
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
#| fig-width: 10
#| fig-height: 8
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
# mutate peak hour column
bmr_acc_sf <- bmr_acc_sf %>%
  #st_drop_geometry() %>%
  mutate(hour = lubridate::hour(incident_datetime)) %>%
  mutate(
    peak_hour = case_when(
      hour %in% 6:9 ~ "Morning Peak",
      hour %in% 16:19 ~ "Afternoon Peak",
      TRUE ~ "Off-Peak"
    )
  ) 
#
#
#
#| fig-width: 14
#| fig-height: 8
# Visualization Geographic Distribution of Accidents by peak hours
tmap_mode('plot')
tm_shape(bmr_adm_boundary) +
  
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, fill_alpha = 0.6) +                                                
  tm_facets(by = "peak_hour") +
  tm_title ("Road Traffic Accidents by Peak Hours in Bangkok Metropolitan Region (2019-2022)",
    position = c("center", "top"), 
    size = 1.5,
  )
#
#
#
#
#
# mutate peak hour column
bmr_acc_sf <- bmr_acc_sf %>%
  #st_drop_geometry() %>%
  mutate(daynight = lubridate::hour(incident_datetime)) %>%
  mutate(
    daynight = case_when(
      hour %in% 6:18 ~ "Day",
      TRUE ~ "Night"
    )
  ) 
#
#
#
#| fig-width: 12
#| fig-height: 8
# Visualization Geographic Distribution of Accidents by peak hours
tmap_mode('plot')
tm_shape(bmr_adm_boundary) +
  
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, fill_alpha = 0.6) +                                                
  tm_facets(by = "daynight") +
  tm_title ("Road Traffic Accidents by Day Night in Bangkok Metropolitan Region (2019-2022)",
            position = c("center", "top"), 
            size = 1.5,
  )
#
#
#
bmr_acc_sf <- bmr_acc_sf %>%
  # Extract the hour (0 to 23)
  mutate(hour = lubridate::hour(incident_datetime)) %>%
  # Categorize into 4-hour blocks
  mutate(
    time_block = case_when(
      hour %in% 0:3   ~ "00:00 - 03:59",
      hour %in% 4:7   ~ "04:00 - 07:59",
      hour %in% 8:11  ~ "08:00 - 11:59",
      hour %in% 12:15 ~ "12:00 - 15:59",
      hour %in% 16:19 ~ "16:00 - 19:59",
      hour %in% 20:23 ~ "20:00 - 23:59"
    ),
    # Convert to an ordered factor so plots display them in the correct time order
    time_block = factor(time_block, levels = c(
      "00:00 - 03:59", 
      "04:00 - 07:59", 
      "08:00 - 11:59", 
      "12:00 - 15:59", 
      "16:00 - 19:59", 
      "20:00 - 23:59"
    ))
  )
#
#
#
names(bmr_acc_sf)
#
#
#
#| fig-width: 16
#| fig-height: 8
# Visualization Geographic Distribution of Accidents by peak hours
tmap_mode('plot')
tm_shape(bmr_adm_boundary) +
  
  tm_shape(bmr_primary_roads) +
  tm_lines(col = "darkgreen", lwd=0.7) +
  tm_shape(bmr_acc_sf) + 
  tm_dots(col = "red", size = 0.25, fill_alpha = 0.6) +                                                
  tm_facets(by = "time_block") +
  tm_title ("Road Traffic Accidents by 4h block in Bangkok Metropolitan Region (2019-2022)",
            position = c("center", "top"), 
            size = 1.5,
  )
#
#
#
#
#
#
#
#| fig-width: 10
#| fig-height: 8

# Build owin from the BMR union, and convert to ppp
bmr_owin <- as.owin(st_union(bmr_adm_boundary))
acc_ppp  <- as.ppp(st_coordinates(bmr_acc_sf), W = bmr_owin)

#acc_ppp        # summary: check n, window area, units

#
#
#
plot(acc_ppp, main = "BMR 2019 - 2022 Accidents")
#
#
#
#
#
#
#
#
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
#| fig-width: 10
#| fig-height: 8
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
#| fig-width: 10
#| fig-height: 8
# Define the number of quadrats along x and y directions
nx <- 10
ny <- 10

# Perform quadrat count
quadrat_counts <- quadratcount(acc_ppp, nx = nx, ny = ny)
quadrat_counts

# Plot the quadrat counts
plot(acc_ppp, pch = 20, cex = 0.5, main = "BMR 2022 Accidents - Quadrat Analysis")
plot(quadrat_counts, add = TRUE, col = "red")



#
#
#
# Chi-squared test for CSR
quadrat_test <- quadrat.test(acc_ppp, nx = nx, ny = ny)
quadrat_test
#
#
#
#
#
#| fig-width: 12
#| fig-height: 6
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
#| fig-width: 10
#| fig-height: 8
plot(density(acc_ppp, sigma = bw_scott), main = "bw.scott")
#
#
#
#
#
#
#
#| fig-width: 10
#| fig-height: 8
# Convert the KDE to a raster for better plotting
acc_kde_bw_terra <- rast(acc_kde)
plot(acc_kde_bw_terra, main = "Cartographic Quality KDE of Accidents")
#
#
#
#
#
#| echo: false
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
# use spatstat.geom::rescale to convert units from meters to kilometers
# we cannot use rescale as ggplot2 also has a function named rescale, which could cause conflicts

Bangkok_ppp <- spatstat.geom::rescale(Bangkok_ppp, 1000, "km")
Nonthaburi_ppp <- spatstat.geom::rescale(Nonthaburi_ppp, 1000, "km")
Pathum_Thani_ppp <- spatstat.geom::rescale(Pathum_Thani_ppp, 1000, "km")
Samut_Prakan_ppp <- spatstat.geom::rescale(Samut_Prakan_ppp, 1000, "km")
Nakhon_Pathom_ppp <- spatstat.geom::rescale(Nakhon_Pathom_ppp, 1000, "km")
Samut_Sakhon_ppp <- spatstat.geom::rescale(Samut_Sakhon_ppp, 1000, "km")


#
#
#
#
#
#| fig-width: 16
#| fig-height: 8
par(mfrow = c(2, 3))
plot(Bangkok_ppp, main = "Bangkok")
plot(Nonthaburi_ppp, main = "Nonthaburi")
plot(Pathum_Thani_ppp, main = "Pathum Thani")
plot(Samut_Prakan_ppp, main = "Samut Prakan")
plot(Nakhon_Pathom_ppp, main = "Nakhon Pathom")
plot(Samut_Sakhon_ppp, main = "Samut Sakhon")
par(mfrow = c(1, 1))
#
#
#
#
#

clark_evans_bangkok <- clarkevans.test(Bangkok_ppp, correction = "none", clipregion = Bangkok_owen, alternative = "two.sided", nsim = 999)
clark_evans_nonthaburi <- clarkevans.test(Nonthaburi_ppp, correction = "none", clipregion = Nonthaburi_owen, alternative = "two.sided", nsim = 999)
clark_evans_pathum_thani <- clarkevans.test(Pathum_Thani_ppp, correction = "none", clipregion = Pathum_Thani_owen, alternative = "two.sided", nsim = 999)
clark_evans_samut_prakan <- clarkevans.test(Samut_Prakan_ppp, correction = "none", clipregion = Samut_Prakan_owen, alternative = "two.sided", nsim = 999)
clark_evans_nakhon_pathom <- clarkevans.test(Nakhon_Pathom_ppp, correction = "none", clipregion = Nakhon_Pathom_owen, alternative = "two.sided", nsim = 999)
clark_evans_samut_sakhon <- clarkevans.test(Samut_Sakhon_ppp, correction = "none", clipregion = Samut_Sakhon_owen, alternative = "two.sided", nsim = 999)

clark_evans_bangkok
clark_evans_nonthaburi
clark_evans_pathum_thani
clark_evans_samut_prakan
clark_evans_nakhon_pathom
clark_evans_samut_sakhon
#
#
#
#
#
#| fig-width: 16
#| fig-height: 8

bw_bangkok <- bw.diggle(Bangkok_ppp)
bw_nonthaburi <- bw.diggle(Nonthaburi_ppp)
bw_pathum_thani <- bw.diggle(Pathum_Thani_ppp)
bw_samut_prakan <- bw.diggle(Samut_Prakan_ppp)
bw_nakhon_pathom <- bw.diggle(Nakhon_Pathom_ppp)
bw_samut_sakhon <- bw.diggle(Samut_Sakhon_ppp)

kde_bangkok <- density(Bangkok_ppp, sigma = bw_bangkok)
kde_nonthaburi <- density(Nonthaburi_ppp, sigma = bw_nonthaburi)
kde_pathum_thani <- density(Pathum_Thani_ppp, sigma = bw_pathum_thani)
kde_samut_prakan <- density(Samut_Prakan_ppp, sigma = bw_samut_prakan)
kde_nakhon_pathom <- density(Nakhon_Pathom_ppp, sigma = bw_nakhon_pathom)
kde_samut_sakhon <- density(Samut_Sakhon_ppp, sigma = bw_samut_sakhon)

par(mfrow = c(2, 3))
plot(kde_bangkok, main = "Bangkok KDE")
plot(kde_nonthaburi, main = "Nonthaburi KDE")
plot(kde_pathum_thani, main = "Pathum Thani KDE")
plot(kde_samut_prakan, main = "Samut Prakan KDE")
plot(kde_nakhon_pathom, main = "Nakhon Pathom KDE")
plot(kde_samut_sakhon, main = "Samut Sakhon KDE")
par(mfrow = c(1, 1))
#
#
#
#
#
#
#
#
#
# ---- G-function ----
G_acc <- Gest(acc_ppp, correction = "border")
plot(G_acc, xlim = c(0, 500))
#
#
#
#
#
#| eval: false
G_acc.csr <- envelope(acc_ppp, Gest, nsim = 99, rank = 1, global = FALSE)
plot(G_acc.csr)
#
#
#
#
#
#
#
#
#
# ---- F-function ----
F_acc <- Fest(acc_ppp)
plot(F_acc)
#
#
#
#
#
#
#
#| eval: false
F_acc.csr <- envelope(acc_ppp, Fest, nsim = 99, rank = 1, global = FALSE)
plot(F_acc.csr)
#
#
#
#
#
#
#
#
#
#| eval: false
# ---- K-function ----
K_acc <- Kest(acc_ppp, correction = "Ripley")
plot(K_acc, . - r ~ r, ylab = "K(d)-r", xlab = "d(m)")
#
#
#
#
#
#
#
#| eval: false
K_acc.csr <- envelope(acc_ppp, Kest, nsim = 99, rank = 1, global = FALSE)
plot(K_acc.csr, . - r ~ r, xlab = "d", ylab = "K(d)-r")
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
#| eval: false
bmr_acc_2022 <- bmr_acc_sf %>%
  filter(year(incident_datetime) == 2022)

acc_2022_ppp  <- as.ppp(st_coordinates(bmr_acc_2022), W = bmr_owin)

K_acc_2022 <- Kest(acc_2022_ppp, correction = "Ripley")
#
#
#
#| eval: false
plot(K_acc_2022, . - r ~ r, ylab = "K(d)-r", xlab = "d(m)")
#
#
#
#
#
#| eval: false
K_acc_2022.csr <- envelope(acc_2022_ppp, Kest, nsim = 99, rank = 1, global = FALSE)

plot(K_acc_2022.csr, . - r ~ r, xlab = "d", ylab = "K(d)-r")
#
#
#
#
#
#
#
#| eval: false
# ---- L-function ----
L_acc <- Lest(acc_ppp, correction = "Ripley")
plot(L_acc, . - r ~ r, ylab = "L(d)-r", xlab = "d(m)")
#
#
#
#
#
#
#
#| eval: false
L_acc.csr <- envelope(acc_ppp, Lest, nsim = 99, rank = 1, global = FALSE)
plot(L_acc.csr, . - r ~ r, xlab = "d", ylab = "L(d)-r")

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
bmr_prov <- c("Bangkok", "Nonthaburi", "Pathum Thani",
              "Samut Prakan", "Nakhon Pathom", "Samut Sakhon")

 
bmr_district <- st_read("data/takehome01/tha_admin2.shp", quiet = TRUE) %>%
  st_transform(32647) %>%                      # projected CRS -> area in metres
  filter(adm1_name %in% bmr_prov) %>%
  select(adm1_name, adm2_name, adm2_pcode) %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6)
 
acc_joined <- st_join(bmr_acc_sf, bmr_district, join = st_within) %>%
  filter(!is.na(adm2_pcode))


#
#
#
#
#
district_density <- bmr_district %>%
  left_join(acc_joined %>% st_drop_geometry() %>% count(adm2_pcode, name = "n_acc"),
            by = "adm2_pcode") %>%
  mutate(n_acc            = replace_na(n_acc, 0),
         acc_per_km2      = n_acc / area_km2) %>%
  arrange(desc(acc_per_km2))
#
#
#
#
#
#
#| fig-width: 16
#| fig-height: 8
#| fig-cap: "District-level accident densities per km²"

basemap <- tm_shape(district_density) +
  tm_polygons() +
  tm_text("adm1_name", size=0.5) +
  tm_layout(legend.position = c("left", "bottom"))+ 

gdppc <- qtm(district_density, "acc_per_km2") +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.text.size = 0.5,
    legend.title.size = 0.7) +
  tm_layout(legend.position = c("left", "bottom"))
tmap_arrange(basemap, gdppc, asp=1, ncol=2)
#
#
#
#
district_density_high <- district_density %>%
  filter(acc_per_km2 >= 5) %>%
  arrange(desc(acc_per_km2))

district_density_high


list(district_density_high = district_density_high)

# 1. District level ------------------------------------------------------
district_density <- bmr_district %>%
  left_join(acc_joined %>% st_drop_geometry() %>% count(adm2_pcode, name = "n_acc"),
            by = "adm2_pcode") %>%
  mutate(n_acc            = replace_na(n_acc, 0),
         acc_per_km2      = n_acc / area_km2,
         acc_per_km2_year = acc_per_km2 / n_years) %>%
  arrange(desc(acc_per_km2))
 
# 2. Province level ------------------------------------------------------
province_density <- district_density %>%
  st_drop_geometry() %>%
  group_by(adm1_name) %>%
  summarise(n_acc = sum(n_acc), area_km2 = sum(area_km2), .groups = "drop") %>%
  mutate(acc_per_km2      = n_acc / area_km2,
         acc_per_km2_year = acc_per_km2 / n_years) %>%
  arrange(desc(acc_per_km2))
 
# 3. Whole region --------------------------------------------------------
region_density <- province_density %>%
  summarise(n_acc = sum(n_acc), area_km2 = sum(area_km2)) %>%
  mutate(acc_per_km2 = n_acc / area_km2, acc_per_km2_year = acc_per_km2 / n_years)

#
#
#
# Code to generate district choropleths and scatter plot
# Example (replace with actual code):
# ggplot(district_data) +
#   geom_sf(aes(fill = accidents_per_km2)) +
#   theme_minimal()

# ggplot(district_data) +
#   geom_sf(aes(fill = accidents_per_road_km)) +
#   theme_minimal()

# ggplot(district_data, aes(x = road_length, y = accidents)) +
#   geom_point() +
#   theme_minimal()
#
#
#
plot(bmr_adm_boundary)
#
#
#
