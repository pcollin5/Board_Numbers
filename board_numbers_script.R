packages <- c("tidyverse", "tidycensus", "leaflet", "mapview", "DT", "sf",
              "knitr", "rmarkdown", "kableExtra", "RColorBrewer", "tigris",
              "directlabels", "officer", "flextable", "zoo", "directlabels", "plotly")



lapply(packages, library, character.only = TRUE)


#### load color scheme ####

#### color vectors ####

UETHDA_Colors <- c(`orange` = "#ff5100", 
                   `dark grey` = "#808285",
                   `black` = "#000000",
                   `white` = "#ffffff",
                   `dark blue` = "#0b5394",
                   `light grey` = "#cccccc",
                   `cyan` = "00ffff")

uethda_cols <- function(...){
  cols <- c(...)
  
  if(is.null(cols))
    return(UETHDA_Colors)
  
  UETHDA_Colors[cols]
}

UETHDA_palettes <- list(
  `main` = uethda_cols("orange", "dark grey", "black"),
  
  `cool` = uethda_cols("orange", "cyan", "dark blue")
)

uethda_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- UETHDA_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_uethda <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- uethda_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("uethda_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#### yearly numbers #####

names(yearly)

yearly$Service = yearly$Service %>%
  recode("Self_Sufficiency" = "Self Sufficiency")

yearly$Secondary = yearly$Secondary %>%
  recode("Self_Sufficiency" = "Self Sufficiency")

  
totals_by_program_20182019 <- yearly %>%
  filter(Time_Period == "2018_2019")%>%
  group_by(Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

totals_by_program_20182019

totals_by_program_20182019 %>%
  kable() %>%
  kable_styling()

totals_by_program_20182019 %>%
  kbl(caption = "2018-2019 Program Total Activity Cost") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

totals_by_program_20192020 <- yearly %>%
  filter(Time_Period == "2019_2020")%>%
  group_by(Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

totals_by_program_20192020 %>%
  kbl(caption = "2019-2020 Program Total Activity Cost")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

yearly_totals_joined <- left_join(totals_by_program_20192020, totals_by_program_20182019, by = "Service")%>%
  rename("Service" = "Service", "2019-2020 Totals" = Totals.x, "2018-2019 Totals" = Totals.y)


yearly_totals_percent_increase <- yearly_totals_joined %>%
  mutate("Percent Increase" = round(100*((`2019-2020 Totals` - `2018-2019 Totals`)/ `2018-2019 Totals`),2))

yearly_totals_percent_increase %>%
  kbl(caption = "Percent Increase from 2018-2019 to 2019-2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")



secondary_service_2018_2019 <- yearly %>%
  filter(Time_Period == "2018_2019")%>%
  group_by(Service, Secondary)%>%
  summarise("Totals by Sub-Service" = sum(ActivityServiceCost))

secondary_service_2018_2019 %>%
  kbl(caption = "2018-2019 Program Total by Service")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

secondary_service_2019_2020 <- yearly %>%
  filter(Time_Period == "2019_2020")%>%
  group_by(Service, Secondary)%>%
  summarise("Totals by Sub-Service" = sum(ActivityServiceCost))

secondary_service_2019_2020 %>%
  kbl(caption = "2019-2020 Program Total by Service")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

secondary_joined <- full_join(secondary_service_2019_2020, secondary_service_2018_2019, by = "Secondary")

secondary_joined <- secondary_joined[,-4]

cleaned_secondary_joined <- secondary_joined %>%
  rename("Service" = "Service.x", "2019-2020 Totals by Sub-Service" = "Totals by Sub-Service.x",
         "2018-2019 Totals by Sub-Service" = "Totals by Sub-Service.y")

secondary_percent_increase <- cleaned_secondary_joined %>%
  mutate("Percent Increase" = round(100 * (`2019-2020 Totals by Sub-Service` - `2018-2019 Totals by Sub-Service`) / `2018-2019 Totals by Sub-Service`,2))

secondary_percent_increase %>%
  kbl(caption = "Percent Increase by Sub-Service from 2018-2019 to 2019-2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

county_totals_by_program_2018_2019 <- yearly %>%
  filter(Time_Period == "2018_2019")%>%
  group_by(County, Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

county_totals_by_program_2018_2019 %>%
  kbl(caption = "2018-2019 County Totals by Program")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
  

county_totals_by_program_2019_2020 <- yearly %>%
  filter(Time_Period == "2019_2020")%>%
  group_by(County, Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

county_totals_by_program_2019_2020 %>%
  kbl(caption = "2019-2020 County Totals by Program")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

county_joined <- full_join(county_totals_by_program_2019_2020, county_totals_by_program_2018_2019, by = c("County", "Service"))%>%
  rename("2019-2020 Totals" = "Totals.x", "2018-2019 Totals" = "Totals.y")

county_percent_increase <- county_joined %>%
  mutate("Percent Increase" = round(100 * (`2019-2020 Totals` - `2018-2019 Totals`) / `2018-2019 Totals`,2))

county_percent_increase %>%
  kbl(caption = "Percent Increase by Program from 2018-2019 to 2019-2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


#### from july-sept ####

july_sept_2019 <- yearly %>%
  filter(Time_Period == "07012019-09302019")

july_sept_2020 <- yearly %>%
  filter(Time_Period == "07012020-09302020")

totals_by_program_2019 <- yearly %>%
  filter(Time_Period == "07012019-09302019")%>%
  group_by(Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

totals_by_program_2019



totals_by_program_2019 %>%
  kbl(caption = "July 1 to September 30 2019 Program Total Activity Cost") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

totals_by_program_2020 <- yearly %>%
  filter(Time_Period == "07012020-09302020")%>%
  group_by(Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

totals_by_program_2020 %>%
  kbl(caption = "July 1 to September 30 2020 Program Total Activity Cost")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

quarterly_totals_joined <- left_join(totals_by_program_2020, totals_by_program_2019, by = "Service")%>%
  rename("Service" = "Service", "July 1 to September 30 2020 Totals" = Totals.x, "July 1 to September 30 2019 Totals" = Totals.y)


quarterly_totals_percent_increase <- yearly_totals_joined %>%
  mutate("Percent Increase" = round(100*((`July 1 to September 30 2020 Totals` - `July 1 to September 30 2019 Totals`)/ `July 1 to September 30 2019 Totals`),2))

quarterly_totals_percent_increase %>%
  kbl(caption = "Percent Increase from July 1 to September 30 to 2019-2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")



secondary_service_2019 <- yearly %>%
  filter(Time_Period == "07012019-09302019")%>%
  group_by(Service, Secondary)%>%
  summarise("Totals by Sub-Service" = sum(ActivityServiceCost))

secondary_service_2019 %>%
  kbl(caption = "July 1 to September 30 2019 Program Total by Service")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

secondary_service_2020 <- yearly %>%
  filter(Time_Period == "07012020-09302020")%>%
  group_by(Service, Secondary)%>%
  summarise("Totals by Sub-Service" = sum(ActivityServiceCost))

secondary_service_2020 %>%
  kbl(caption = "July 1 to September 30 2020 Program Total by Service")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

quarterly_secondary_joined <- full_join(secondary_service_2020, secondary_service_2019, by = "Secondary")

quarterly_secondary_joined <- quarterly_secondary_joined[,-4]

cleaned_quarterly_secondary_joined <- secondary_joined %>%
  rename("Service" = "Service.x", "July 1 to September 30 2020 Totals by Sub-Service" = "Totals by Sub-Service.x",
         "July 1 to September 30 2019 Totals by Sub-Service" = "Totals by Sub-Service.y")

quarterly_secondary_percent_increase <- cleaned_quarterly_secondary_joined %>%
  mutate("Percent Increase" = round(100 * (`July 1 to September 30 2020 Totals by Sub-Service` - `July 1 to September 30 2019 Totals by Sub-Service`) / `July 1 to September 30 2019 Totals by Sub-Service`,2))

quarterly_secondary_percent_increase %>%
  kbl(caption = "Percent Increase by Sub-Service from July 1 to September 30 2019 to 2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

county_totals_by_program_2019 <- yearly %>%
  filter(Time_Period == "07012019-09302019")%>%
  group_by(County, Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

county_totals_by_program_2019 %>%
  kbl(caption = "July 1 to September 30 2019 County Totals by Program")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")


county_totals_by_program_2020 <- yearly %>%
  filter(Time_Period == "07012020-09302020")%>%
  group_by(County, Service)%>%
  summarise("Totals" = sum(ActivityServiceCost))

county_totals_by_program_2020 %>%
  kbl(caption = "July 1 to September 30 2020 County Totals by Program")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

quarterly_county_joined <- full_join(county_totals_by_program_2020, county_totals_by_program_2019, by = c("County", "Service"))%>%
  rename("July 1 to September 30 2020 Totals" = "Totals.x", "July 1 to September 30 2019 Totals" = "Totals.y")

quarterly_county_percent_increase <- quarterly_county_joined %>%
  mutate("Percent Increase" = round(100 * (`July 1 to September 30 2020 Totals` - `July 1 to September 30 2019 Totals`) / `July 1 to September 30 2019 Totals`,2))

quarterly_county_percent_increase %>%
  kbl(caption = "Percent Increase by Program from July 1 to September 30 2019 to 2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

#### breakdown by activity service type ####

names(yearly)

totals_by_service_type_2018_2019 <- yearly %>%
  filter(Time_Period == "2018_2019")%>%
  group_by(Service, Secondary, ActivityServiceType)%>%
  summarise("Totals by Activity Service Type" = sum(ActivityServiceCost))

totals_by_service_type_2019_2020 <- yearly %>%
  filter(Time_Period == "2019_2020")%>%
  group_by(Service, Secondary, ActivityServiceType)%>%
  summarise("Totals by Activity Service Type" = sum(ActivityServiceCost))

totals_by_service_type_2019_2020

joined_service_type <- full_join(totals_by_service_type_2019_2020, totals_by_service_type_2018_2019, by = c("Service", "Secondary", "ActivityServiceType"))%>%
  rename("2019-2020 Totals by Activity Service Type" = "Totals by Activity Service Type.x",
         "2018-2019 Totals by Activity Service Type" = "Totals by Activity Service Type.y")

names(joined_service_type)

percent_change_service_type <- joined_service_type %>%
  mutate("Percent Change" = round(100 * (`2019-2020 Totals by Activity Service Type`- `2018-2019 Totals by Activity Service Type`) / `2018-2019 Totals by Activity Service Type`,2))

percent_change_service_type %>%
  kbl(caption = "Percent Change in Activity Service Type from 2018-2019 to 2019-2020")%>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
