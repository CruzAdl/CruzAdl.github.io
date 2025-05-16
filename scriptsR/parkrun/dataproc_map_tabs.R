# parkrun_data_processing.R

# Load necessary packages
librarian::shelf(
  dplyr,data.table,
  lubridate, stringr, 
  sf, 
  mapgl, ggplot2,plotly,flextable,htmltools,viridis, RColorBrewer
  )

# FUNCTIONS

# Make total minutes and total seconds for compute
convertTime <- function(timeStr) {
  minutesAndSeconds <- strsplit(timeStr, ":")
  totalMinutes <- sapply(minutesAndSeconds, function(x) {
    minutes <- as.numeric(x[1])
    seconds <- as.numeric(x[2])
    round(minutes + seconds / 60, 2)
  })
  
  totalMinutes
}

# Convert seconds to hh:mm:ss format
to_hhmmss <- function(seconds) {
  total_seconds <- round(seconds)
  hours <- total_seconds %/% 3600
  mins <- (total_seconds %% 3600) %/% 60
  secs <- total_seconds %% 60
  if (hours > 0) {
    sprintf("%02d:%02d:%02d", hours, mins, secs)
  } else {
    sprintf("%02d:%02d", mins, secs)
  }
}

# Generate HTML row for popup table
generate_html_row <- function(Date, Time, Position, AgeGrade) {
  paste0("<tr>",
         "<td style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>", Date, "</td>",
         "<td style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>", Time, "</td>",
         "<td style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>", Position, "</td>",
         "<td style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>", AgeGrade, "%</td>",
         "</tr>")
}

# Assemble HTML table from rows
assemble_html_table <- function(html_rows) {
  if (length(html_rows) == 0) return(NA)
  header <- "<tr>
               <th style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>Date</th>
               <th style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>Time</th>
               <th style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>Position</th>
               <th style='border: 1px solid black; text-align: center; padding: 5px; color: black;'>Age Grade</th>
             </tr>"
  paste0("<table style='border-collapse: collapse;'>", header, html_rows, "</table>")
}

# FROM RAW TO MAP AND DISPLAY DATA

# LOCS FROM JSON FILE IN OTHER SCRIPT
locs <- readRDS("rawdata/event_locations.RDS")
home <- locs[locs$Event=="Lordship Recreation Ground",] %>% st_transform(.,27700)

# BOROUGH DATA
borough_pol <- st_read("rawdata/boroughs_LON/London_Boroughs.shp", quiet = TRUE) %>%
  rename(Borough = DISTRICT) %>%
  st_transform(4326)

# FORMAT DATA - JOIN WITH LOCS
all_parkruns <- read.csv("rawdata/raw_results.csv") %>% 
  rowwise() %>% 
  dplyr::mutate(
    Date = as_date(Date, format="%d/%m/%Y"),Run.Number=NULL,
    inMinutes = convertTime(Time),
    inSeconds = as.numeric(substr(Time, 1, 2)) * 60 + as.numeric(substr(Time, 4, 5)),
    PB = if_else(PB=="PB",1,0),
    Age.Grade=as.numeric(str_remove(Age.Grade,"%"))) %>% 
  dplyr::full_join(., locs, "Event") %>%
  dplyr::select(Event,Date,Time,inMinutes,inSeconds,Position,Age.Grade,x,y) %>% 
  mutate(Event=as.factor(Event)) %>%
  ungroup()

# 1. Race data (more for teh timeline)
borough_race_pts <- st_intersection(
  st_as_sf(all_parkruns, coords=c("x","y"), crs=4326), 
  borough_pol
) %>% filter(!is.na(Date))

# 2. Boroughs with T/F (basic completed not completed boroughs)
borough_pol <- borough_pol %>% 
  mutate(Complete=ifelse(Borough%in%borough_race_pts$Borough,1, 0)) %>% 
  mutate(BoroughLab=paste0("<p style='color: black;'>", Borough, "</p>"))
  

# 3. Borough park data (more for map)
borough_prk_pts <-full_join(
  distinct(st_join(locs, borough_pol["Borough"], left = FALSE) %>% 
             mutate(Complete=ifelse(Event%in%borough_race_pts$Event,1, 0)) %>%
             select(Event, Borough, Complete)),
  distinct(borough_race_pts %>% arrange(Date) %>%
             group_by(Event) %>%
             mutate(n = n(), n = ifelse(is.na(Date),0,n),
                    html_row = generate_html_row(Date, Time, Position, Age.Grade),
                    html_rows = paste(html_row, collapse=""), 
                    inSeconds = median(inSeconds),
                    inMinutes=median(inMinutes),
                    Position=median(Position),
                    Age.Grade=median(Age.Grade),
                    html_table = ifelse(!is.na(n), assemble_html_table(html_rows), "TBR"),
                    html_row=NULL, html_rows=NULL, Time=NULL, Date=NULL) %>% 
             ungroup(.) %>% st_drop_geometry(.))
  ) %>%
  mutate(
    n=ifelse(is.na(n),0,n),
    html_table=ifelse(is.na(html_table),"TBR",html_table)
    ) %>%
  mutate(EventLab=paste0("<p style='color: black;'>", Event, "</p>"))

# could save
# my_parkruns.csv
# my_parkruns_mapdata.csv

# PREP TABLE + MAP

map <- maplibre(style = carto_style("voyager")) |> 
  fit_bounds(borough_pol, animate = FALSE) |> 
  add_fill_layer(id = "boroughs",
                 source = borough_pol,
                 # should be green if true with match expr
                 fill_color = match_expr(
                   column = "Complete",
                   values = c(1,0),
                   stops = c("lightgreen","transparent"),
                   default = "black"
                 ),
                 fill_outline_color = "black",
                 fill_opacity = 0.5,
                 tooltip = "BoroughLab",
                 before_id="building") |>
  add_circle_layer(id="parkruns",
                   source=borough_prk_pts,
                   circle_color=match_expr(
                     column = "Complete",
                     values = c(1,0),
                     stops = c("green","#b10000"),
                     default = "black"
                   ),
                   circle_radius = 6,
                   circle_opacity=0.8,
                   tooltip = "EventLab",
                   popup="html_table",
                   before_id="building")

# Prepare data for timeline plot
timeline_data <- borough_race_pts
ymaxred = max(timeline_data$inMinutes)

dfbigflu <- data.frame(
  xmini = as.Date("2019-12-31"),
  xmaxi = as.Date("2021-05-01"),
  ymini = 17.5,
  ymaxi = ymaxred,
  lab="Covid-19: Parkrun cancelled"
)

dfplantfas <- data.frame(
  xmini = as.Date("2024-10-25"),
  xmaxi = as.Date("2025-05-05"),
  ymini = 17.5,
  ymaxi = ymaxred,
  lab = "Atypical Plantar Fasciitis"
)

# Load colorblind-friendly palette
event_colors <- c(colorRampPalette(
  brewer.pal(min(length(unique(timeline_data$Event)), 12), "Paired")
  )(length(unique(timeline_data$Event))),"orange", "red")

# Create the timeline plot
timeline_event <- 
  ggplot(data=timeline_data, aes(x = Date, y = inMinutes, fill = Event, time=Time)) +
  geom_rect(data = dfplantfas, mapping=aes(xmin = xmini, xmax = xmaxi, ymin = ymini, ymax = ymaxi, fill=lab), 
            alpha = 0.3, color=NA, inherit.aes = F) +
  geom_rect(data = dfbigflu, mapping=aes(xmin = xmini, xmax = xmaxi, ymin = ymini, ymax = ymaxi, fill=lab), 
            alpha = 0.3, color=NA, inherit.aes = F) +
  geom_point(size = 3, shape=21, stroke=0.3) +
  scale_fill_manual(values = event_colors) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Minutes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to interactive plot
timeline_evently <- ggplotly(timeline_event, tooltip = c("Date", "Event", "Time")) %>%
  plotly::style(showlegend = FALSE, traces = c(1,2))

table_dta <- sf::st_drop_geometry(timeline_data)

# Prepare data for summaries
total_summary <- data.frame(
  Events = nrow(table_dta),
  Kilometers = nrow(table_dta) * 5,
  TotalTime = to_hhmmss(sum(table_dta$inSeconds)),
  MedianTime = to_hhmmss(median(table_dta$inSeconds))
)

time_by_year <- table_dta %>%
  group_by(Year = year(Date)) %>%
  summarise(Events = n(),
            MedianTime = to_hhmmss(median(inSeconds))) %>%
  arrange(desc(Year))

time_by_event <- table_dta %>%
  group_by(Event) %>%
  summarise(Events = n(),
            MedianTime = to_hhmmss(median(inSeconds))) %>%
  arrange(desc(Events)) 

# Create flextables
tbe <- flextable(time_by_event) %>%
  theme_vanilla() %>%
  set_caption("Times by Event") %>%
  color(color = "white", part = "all") 

tby <- flextable(time_by_year) %>%
  theme_vanilla() %>%
  set_caption("Times by Year") %>%
  color(color = "white", part = "all")

# Prepare the full results table
ft <- table_dta %>%
  arrange(desc(Date)) %>%
  mutate(Rank = rank(inMinutes, ties.method = "first"),
         Percentile = round(percent_rank(inMinutes) * 100)) %>%
  select(Date, Time, Event, Age.Grade, Position, inMinutes, inSeconds, Rank, Percentile)

ft <- flextable(ft) %>%
  theme_vanilla() %>%
  set_caption("All Resultsh") %>%
  color(color = "white", part = "header") %>%
  bg(i = ~Percentile <= 33, bg = "darkred", part = "body") %>%
  color(i = ~Percentile <= 66, color = "white", part = "body") %>% 
  bg(i = ~Percentile >= 66, bg = "lightblue", part = "body") %>%
  color(i = ~Percentile >= 66, color = "black", part = "body") %>%
  autofit()

