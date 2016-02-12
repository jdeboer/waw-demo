# Which package should you use?

# Available through CRAN
# ======================
install.packages("RGoogleAnalytics")
library(RGoogleAnalytics)
# library(RGoogleAnalyticsPremium) # Alternative version for GA Premium.
# library(bigrquery) # CRAN version does not work; use GitHub version instead.

# Available through GitHub
# ========================
devtools::install_github("MarkEdmondson1234/googleAnalyticsR_public")
library(googleAnalyticsR)
devtools::install_github("andrewgeisler/GAR")
library(GAR) # Authentication does not seem to work.
devtools::install_github("jdeboer/ganalytics")
library(ganalytics)
devtools::install_github("rstats-db/bigrquery")
library(bigrquery)

# Two packages with the same name by two different people
# In Windows OS, these will overwrite each other.
install.packages("RGA")
library(RGA)
devtools::install_github("skardhamar/rga")
library(rga)

# To saves lots of retyping, let's define some parameters we'll be using.
user_name <- "you@domain.com"
client_id <- "144394141628-8m5i5icva7akegi3tp6215d9eg9o5cln.apps.googleusercontent.com"
client_secret <- "wlFmhluHqTdZw6UG22h5A2nr"
view_id <- "ga:106134121"
start_date <- "2016-01-01"
end_date <- "2016-01-31"

# Some other packages we'll be using for this demonstration
install.packages(c("tidyr", "ggplot2", "dplyr", "lubridate"))
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)

# A function we'll reuse to plot the data from each GA package being compared.
ga_plot <- function(ga_data) {
  ga_data <- complete(
    ga_data, dayOfWeek, hour, deviceCategory,
    fill = list(sessions = 0, bounces = 0)
  )
  ga_data <- transform(
    ga_data,
    bounceRate = (bounces + 1) / (sessions + 1)
  )
  ggplot(ga_data) + 
    aes(dayOfWeek, hour, fill = bounceRate) +
    geom_tile() + facet_wrap(~deviceCategory)
}

### RGA ###
# https://github.com/artemklevtsov/RGA
#
token <- authorize(
  username = user_name,
  client.id = client_id,
  client.secret = client_secret
)
ga_data <- get_ga(
  profileId = view_id,
  start.date = start_date,
  end.date = end_date,
  metrics = "ga:sessions,ga:bounces",
  dimensions = "ga:dayOfWeek,ga:hour,ga:deviceCategory",
  sort = "ga:dayOfWeek,ga:hour",
  token = token
)
head(ga_data)
summary(ga_data)
ga_plot(ga_data)
detach("package:RGA", unload = TRUE)

### RGoogleAnalytics and RGoogleAnalyticsPremium ###
# https://github.com/Tatvic/RGoogleAnalytics
# https://github.com/Tatvic/RGoogleAnalyticsPremium
#
token <- Auth(
  client.id = client_id,
  client.secret =  client_secret
)
query_list <- Init(
  start.date = start_date,
  end.date = end_date,
  dimensions = "ga:dayOfWeek,ga:hour,ga:deviceCategory",
  metrics = "ga:sessions,ga:bounces",
  sort = "ga:dayOfWeek,ga:hour",
  table.id = view_id
)
ga_query <- QueryBuilder(query_list)
ga_data <- GetReportData(ga_query, token)
head(ga_data)
summary(ga_data)
ga_plot(ga_data)

### bigrquery ###
# https://github.com/rstats-db/bigrquery
sql <- "
  SELECT device.isMobile AS isMobile,
         hits.hour AS hour,
         date,
         SUM ( totals.visits ) AS sessions,
         SUM ( totals.bounces ) AS bounces
    FROM [google.com:analytics-bigquery:LondonCycleHelmet.ga_sessions_20130910]
    GROUP BY isMobile, hour, date
    ORDER BY date, hour;
  "
ga_data <- query_exec(sql, project = "960582788010")
head(ga_data)
ga_data <- ga_data %>%
  mutate(
    isTablet = FALSE,
    deviceCategory = c("Desktop", "Mobile", "Tablet")[isMobile + isTablet + 1][1],
    dayOfWeek = wday(ymd(date)) - 1
  )
ga_data <- ga_data %>% select(dayOfWeek, hour, deviceCategory, sessions, bounces)
summary(ga_data)
ga_plot(ga_data)

### rga ###
# https://github.com/skardhamar/rga
rga.open(instance = "ga", where = "rga_auth.RData")
ga_data <- ga$getData(
  view_id, start.date = start_date, end.date = end_date, 
  metrics = "ga:sessions,ga:bounces",
  dimensions = "ga:dayOfWeek,ga:hour,ga:deviceCategory", 
  sort = "ga:dayOfWeek,ga:hour"
)
head(ga_data)
summary(ga_data)
ga_plot(ga_data)

# googleAnalyticsR
# https://github.com/MarkEdmondson1234/googleAnalyticsR_public
ga_auth()
ga_data <- google_analytics(
  id = view_id, 
  start = start_date, end = end_date, 
  metrics = c("sessions", "bounces"), 
  dimensions = c("dayOfWeek", "hour", "deviceCategory"),
  sort = c("ga:dayOfWeek,ga:hour")
)
head(ga_data)
summary(ga_data)
ga_plot(ga_data)

# ganalytics
# https://github.com/jdeboer/ganalytics
GoogleApiCreds(userName = user_name, appCreds = "client_secret.json")
query <- GaQuery(
  view = view_id,
  startDate = start_date, endDate = end_date,
  metrics = c("sessions", "bounces"),
  dimensions = c("dayOfWeek", "hour", "deviceCategory"),
  sortBy = c("dayOfWeek", "hour")
)
ga_data <- GetGaData(query)
head(ga_data)
summary(ga_data)
ga_plot(ga_data)

### ganalytics examples ###
# Installation and "hello world!"
devtools::install_github("jdeboer/ganalytics")

library(ganalytics)

creds <- GoogleApiCreds(
  "you@domain.com", # Your Google username
  "client_secret.json" # From Google APIs console
)

# Default view of first account and property
view_id <- NA

query <- GaQuery(view_id, creds)

GetGaData(query)

# Example 1
q <- GaQuery(view_id, creds)

DateRange(q) <- c("2015-10-27", "2015-11-26")
Dimensions(q) <- c("userGender", "deviceCategory", "channelGroup")
Metrics(q) <- c("users", "sessions", "pageviews")

GetGaData(q)

# Example 2
played_video <-
  Expr(~EventCategory == "Video") &
  Expr(~EventAction == "Play")

purchased_ticket <-
  PerHit(
    Expr(~goal1completions > 0)
  )

journey <- PerUser(Sequence(
  played_video,
  Then(purchased_ticket)
))

cat(as(journey, "character"))

# Example 3
query <- GaQuery(
  view = view_id,
  startDate = "2015-01-01", endDate = "2015-01-31",
  metrics = c("users", "sessions", "pageviews"),
  dimensions = c("deviceCategory", "dayOfWeekName"),
  filter = Expr(~deviceCategory %matches% "mobile|tablet"),
  segment = Expr(~country == "Australia")
)

response <- GetGaData(query)

# Example 4
e1 <- Expr(~keyword %starts_with% 'buy')
e2 <- Expr(~city %matches% '^(Sydney|Melbourne)$')
e3 <- Expr(~deviceCategory == 'tablet')

e4 <- e1 & (e2 | e3)

as(e4, 'character')

# Example 5
# Average number of visits per hour and day – split by desktop, mobile and tablet
query <- GaQuery(view_id)

DateRange(query) <- c("2015-01-01", "2015-12-31")

Dimensions(query) <- c(
  "deviceCategory", "dayOfWeekName", "hour", "date"
)

Metrics(query) <- "sessions"

MaxResults(query) <- 30000

data <- GetGaData(query)

library(dplyr)

weekly_data <- tbl_df(data) %>%
  group_by(deviceCategory, dayOfWeekName, hour) %>%
  summarise(avg_sessions_per_day = mean(sessions))

library(ggplot2)

ggplot(weekly_data) +
  aes(
    x = hour,
    y = avg_sessions_per_day,
    fill = deviceCategory,
    group = deviceCategory
  ) +
  geom_area(position = "stack") +
  facet_wrap(~dayOfWeekName)


# Example 6
# Define sequences to segment users by permutations of device type and compare against total users of each type of device.
Dimensions(query) <- NULL

Metrics(query) <- c("users")

DateRange(query) <- c("2015-01-01", "2015-03-31")
# Maximum of 90 days for user-based segmentation

devices <- list(
  desktop = Expr(~deviceCategory == "desktop"),
  mobile = Expr(~deviceCategory == "mobile"),
  tablet = Expr(~deviceCategory == "tablet")
)

device_sequences <- lapply(devices, function(from) {
  lapply(devices, function(to) {
    SegmentFilters(
      Sequence(First(from), Then(to)),
      scope = "users"
    )
  })
})

data <- lapply(seq_along(device_sequences), function(from_index){
  from_name <- names(device_sequences)[from_index]
  from_seq <- device_sequences[[from_index]]
  lapply(seq_along(from_seq), function(to_index){
    to_name <- names(from_seq)[to_index]
    Segment(query) <- from_seq[[to_index]]
    df <- GetGaData(query)
    df <- cbind(from = from_name, to = to_name, df)
  })
})

data <- unlist(data, recursive = FALSE)
data <- do.call(rbind, data)

library(dplyr)

benchmarks <- data %>%
  subset(from == to) %>%
  select(from, benchmark = users)

data <- data %>%
  subset(from != to) %>%
  inner_join(benchmarks, by = "from") %>%
  group_by(from, to) %>%
  summarise(transitioned = users / benchmark)

library(ggplot2)
library(scales)

ggplot(data) + 
  aes(x = from, y = to, fill = transitioned) +
  geom_tile() +
  scale_fill_continuous(labels = percent) +
  theme_minimal(base_size = 18) +
  theme(axis.title.y = element_text(angle = 0))

