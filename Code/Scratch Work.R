# Extra JB Work
setdiff(
  "pacman",
  rownames(installed.packages())
) |>
  install.packages()

#' Load (And Install if Necessary) Required Packages
pacman::p_load(
  CASdatasets,
  tidyverse,
  tidylog
)
# ```{r development_plot}
#' Sample `mtcars` scatterplot with groups
data(usworkcomptri8807)

usworkcomptri8807 |>
  as.data.frame() |>
  rownames_to_column(var = "accident_yr") |>
  as_tibble() |>
  pivot_longer(-accident_yr,
               names_to = "development_yr",
               values_to = "cum_loss") |>
  arrange(accident_yr, development_yr) |>
  group_by(accident_yr) |>
  mutate(increm_loss = cum_loss - lag(cum_loss),
         increm_loss = if_else(is.na(increm_loss), cum_loss, increm_loss)) |>
  ggplot(aes(x = cum_loss, y = increm_loss, group = development_yr)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black") +
  theme_minimal() +
  labs(
    x = "Cumulative Loss",
    y = "Incremental Loss",
    group = "# of Cylinders",
    title = "Example Plot for Presentation B"
  )

#' #' Save to Image Folder
#' ggsave(
#'   paste0(
#'     path_png,
#'     "example_plot_c.png"
#'   ),
#'   width = 6,
#'   height = 4)
#' # ```
#' 

Hurricane |> mutate(month = lubridate::month(ISO_Time), Year = factor(Year)) |> count(Year, month, Number, Name) |> count(Year, month) |> ggplot(aes(x = month, y = n, color = Year, group = Year)) + geom_line() + scale_x_continuous(breaks = scales::pretty_breaks())


PPA_LossTrend |>
  mutate(freq = ClosedClaimCount / EarnedExposure) |>
  ggplot(aes(x = YearEndingQuarter, y = freq)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.075))




sample_data <- read.delim("clipboard")



sample_data |>
  as_tibble() |>
  janitor::clean_names() |>
  mutate(dev_pd = factor(dev_pd)) |>
  filter(acc_yr != 2019) |>
  group_by(acc_yr) |>
  mutate(prior_cum = lag(cum_loss)) |>
  ggplot(aes(x = prior_cum, y = increm, group = dev_pd, color = dev_pd)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black")
sample_data |>
  as_tibble() |>
  janitor::clean_names() |>
  mutate(dev_pd = factor(dev_pd)) |>
  filter(acc_yr != 2019) |>
  ggplot(aes(x = cum_loss, y = increm, group = dev_pd, color = dev_pd)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black") +
  theme_minimal() +
  labs(
    x = "Cumulative Loss",
    y = "Incremental Loss",
    col = "Dev. Pd.",
    title = "Example Plot for Presentation B"
  )
sample_data |>
  as_tibble() |>
  janitor::clean_names() |>
  mutate(dev_pd = factor(dev_pd)) |>
  filter(acc_yr != 2019) |>
  ggplot(aes(x = cum_loss, y = increm)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black",
              se = FALSE) +
  facet_grid(~dev_pd) +
  # theme_minimal() +
  labs(
    x = "Cumulative Loss",
    y = "Incremental Loss",
    col = "Dev. Pd.",
    title = "Example Plot for Presentation B"
  )

sample_data |>
  as_tibble() |>
  janitor::clean_names() |>
  mutate(dev_pd = factor(dev_pd)) |>
  filter(acc_yr != 2019) |>
  ggplot(aes(x = dev_pd, y = tot_perc)) +
  geom_point() +
  geom_line(aes(group = acc_yr)) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              color = "black") +
  theme_minimal() +
  labs(
    x = "Cumulative Loss",
    y = "Incremental Loss",
    col = "Dev. Pd.",
    title = "Example Plot for Presentation B"
  )



Hurricane |>
  mutate(ISO_Date = lubridate::date(ISO_Time)) |>
  group_by(Year, Number, Name, ISO_Date) |>
  summarize(
    Latitude = mean(Latitude, na.rm = T),
    Longitude = mean(Longitude, na.rm = T),
    Wind = mean(Wind, na.rm = T),
    Pressure = mean(Pressure, na.rm = T)) |>
  ggplot(aes(x = Longitude,
             y = Latitude#,
             # group = Name,
             # color = Wind
  )) +
  geom_tile()


# Frequency Comparison
StateExperience |>
  group_by(Region, PolicyYear) |>
  summarize(eu = sum(NumPolicies),
            cc = sum(NumClaims),
            freq = cc / (eu * 100),
            .groups = "drop") |>
  ggplot() +
  geom_line(aes(x = Region,
                y = freq,
                group = PolicyYear)) +
  geom_bar(aes(x = Region,
               y = eu / 62500),
           stat = "identity",
           alpha = 0.5,
           width = 0.25)
# group = PolicyYezvar)) +
  



# Loss Develoment:
# library(ChainLadder)
# 
# 
# # rep_tri <- 
# PPA_LossDevelopment |>
#   mutate(AccidentYear = factor(AccidentYear)) |>
#   arrange(AccidentYear, Lag) |>
#   group_by(AccidentYear) |>
#   mutate(PriorReportedLoss = lag(ReportedLoss, default = 0),
#          Increm = ReportedLoss - PriorReportedLoss,
#          Lag_LDF = paste0(lag(Lag), "-", Lag),
#          LDF = ReportedLoss / lag(ReportedLoss)) |>
#   filter(Lag > 15) |>
#   ggplot(aes(x = PriorReportedLoss, y = Increm, color = Lag_LDF, group = Lag_LDF)) +
#   geom_point(size = 4)
# 
# 
# 
# 
# # ggplot(aes(x = Lag, y = LDF, group = AccidentYear, color = AccidentYear)) +
# # geom_point(linewidth = 1.25)
# # as.triangle(
# #   origin = "AccidentYear",
# #   dev = "Lag",
# #   value = "ReportedLoss")
# 
# plot(rep_tri)
# ata(rep_tri)


