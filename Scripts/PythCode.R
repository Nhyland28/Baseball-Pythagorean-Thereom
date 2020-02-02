library(tidyverse)
library(formattable)
library(webshot)

## Read AL 2019 dataset

MLB <- read.csv("./Data/Standings 2018-2019 - Sheet1.csv")

#### Run a for loop for 1-3 (including the tenths spot) to test exponents for the pythagorean theorem 
## Calculate the pythagoraen theorem for w/l
## r^2 / (r^2 + 1) : where r is runs scored / runs allowed

## Create a dataframe to store the MADs of the different exponents
MADdf <- tibble(Exponent = numeric(),
                Mean.Absolute.Difference = numeric())

for (i in seq(1, 3, by = 0.1)) {
  # Set a dummy variable for the exponent we want to test
  ex <- i
  # Each exponent we test gets a dataframe
  i <- MLB %>%
    mutate(RunRatio = RS/RA) %>%
    mutate(PythTheorem = (RunRatio^ex)/((RunRatio^ex)+1)) %>%
    mutate(PredVsActWinPercent = PythTheorem - W.L.) %>%
    mutate(AbsPredVsActWinPercent = abs(PredVsActWinPercent))
  
  MAD <- mean(i$AbsPredVsActWinPercent)
  MADdf <- add_row(MADdf, Exponent = ex, Mean.Absolute.Difference = MAD)
}

#### Create a graph to show the exponents with the lowest MAD
Exponent.Graph <- ggplot(MADdf, mapping = aes(x = Exponent, y = Mean.Absolute.Difference)) + 
  geom_point(color = "#002c74") + 
  geom_line(color = "#002c74") +
  ggtitle("Best Exponent for Baseball's Pythagorean Theorem") +
  ylab("Mean Absolute Difference")

ggsave("./Outputs/ExponentGraph.png", plot = Exponent.Graph)

#### Lowest MAD was an exponent of 1.9 so we will use this as our base case

MLB.1.9 <- MLB %>%
  mutate(RunRatio = RS/RA) %>%
  mutate(PythTheorem = (RunRatio^1.9)/((RunRatio^1.9) + 1)) %>%
  mutate(PredVsActWinPercent = PythTheorem - W.L.) %>%
  mutate(AbsPredVsActWinPercent = abs(PredVsActWinPercent)) %>%
  mutate(PredictedWins = ceiling(PythTheorem*162)) %>%
  mutate(PredictedLosses = 162 - PredictedWins) %>%
  mutate(RealWinsVsExpectedWins = W - PredictedWins) %>%
  select(Year, Tm, W, L, W.L., PythTheorem, PredictedWins, PredictedLosses, RealWinsVsExpectedWins) %>%
  arrange(RealWinsVsExpectedWins)

#### Create a formatted data table

MLB.1.9.Table <- MLB.1.9[c(1:5, 27, 30, 55:60),]
MLB.Table <- formattable(MLB.1.9.Table)

#### Download function to export formattable table and then export
#### Function from https://github.com/renkun-ken/formattable/issues/26

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
export_formattable(MLB.Table, "./Outputs/MLBStandings.png")
