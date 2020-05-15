GDP <- read.csv(file = "./Data/GDP.csv", 
                skip = 4,
                nrows = 231,
                strip.white = TRUE, 
                na.strings = c("", "..")) %>%
        select(X, X.1, X.3, X.4) %>%
        filter(!is.na(X.1)) %>%
        rename(CountryCode = X, 
               GDP.Rank = X.1, 
               Table.Name = X.3, 
               GDP.Dollars = X.4)

edu <- read.csv(file = "./Data/edu.csv")

matched <- merge(GDP, edu, by = "CountryCode", all.x = TRUE) %>%
        arrange(desc(GDP.Rank))

count(matched)

matched[13, 1]

matched %>%
        group_by(Income.Group) %>%
        summarise(mean(GDP.Rank))

library(Hmisc)
quanted <- mutate(matched, GDP.Quant = cut2(GDP.Rank, g = 5))
table(quanted$GDP.Quant, quanted$Income.Group)
