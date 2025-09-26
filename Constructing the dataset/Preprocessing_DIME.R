library(tidyr)
library(dplyr)

#in order to change the path, follow https://www.dropbox.com/scl/fi/bcpmxvxwc7iwmqvchm2sp/dime_recipients_1979_2024.csv.gz?rlkey=4ii8765xw8y2svlzcbxzfvnbh&dl=1
dime <-read.table("/Users/giovannistivella/Documents/Università/SSSUP/PSU/Data/dime_recipients_1979_2024.csv", 
                  header = TRUE,    # first row of file contains variable names
                  sep = ",",        # need to specify that the file is comma-separator
                  dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")


#Select only rows where "cycle" is 2016, 2018, 2020, "fecyear" is 2016,2018,2020 and ("state" is PA or "seat" is "federal:president")
dime_pa_filtered <- dime %>%
    filter(cycle %in% c(2016, 2018, 2020) & (fecyear %in% c(2016,2018,2020)) &(state == "PA" | seat == "federal:president"))

#Select only rows where "seat" is "federal:president", "federal:senate", "state:attorneyg", "state:governor"
dime_pa_filtered <- dime_pa_filtered %>%
  filter(seat %in% c("federal:president", "federal:senate", "state:attorneyg", "state:governor"))

#There are a lot of missing data on "pwinner" or "gen.vote.pct"; consequently, I will pick candidates "by hand"
dime_pa_candidates <- dime_pa_filtered %>%
  filter(lname %in% 
           {
             pa_long <- read.csv("pennsylvania_long.csv")
             unique(c(pa_long$candidate_name_1, pa_long$candidate_name_2))
})

#Save this
write.csv(dime_pa_candidates, "dime_pa_candidates.csv", row.names = FALSE)
