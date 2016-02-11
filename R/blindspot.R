library("dplyr")
library("ggplot2")
library("tidyr")

# Example to hardcode data file: 
# getwd() # find project folder & paste location as string for path
# f_path <- "/Users/R/Documents/Teaching/Spring2016/SensLab/CogLabs/"  # paste address string in quotes
# f_csv<-"SignalDetection/SigDet.csv"  # filename string
# data_set<-read.csv(file= paste0(f_path,f_csv), header = TRUE, stringsAsFactors = FALSE)

fn<-file.choose()
data_set <- read.csv(file = fn, header = TRUE, stringsAsFactors = FALSE)
blind_spot <- as.tbl(data_set)

# Tidy up data set for plotting
blind_spot_tidy <- gather(blind_spot, View, X.Coordinate, Disappeared, Reappeared)

# Tile plot of raw data
ggplot(blind_spot_tidy, aes(x = X.Coordinate, y = Y.Coordinate, fill = View)) + 
  geom_tile() + facet_wrap(~randGator)

# Filter out y-axis observations w/o blindspot (to see data more clearly)
blind_spot_nreap <- blind_spot %>% 
  filter(Reappeared >= Disappeared)

# Linerange plots of faceted individuals from class data set
ggplot(blind_spot_nreap, aes(x = Y.Coordinate)) + 
  geom_linerange(alpha = 0.4, size = 10, aes(ymin = Disappeared, ymax = Reappeared)) + 
  facet_wrap(~randGator)

# Linerange plots of data overlay for all individuals from class data set
ggplot(blind_spot_nreap, aes(x = Y.Coordinate, group = randGator)) + 
  geom_linerange(alpha = 0.07, size = 10, aes(ymin = Disappeared, ymax = Reappeared)) +
  coord_fixed(ratio = 1) +
  coord_flip()

