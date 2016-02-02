

blind_spot_tidy <- gather(blind_spot,View,X.Coordinate,Disappeared,Reappeared)

# Tile plot of raw data
ggplot(blind_spot_tidy, aes(x = X.Coordinate, y = Y.Coordinate, fill = View)) + 
  geom_tile() + facet_wrap(~randGator)

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

