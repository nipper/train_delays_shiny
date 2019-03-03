full_trains %>%
  filter(departure_station == "ST PIERRE DES CORPS" ) %>% 
  mutate(on_time_trains = total_num_trips - num_late_at_departure) %>% 
  select(departure_station,observation_date,on_time_trains,num_late_at_departure) %>% 
  gather(key,value,c(on_time_trains,num_late_at_departure)) %>% 
  group_by(observation_date) %>% 
  mutate(pct = value/sum(value)) %>% 
  {
    if (t) {
      ggplot(.,aes(x = observation_date,y = pct , fill = fct_rev(key) ))
    } else {
      ggplot(.,aes(x = observation_date, y = value, fill = fct_rev(key) ))
    }
  } + 
  geom_col() +
  { 
    if (s) {
      if (t) {
        geom_label_repel(aes(label = percent(pct)))
      } else {
        geom_label(aes(label = value))
      }
    }
    } +
  scale_x_date(date_breaks = "3 months",
               labels = function(x) {format(x,format = "%b\n%Y")}) +
  scale_fill_manual(labels = c("Departed on Time","Departed Late"),
                    values = c("#23ACB4","#F84343")) +
  labs(x = "",
       y = "# of trips",
       title = "Trains Arriving Late",
       fill = "Departure Status") +
  legend_bottom()

t <- TRUE
s <- TRUE
