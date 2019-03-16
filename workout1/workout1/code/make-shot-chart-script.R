#################################################################################################
#Title: Make Shot Chart
#Description: 
#Inputs:
#Outputs:
#################################################################################################

library(ggplot2)
library(jpeg)
library(grid)
library(dplyr)

klay = read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
draymond = read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
kevin = read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
andre = read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
shots.data = read.csv("../data/shots.data.csv", stringsAsFactors = FALSE)

court_file <- "../images/nba-court.jpg"

#rastering image. The function readJPEG() allows you to import the .jpg file. In turn,
#rasterGrob() takes the .jpg file and converts it into a raster graphical object. This object
#will be used as the background for the ggplot grap 

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))


klay_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))
curry_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))
draymond_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))
kevin_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))
andre_scatterplot <- ggplot(data = klay) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))



#chart with background image 
klay_shot_chart <- ggplot(data = klay) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
klay_shot_chart



curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
curry_shot_chart




draymond_shot_chart <- ggplot(data = draymond) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
draymond_shot_chart



kevin_shot_chart <- ggplot(data = kevin) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
kevin_shot_chart



andre_shot_chart <- ggplot(data = andre) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
andre_shot_chart




#4.1) Shot charts of each player
#Write code in the R script make-shot-charts-script.R to create shot charts (with court
                                                                            #backgrounds) for each player, and save the plots in PDF format, with dimensions width =
 # 6.5 and height = 5 inches, inside the folder images/:




#faceted chart
shot_all = ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420)+
  ylim(-50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+ 
  facet_grid(cols = vars(player_name), cols=3)
shot_all


shot_all = ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420)+
  ylim(-50, 420)+
  geom_point(aes(x = x, y = y, color = shot_made_flag))+ 
  facet_wrap(~player_name, ncol=3)
shot_all
ggsave("gsw-shot-charts.png", plot = last_plot(), width = 8, height = 7, units = "in")

shot_all = ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: (2016 season)') +
  theme_minimal()
shot_all  

shot_all + facet_grid(cols = vars(player_name))

facet_shot_chart <- ggplot(data = shot_all) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: (2016 season)') +
  theme_minimal()
facet_shot_chart

#table
table1 = summarise(
  group_by(shots_data, player_name),
  total_shottype = sum(shot_type=="2PT Field Goal"),
  total_shotmade = sum(shot_made_flag=="shot_yes" & shot_type=="2PT Field Goal"),
  percentage = total_shottype/total_shotmade
)

table1 =arrange(table1, desc(percentage))

table2 = summarise(
  group_by(shots_data, player_name),
  total_shottype = sum(shot_type=="3PT Field Goal"),
  total_shotmade = sum(shot_made_flag=="shot_yes" & shot_type=="3PT Field Goal"),
  percentage = total_shottype/total_shotmade
)


table2 =arrange(table2, desc(percentage))


table3 = summarise(
  group_by(shots_data, player_name),
  total = sum(shot_type=="3PT Field Goal" | shot_type=="2PT Field Goal"),
  total2 = sum(shot_made_flag=="shot_yes" & shot_type =="2PT Field Goal" | shot_type =="3PT Field Goal" ),
  percentage = total/total2
)
table3 =arrange(table3, desc(percentage))

write.csv(table1, "/Users/juliavassey/Desktop/demo-repo-master/workout1/data/table1.csv")
write.csv(table2, "/Users/juliavassey/Desktop/demo-repo-master/workout1/data/table2.csv")
write.csv(table3, "/Users/juliavassey/Desktop/demo-repo-master/workout1/data/table3.csv")

