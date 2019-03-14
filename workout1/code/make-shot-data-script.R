#reading all files
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
draymond = read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
kevin = read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
klay = read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
andre = read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)

#adding columns
curry$stephencurry= "Stephen Curry"
draymond$draymondgreen = "Draymond Green"
kevin$kevindurant = "Kevin Durant"
klay$klaythompson = "Klay Thompson"
andre$andreiquodala = "Andre Igoudala"

#renaming values
curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes"

draymond$shot_made_flag[draymond$shot_made_flag == "n"] = "shot_no"
draymond$shot_made_flag[draymond$shot_made_flag == "y"] = "shot_yes"

kevin$shot_made_flag[kevin$shot_made_flag == "n"] = "shot_no"
kevin$shot_made_flag[kevin$shot_made_flag == "y"] = "shot_yes"

klay$shot_made_flag[klay$shot_made_flag == "n"] = "shot_no"
klay$shot_made_flag[klay$shot_made_flag == "y"] = "shot_yes"

andre$shot_made_flag[andre$shot_made_flag == "n"] = "shot_no"
andre$shot_made_flag[andre$shot_made_flag == "y"] = "shot_yes"

#creating minutes
curry$minute <- (curry$period*12 - curry$minutes_remaining)
draymond$minute = (draymond$period*12 - draymond$minutes_remaining)
kevin$minute = (kevin$period*12 - kevin$minutes_remaining)
klay$minute = (klay$period*12 - klay$minutes_remaining)
andre$minute = (andre$period*12 - andre$minutes_remaining)

#used capture function to send the files to output
curry.summary = summary(curry)
capture.output(curry.summary, file = "../output/stephen-curry-summary.txt")

draymond.summary = summary(draymond)
capture.output(draymond.summary, file = "../output/draymond-green-summary.txt")

kevin.summary = summary(kevin)
capture.output(kevin.summary, file = "../output/kevin-durant-summary.txt")

klay.summary = summary(klay)
capture.output(klay.summary, file = "../output/klay-thompson-summary.txt")

andre.summary = summary(andre)
capture.output(andre.summary, file = "../output/andre-iguodala-summary.txt")

#unused codes
#shots_data_all = c("curry", "klay", "andre", "kevin", "draymond")
#x.list <- lapply(shots_data_all, get)
#shots_data = do.call(rbind, x.list)

#giving column the same name
colnames(curry)[14] = "player_name"
colnames(draymond)[14] = "player_name"
colnames(kevin)[14] = "player_name"
colnames(klay)[14] = "player_name"
colnames(andre)[14] = "player_name"

#binding in one table
rbind(curry, klay, andre, kevin, draymond)


#creating dataframe for all players
write.csv(
  rbind(curry, klay, andre, kevin, draymond), "../data/shots-data.csv")

#summary
shots.data = read.csv("shots-data.csv")
shots.data.summary = summary(shots.data)
capture.output(shots.data.summary, file = "../output/shots-data-summary.txt")



