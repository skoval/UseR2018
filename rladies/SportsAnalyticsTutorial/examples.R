library(stringr)

load(file = "~/Talks/SportsAnalyticsTutorial/match_stats.RData")

split <- str_split(match_stats, "\n")[[1]]


pattern <- "[a-z]"

stats <- str_detect(split, pattern)

stats <- str_subset(split, pattern)

values <- split[
	!str_detect(split, pattern) &
	!str_detect(split, "[A-Z]")
 ] # Get values


counts <- stats %in% c("Aces", 
		"Double Faults", 
		"Service Games Played", 
		"Return Games Played")

data.frame(
	stat = rep(stats, ifelse(counts, 2, 4)),
	values = values
)


str_replace_all(values,"[\\(%\\)]", "")

library(gtools)

load("atp_2016_odds.RData")

classes <- apply(atp_odds, 2, class)

for(name in names(classes)){
	print(sort(unique(atp_odds[,name])))
	ask()
}


atp_odds$Best.of <- as.numeric(atp_odds$Best.of)


rounds <- c("Round Robin", "1st Round", "2nd Round", "3rd Round", "4th Round", "Quarterfinals", "Semifinals", "The Final")

atp_odds$Round <- factor(atp_odds$Round,
		levels = rounds,
		order = T
)

sort(unique(atp_odds$Tournament))

players <- sort(unique(atp_odds$Winner))

approx <- lapply(players, agrep, fixed = T, x = players)

players[sapply(approx, length) > 1]


# Practice
atp_matches <- read.csv("~/Talks/SportsAnalyticsTutorial/atp_matches_2016.csv", stringsAsFactors = F)

atp_matches <- atp_matches[,c('tourney_name', 'surface', 'draw_size', 'tourney_date', 'winner_name', 'loser_name', 'round', 'minutes', 'w_1stWon', 'w_2ndWon', 'w_svpt', 'l_1stWon', 'l_2ndWon', 'l_svpt')]


save(atp_matches, file = "~/Talks/SportsAnalyticsTutorial/atp_matches_2016.RData")


load("~/Talks/SportsAnalyticsTutorial/atp_matches_2016.RData")

classes <- apply(atp_matches, 2, class)

for(name in names(classes)){
	print(sort(unique(atp_matches[,name])))
	ask()
}


atp_matches <- atp_matches %>%
	filter(round != "", tourney_name != "Olympics") %>%
	dplyr::mutate(
		tourney_date = ymd(tourney_date),
		 minutes = as.numeric(minutes),
		 w_1stWon = as.numeric(w_1stWon),
		 w_2ndWon = as.numeric(w_2ndWon),
		 w_svpt = as.numeric(w_svpt),
		 l_1stWon = as.numeric(l_1stWon),
		 l_2ndWon = as.numeric(l_2ndWon),
		 l_svpt = as.numeric(l_svpt),
		 draw_size = as.numeric(draw_size),
		 round = factor(round, levels = c("RR","R128","R64","R32","R16","QF","SF","F"), order = T)
	)
	

players <- sort(unique(c(atp_matches$winner_name, atp_matches$loser_name)))

approx <- lapply(players, agrep, fixed = T, x = players)

players[sapply(approx, length) > 1]

atp_matches$winner_name[atp_matches$winner_name == "Joao Souza"] <- "Joao Sousa"
atp_matches$loser_name[atp_matches$loser_name == "Joao Souza"] <- "Joao Sousa"
	

save(atp_matches, file = "~/Talks/SportsAnalyticsTutorial/atp_matches_clean.RData")
	
	
atp_matches %>%
	dplyr::select(minutes:l_svpt)	%>%
	dplyr::summarise_each(
		funs(
			missing = sum(is.na(.)),
			mean = mean(., na.rm = T),
			median = median(., na.rm = T),
			IQR = IQR(., na.rm = T)
			)
	)
	
	
atp_matches %>%
	ggplot(aes(y = minutes, x = tourney_date)) + 
	geom_point() + 
	theme_bw()
	
load(file = "~/Courses/mvparrot/data/atp_serves.RData")


atp_serves <- as.data.frame(atp_serves) %>%
	filter(server == "RAONIC", sign(start.x) == 1, side == "Ad") 
	
atp_serves <- atp_serves[,c(grep("^raw", names(atp_serves), val = T), "start.1", "start.3", "duration.arc1", "duration.arc3")]

atp_serves$serveid <- 1:nrow(atp_serves)

arc1 <- atp_serves[,c("serveid", grep("1$", names(atp_serves), val = T))]
arc3 <- atp_serves[,c("serveid", grep("3$", names(atp_serves), val = T))]

arc1$arc <- 1
arc3$arc <- 2

names(arc1) <- sub("raw.", "", names(arc1))
names(arc1) <- sub("\\.1", "", names(arc1))
names(arc1) <- sub("\\.arc1", "", names(arc1))


names(arc3) <- sub("raw.", "", names(arc3))
names(arc3) <- sub("\\.3", "", names(arc3))
names(arc3) <- sub("\\.arc3", "", names(arc3))


atp_serves <- rbind(as.data.frame(arc1), as.data.frame(arc3))

atp_serves <- atp_serves[order(atp_serves$serveid, atp_serves$arc),]

save(atp_serves, file = "~/Talks/SportsAnalyticsTutorial/atp_serves.RData")


get_position <- function(a, b, c, d, t0, t1){

	# Create positional function
	g <- function(t) c(a, b, c, d) %*% t^(0:3)
	g <- Vectorize(g)

	times <- seq(t0, t1, length = 1000)
	
data.frame(
		position = g(times),
		seconds = times
	)
}

library(purrr)

atp_serves$id <- interaction(atp_serves$serveid, atp_serves$arc)

x_position <- atp_serves %>%
	split(.$id) %>%
	map_df(
		~ get_position(.x$x0, .x$x1, .x$x2, .x$x3, .x$start, .x$start + .x$duration)
	)
	
y_position <- atp_serves %>%
	split(.$id) %>%
	map_df(
		~ get_position(.x$y0, .x$y1, .x$y2, .x$y3, .x$start, .x$start + .x$duration)	
		)

z_position <- atp_serves %>%
	split(.$id) %>%
	map_df(
		~ get_position(.x$z0, .x$z1, .x$z2, .x$z3, .x$start, .x$start + .x$duration)
	)	
	
positions <- data.frame(
	seconds = x_position$seconds,
	x = x_position$position,
	y = y_position$position,
	z = z_position$position,
	id = str_replace(rep(sort(atp_serves$id), each = 1000), "(.*)(\\..*)", "\\1")
)
		
positions$id <- as.numeric(positions$id)		

positions  <- positions[order(positions$id, positions$seconds),]


library(plot3D)



points3D(
	x = positions$x,
	y = positions$y,
	z = positions$z,
	colvar = positions$id
)



points3D(
	x = positions$x,
	y = positions$y,
	z = positions$z,
	colvar = as.numeric(positions$id),
	theta = 0
)

positions <- positions %>%
    group_by(id) %>%
    dplyr::mutate(
      negative = x[1] < 0
    )


points3D(
	x = positions$x[positions$negative],
	y = positions$y[positions$negative],
	z = positions$z[positions$negative],
	colvar = positions$id[positions$negative],
	theta = 0
)



atp_matches <- read.csv("~/Talks/SportsAnalyticsTutorial/atp_matches_2016.csv", stringsAsFactors = F)

atp_matches <- 	atp_matches %>%
	filter(round != "", tourney_name != "Olympics") %>%
	dplyr::mutate(
		tourney_date = ymd(tourney_date)
	)
	
for(name in c(grep("^w_", names(atp_matches)), grep("^l_", names(atp_matches))))
	atp_matches[,name] <- as.numeric(atp_matches[,name])
	
atp_matches <- atp_matches %>%
	filter(!is.na(w_svpt), w_svpt != 0) %>%
	dplyr::mutate(
		w_ace_rate = (w_ace / w_svpt),
		w_df_rate = (w_df / w_svpt),
		w_first_serve_in_p = (w_1stIn / w_svpt), 
		w_first_serve_won_p =  (w_1stWon / w_1stIn),
		w_second_serve_won_p =  (w_2ndWon / (w_svpt - w_1stIn)),
		w_serve_won_p = ((w_1stWon + w_2ndWon) / w_svpt),
		w_first_return_won_p = 1 - (l_1stWon / l_1stIn),
		w_second_return_won_p = 1 - (l_2ndWon / (l_svpt - l_1stIn)),
		w_return_won_p = 1 - ((l_1stWon + l_2ndWon) / l_svpt),
		w_total_won_p = ((w_1stWon + w_2ndWon) + (l_svpt - (l_1stWon + l_2ndWon))) / (l_svpt + w_svpt), 
		w_bp = (l_bpFaced - l_bpSaved),
		w_bp_opp =  l_bpFaced,
		l_ace_rate = (l_ace / l_svpt),
		l_df_rate = (l_df / l_svpt),
		l_first_serve_in_p = (l_1stIn / l_svpt), 
		l_first_serve_won_p =  (l_1stWon / l_1stIn),
		l_second_serve_won_p =  (l_2ndWon / (l_svpt - l_1stIn)),
		l_serve_won_p = ((l_1stWon + l_2ndWon) / l_svpt),
		l_first_return_won_p = 1 - (w_1stWon / w_1stIn),
		l_second_return_won_p = 1 - (w_2ndWon / (w_svpt - w_1stIn)),
		l_return_won_p = 1 - ((w_1stWon + w_2ndWon) / w_svpt),
		l_total_won_p = ((l_1stWon + l_2ndWon) + (w_svpt - (w_1stWon + w_2ndWon))) / (w_svpt + l_svpt), 
		l_bp = (w_bpFaced - w_bpSaved),
		l_bp_opp =  w_bpFaced
	)


atp_matches_2015 <- read.csv("~/Talks/SportsAnalyticsTutorial/atp_matches_2015.csv", stringsAsFactors = F)

atp_matches_2015 <- atp_matches_2015 %>%
	filter(round != "", tourney_name != "Olympics") %>%
	dplyr::mutate(
		tourney_date = ymd(tourney_date)
	)
	
for(name in c(grep("^w_", names(atp_matches_2015)), grep("^l_", names(atp_matches_2015))))
	atp_matches_2015[,name] <- as.numeric(atp_matches_2015[,name])
	
atp_matches_2015 <- atp_matches_2015 %>%
	filter(!is.na(w_svpt), w_svpt != 0) %>%
	dplyr::mutate(
		w_ace_rate = (w_ace / w_svpt),
		w_df_rate = (w_df / w_svpt),
		w_first_serve_in_p = (w_1stIn / w_svpt), 
		w_first_serve_won_p =  (w_1stWon / w_1stIn),
		w_second_serve_won_p =  (w_2ndWon / (w_svpt - w_1stIn)),
		w_serve_won_p = ((w_1stWon + w_2ndWon) / w_svpt),
		w_first_return_won_p = 1 - (l_1stWon / l_1stIn),
		w_second_return_won_p = 1 - (l_2ndWon / (l_svpt - l_1stIn)),
		w_return_won_p = 1 - ((l_1stWon + l_2ndWon) / l_svpt),
		w_total_won_p = ((w_1stWon + w_2ndWon) + (l_svpt - (l_1stWon + l_2ndWon))) / (l_svpt + w_svpt), 
		w_bp = (l_bpFaced - l_bpSaved),
		w_bp_opp =  l_bpFaced,
		l_ace_rate = (l_ace / l_svpt),
		l_df_rate = (l_df / l_svpt),
		l_first_serve_in_p = (l_1stIn / l_svpt), 
		l_first_serve_won_p =  (l_1stWon / l_1stIn),
		l_second_serve_won_p =  (l_2ndWon / (l_svpt - l_1stIn)),
		l_serve_won_p = ((l_1stWon + l_2ndWon) / l_svpt),
		l_first_return_won_p = 1 - (w_1stWon / w_1stIn),
		l_second_return_won_p = 1 - (w_2ndWon / (w_svpt - w_1stIn)),
		l_return_won_p = 1 - ((w_1stWon + w_2ndWon) / w_svpt),
		l_total_won_p = ((l_1stWon + l_2ndWon) + (w_svpt - (w_1stWon + w_2ndWon))) / (w_svpt + l_svpt), 
		l_bp = (w_bpFaced - w_bpSaved),
		l_bp_opp =  w_bpFaced
	)
	
# Assign Rounds
atp_matches <- atp_matches %>%
	dplyr::mutate(
	    	matchid = paste(tourney_date, round, winner_name, loser_name, sep = ":"),
		 round = factor(round, levels = c("RR","R128","R64","R32","R16","QF","SF","F"), order = T)
	)

atp_matches_2015 <- atp_matches_2015 %>%
	dplyr::mutate(
		 matchid = paste(tourney_date, round, winner_name, loser_name, sep = ":"),
		 round = factor(round, levels = c("RR","R128","R64","R32","R16","QF","SF","F"), order = T)
	)
		

atp_matches <- rbind(atp_matches, atp_matches_2015)

atp_matches_winners <- atp_matches %>%
	dplyr::select(matchid, tourney_name, surface, tourney_level, tourney_date, round, winner_name, dplyr::contains("w_"), -draw_size)	

names(atp_matches_winners) <- sub("(winner_)", "", names(atp_matches_winners))
names(atp_matches_winners) <- sub("^w_", "", names(atp_matches_winners))
atp_matches_winners$winner <- 1

atp_matches_losers <- atp_matches %>%
	dplyr::select(matchid, tourney_name, surface, tourney_level, tourney_date, round, loser_name, dplyr::contains("l_"), -w_total_won_p)	

names(atp_matches_losers) <- sub("(loser_)", "", names(atp_matches_losers))
names(atp_matches_losers) <- sub("^l_", "", names(atp_matches_losers))
atp_matches_losers$winner <- 0

atp_matches <- rbind(atp_matches_winners, atp_matches_losers)

players <- sort(unique(atp_matches$name))

approx <- lapply(players, agrep, fixed = T, x = players)

players[sapply(approx, length) > 1]

atp_matches$name[atp_matches$name == "Joao Souza"] <- "Joao Sousa"

atp_matches <- atp_matches[order(atp_matches$tourney_date, atp_matches$round),]
		
atp_matches <- atp_matches %>%
	group_by(tourney_name, tourney_date, name) %>%
	dplyr::mutate(
		match_date = tourney_date + days(0:(n() - 1))
	)		
	
library(zoo)	
		
atp_matches <- atp_matches[order(atp_matches$match_date),]		

atp_averages <- atp_matches %>%
	group_by(name) %>%
	dplyr::mutate(
		ace_rate = rollmean(ace_rate, k = min(c(30, n())), na.pad = T, align = "right"),   
		df_rate = rollmean(df_rate, k = min(c(30, n())), na.pad = T, align = "right"),
		first_serve_in_p = rollmean(first_serve_in_p, k = min(c(30, n())), na.pad = T, align = "right"),
		first_serve_won_p = rollmean(first_serve_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		second_serve_won_p = rollmean(second_serve_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		serve_won_p = rollmean(serve_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		first_return_won_p = rollmean(first_return_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		second_return_won_p = rollmean(second_return_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		return_won_p= rollmean(return_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		total_won_p = rollmean(total_won_p, k = min(c(30, n())), na.pad = T, align = "right"),
		bp = rollmean(bp, k = min(c(30, n())), na.pad = T, align = "right"),
		bp_opp = rollmean(bp_opp, k = min(c(30, n())), na.pad = T, align = "right")
	)
	
# Shift one forward to prevent overlap with current performance	
atp_averages <- atp_averages %>%
	group_by(name) %>%
	dplyr::mutate(
		ace_rate = c(NA, ace_rate[-n()]),
		df_rate = c(NA, df_rate[-n()]),
		first_serve_in_p = c(NA, first_serve_in_p[-n()]),
		first_serve_won_p = c(NA, first_serve_won_p[-n()]),
		second_serve_won_p = c(NA, second_serve_won_p[-n()]),
		serve_won_p = c(NA, serve_won_p[-n()]),
		first_return_won_p = c(NA, first_return_won_p[-n()]),
		second_return_won_p = c(NA, second_return_won_p[-n()]),
		return_won_p= c(NA, return_won_p[-n()]),
		total_won_p = c(NA, total_won_p[-n()]),
		bp = c(NA, bp[-n()]),
		bp_opp = c(NA, bp_opp[-n()])
	)	

# Retain only 2016 and non-missing
atp_averages <- atp_averages %>%
	filter(year(tourney_date) == 2016, !is.na(ace_rate))	

atp_averages <- atp_averages %>%
	group_by(matchid) %>%
	dplyr::mutate(
		order = sample(1:2, size = n()),
		n = n()
	)

atp_averages <- atp_averages[order(atp_averages$matchid, atp_averages$order),]

atp_matches <- atp_averages %>%
	filter(n == 2) %>%
	dplyr::summarise(
		tourney_name = tourney_name[1],
		surface = surface[1],
		player1 = name[1],
		player2 = name[2],
		tourney_date = tourney_date[1],
		round = round[1],
		winner = winner[1],
		diff_ace_rate = ace_rate[1] - ace_rate[2],
		diff_df_rate = df_rate[1] - df_rate[2],
		diff_first_serve_in_p = first_serve_in_p[1] - first_serve_in_p[2],
		diff_first_serve_won_p =  first_serve_won_p[1] - first_serve_won_p[2],
		diff_second_serve_won_p = second_serve_won_p[1] - second_serve_won_p[2],
		diff_serve_won_p = serve_won_p[1] - serve_won_p[2],
		diff_first_return_won_p = first_return_won_p[1] - first_return_won_p[2],
		diff_second_return_won_p = second_return_won_p[1] - second_return_won_p[2],
		diff_return_won_p = return_won_p[1] - return_won_p[2],
		diff_total_won_p = total_won_p[1] - total_won_p[2],
		diff_bp = bp[1] - bp[2],
		diff_bp_opp = bp_opp[1] - bp_opp[2]
	)	
	

diff_stats <- as.data.frame(atp_matches)

impute <- mice(diff_stats[, grep("(winner)|(diff)", names(diff_stats), val = T)], m = 1)

diff_stats[, grep("(winner)|(diff)", names(diff_stats), val = T)] <- 
	complete(impute)

save(diff_stats, file = "~/Talks/SportsAnalyticsTutorial/diff_stats.RData")

diff_stats[,grep("diff", names(diff_stats))] <- 
	mice(diff_stats[,grep("diff", names(diff_stats))], m = 1)$imputation[[1]]


library(caret)

data(diff_stats)

train <- createDataPartition(diff_stats$winner, times = 1, p = .7, list = F)

diff_stats$winner <- factor(diff_stats$winner, labels = c("Loser", "Winner"))

features <- grep("diff", names(diff_stats))

diff_stats[,features] <- scale(diff_stats[,features])

correlation <- cor(diff_stats[,features])

round(correlation, 2) > .95 & row(correlation) != col(correlation)

diff_stats$train <- 0
diff_stats$train[train[,1]] <- 1

trainingData <- diff_stats %>%
	dplyr::filter(train == 1) %>%
	select(winner, dplyr::contains("diff"))

testData <- diff_stats %>%
	dplyr::filter(train == 0) %>%
	select(winner, dplyr::contains("diff"))


ctrlSpecs <- trainControl(
	method = "repeatedCV",
	repeats = 5,
	summaryFunction = mnLogLoss,
	classProbs = TRUE
)

set.seed(1115)

rfFit <- train(
	winner ~ .,
	data = trainingData,
	method = "rf",
	trControl = ctrlSpecs,
	metric = "logLoss"
)

plot(rfFit) 

rfFit$finalModel

importance(rfFit$finalModel) # Final model

predictions <- predict(rpartFit, testData)

confusionMatrix(predictions, testData$winner)

set.seed(1115)

tuneGrid <- expand.grid(
	.model = "tree",
	.trials = 1:100,
	.winnow = FALSE
)

C5Fit <- train(
	winner ~ .,
	data = trainingData,
	method = "C5.0",
	tuneGrid = tuneGrid,
	trControl = ctrlSpecs,
	metric = "logLoss"
)

plot(C5Fit) 

set.seed(1115)


svmFit <- train(
	winner ~ .,
	data = trainingData,
	method = "svmLinear",
	tuneLength = 30,
	trControl = ctrlSpecs,
	metric = "logLoss"
)

plot(svmFit) 

#### Blogging

library(blogdown)

setwd("~/Software/sports-blog")

new_site()

install_theme("vimux/mainroad", theme_example = TRUE, update = TRUE)

new_post(
	title = "French Open 2017",
	kind = "Rmd"
)