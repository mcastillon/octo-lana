library(randomForest)
library(magrittr)
library(reshape2)
library(data.table)
library(tidyverse)
library(MLmetrics)

setwd('../octo-lana/')

compact_results <- fread('mens-machine-learning-competition-2019/DataFiles/RegularSeasonCompactResults.csv')
conference_results <- fread('mens-machine-learning-competition-2019/DataFiles/ConferenceTourneyGames.csv')
setnames(conference_results, tolower(colnames(conference_results)))
setnames(compact_results, tolower(colnames(compact_results)))
tourney_results <- fread('mens-machine-learning-competition-2019/DataFiles/NCAATourneyCompactResults.csv')
setnames(tourney_results, tolower(colnames(tourney_results)))
nit_results <- fread('mens-machine-learning-competition-2019/DataFiles/SecondaryTourneyCompactResults.csv')
setnames(nit_results, tolower(colnames(nit_results)))
team_spellings <- fread('mens-machine-learning-competition-2019/DataFiles/TeamSpellings.csv')

kenpom <- lapply(2:18,
                 function(s) {
                   yr <- formatC(s, format = "d", width = 2, flag = "0")
                   fread(paste0('kenpom/summary', yr, '_pt.csv'))
                 })
kp <- bind_rows(kenpom)
kp[, TeamNameSpelling := tolower(TeamName)]
kp_t <- merge(kp, team_spellings,
              by = 'TeamNameSpelling',
              all.x = T, all.y = F)
filter(kp_t,
       is.na(TeamID)) %>%
  group_by(TeamNameSpelling) %>%
  summarize(n = n())

homevec <- function(vec){
  newvec <- 1*(vec == "H")
  newvec[vec == "A"] <- -1
  newvec[vec == 'N'] <- 0
  return(newvec)
}

compact_results[, periods := 2 + numot]

cr <- merge(compact_results, kp_t,
            by.x = c('season', 'wteamid'), by.y = c('Season', 'TeamID'),
            all.x = T, all.y = F)
cr_full <- merge(cr, kp_t,
                 by.x = c('season', 'lteamid'), by.y = c('Season', 'TeamID'),
                 all.x = T, all.y = F)
setnames(cr_full, tolower(colnames(cr_full)))

sam <- sample(1:nrow(cr_full), nrow(cr_full)/2)

#### Stack compact results twice ####
compact_11 <- cr_full[sam,]
compact_12 <- cr_full[-sam,]
compact_21 <- cr_full[-sam,]
compact_22 <- cr_full[sam,]

compact_11 %<>% transform(team1 = wteamid, score1 = wscore,
                          team2 = lteamid, score2 = lscore,
                          team1Home = homevec(wloc), orig = 1,
                          tempo1 = tempo.x, ranktempo1 = ranktempo.x,
                          adjtempo1 = adjtempo.x, rankadjtempo1 = rankadjtempo.x,
                          oe1 = oe.x, rankoe1 = rankoe.x,
                          adjoe1 = adjoe.x, rankadjoe1 = rankadjoe.x,
                          de1 = de.x, rankde1 = rankde.x,
                          adjde1 = adjde.x, rankadjde1 = rankadjde.x,
                          adjem1 = adjem.x, rankdajem1 = rankadjem.x,
                          seed1 = seed.x,
                          tempo2 = tempo.y, ranktempo2 = ranktempo.y,
                          adjtempo2 = adjtempo.y, rankadjtempo2 = rankadjtempo.y,
                          oe2 = oe.y, rankoe2 = rankoe.y,
                          adjoe2 = adjoe.y, rankadjoe2 = rankadjoe.y,
                          de2 = de.y, rankde2 = rankde.y,
                          adjde2 = adjde.y, rankadjde2 = rankadjde.y,
                          adjem2 = adjem.y, rankdajem2 = rankadjem.y,
                          seed2 = seed.y)
compact_12 %<>% transform(team1 = lteamid, score1 = lscore,
                          team2 = wteamid, score2 = wscore,
                          team1Home = homevec(wloc), orig = 1,
                          tempo1 = tempo.y, ranktempo1 = ranktempo.y,
                          adjtempo1 = adjtempo.y, rankadjtempo1 = rankadjtempo.y,
                          oe1 = oe.y, rankoe1 = rankoe.y,
                          adjoe1 = adjoe.y, rankadjoe1 = rankadjoe.y,
                          de1 = de.y, rankde1 = rankde.y,
                          adjde1 = adjde.y, rankadjde1 = rankadjde.y,
                          adjem1 = adjem.y, rankdajem1 = rankadjem.y,
                          seed1 = seed.y,
                          tempo2 = tempo.x, ranktempo2 = ranktempo.x,
                          adjtempo2 = adjtempo.x, rankadjtempo2 = rankadjtempo.x,
                          oe2 = oe.x, rankoe2 = rankoe.x,
                          adjoe2 = adjoe.x, rankadjoe2 = rankadjoe.x,
                          de2 = de.x, rankde2 = rankde.x,
                          adjde2 = adjde.x, rankadjde2 = rankadjde.x,
                          adjem2 = adjem.x, rankdajem2 = rankadjem.x,
                          seed2 = seed.x)
compact_21 %<>% transform(team1 = wteamid, score1 = wscore,
                          team2 = lteamid, score2 = lscore,
                          team1Home = homevec(wloc), orig = 0,
                          tempo1 = tempo.x, ranktempo1 = ranktempo.x,
                          adjtempo1 = adjtempo.x, rankadjtempo1 = rankadjtempo.x,
                          oe1 = oe.x, rankoe1 = rankoe.x,
                          adjoe1 = adjoe.x, rankadjoe1 = rankadjoe.x,
                          de1 = de.x, rankde1 = rankde.x,
                          adjde1 = adjde.x, rankadjde1 = rankadjde.x,
                          adjem1 = adjem.x, rankdajem1 = rankadjem.x,
                          seed1 = seed.x,
                          tempo2 = tempo.y, ranktempo2 = ranktempo.y,
                          adjtempo2 = adjtempo.y, rankadjtempo2 = rankadjtempo.y,
                          oe2 = oe.y, rankoe2 = rankoe.y,
                          adjoe2 = adjoe.y, rankadjoe2 = rankadjoe.y,
                          de2 = de.y, rankde2 = rankde.y,
                          adjde2 = adjde.y, rankadjde2 = rankadjde.y,
                          adjem2 = adjem.y, rankdajem2 = rankadjem.y,
                          seed2 = seed.y)
compact_22 %<>% transform(team1 = lteamid, score1 = lscore,
                          team2 = wteamid, score2 = wscore,
                          team1Home = homevec(wloc), orig = 0,
                          tempo1 = tempo.y, ranktempo1 = ranktempo.y,
                          adjtempo1 = adjtempo.y, rankadjtempo1 = rankadjtempo.y,
                          oe1 = oe.y, rankoe1 = rankoe.y,
                          adjoe1 = adjoe.y, rankadjoe1 = rankadjoe.y,
                          de1 = de.y, rankde1 = rankde.y,
                          adjde1 = adjde.y, rankadjde1 = rankadjde.y,
                          adjem1 = adjem.y, rankdajem1 = rankadjem.y,
                          seed1 = seed.y,
                          tempo2 = tempo.x, ranktempo2 = ranktempo.x,
                          adjtempo2 = adjtempo.x, rankadjtempo2 = rankadjtempo.x,
                          oe2 = oe.x, rankoe2 = rankoe.x,
                          adjoe2 = adjoe.x, rankadjoe2 = rankadjoe.x,
                          de2 = de.x, rankde2 = rankde.x,
                          adjde2 = adjde.x, rankadjde2 = rankadjde.x,
                          adjem2 = adjem.x, rankdajem2 = rankadjem.x,
                          seed2 = seed.x)
compact <- rbind(compact_11, compact_12,
                 compact_21, compact_22)
compact %<>% group_by(season,
                      team1) %>%
  arrange(daynum) %>%
  mutate(total_offense = cumsum(score1) - score1,
         total_defense = cumsum(score2) - score2,
         n1 = cumsum(periods) - periods,
         off_eff = total_offense / n1,
         def_eff = total_defense / n1) %>%
  ungroup() %>%
  group_by(season,
           team2) %>%
  mutate(opp_offense = cumsum(score2) - score2,
         opp_defense = cumsum(score1) - score1,
         n2 = cumsum(periods) - periods,
         opp_off = opp_offense / n2,
         opp_def = opp_defense / n2) %>%
  ungroup() %>%
  mutate(adj_off_eff = off_eff / opp_def,
         adj_def_eff = def_eff / opp_off)

final_metrics <- group_by(compact,
                          season,
                          team1) %>%
  filter(daynum == max(daynum)) %>%
  ungroup() %>%
  select(season, team1,
         ends_with('1'),
         ends_with('ff'))

final <- rbind(tourney_results,
               nit_results,
               conference_results,
               fill = T)

samp <- sample(1:nrow(final), nrow(final)/2)

tourney_11 <- final[samp,]
tourney_12 <- final[-samp,]
tourney_21 <- final[-samp,]
tourney_22 <- final[samp,]


tourney_11 %<>% transform(team1 = wteamid, score1 = wscore,
                          team2 = lteamid, score2 = lscore,
                          team1Home = homevec(wloc), orig = 1)
tourney_12 %<>% transform(team1 = lteamid, score1 = lscore,
                          team2 = wteamid, score2 = wscore,
                          team1Home = homevec(wloc), orig = 1)
tourney_21 %<>% transform(team1 = wteamid, score1 = wscore,
                          team2 = lteamid, score2 = lscore,
                          team1Home = homevec(wloc), orig = 0)
tourney_22 %<>% transform(team1 = lteamid, score1 = lscore,
                          team2 = wteamid, score2 = wscore,
                          team1Home = homevec(wloc), orig = 0)

tourney <- rbind(tourney_11, tourney_12,
                 tourney_21, tourney_22)
tourney[, win := (wteamid == team1)]

tourney_data <- merge(tourney,
                      final_metrics,
                      by = c('season', 'team1'))
tourney_full <- merge(tourney_data,
                      final_metrics,
                      by.x = c('season', 'team2'),
                      by.y = c('season', 'team1'))
tourney_full

seasons <- lapply(unique(tourney_full$season)[-1],
                  function(s) {
                    d <- filter(tourney_full,
                                season < s)
                    test <- filter(tourney_full,
                                   season == s)
                    
                    mod <- glm(data = d,
                               as.factor(win) ~
                                 log(off_eff.x) +
                                 log(def_eff.x) +
                                 log(off_eff.y) +
                                 log(def_eff.y) +
                                 log(adj_off_eff.x) + log(adj_def_eff.x) +
                                 log(adj_off_eff.y) + log(adj_def_eff.y),
                               family = 'binomial')
                    
                    test %<>% mutate(pred1 = predict(mod,
                                                     newdata = test,
                                                     type = 'response'))
                    test
                    #mod
                  })
#mean(unlist(seasons))
seasons
#summary(seasons[[33]])

final_model <- glm(data = tourney_full,
                   win ~
                     log(off_eff.x) +
                     log(def_eff.x) +
                     log(off_eff.y) +
                     log(def_eff.y) +
                     log(adj_off_eff.x) + log(adj_def_eff.x) +
                     log(adj_off_eff.y) + log(adj_def_eff.y),
                   family = 'binomial')
summary(final_model)

rf_list <- lapply(2003:2018,
                  function(s) {
                    d <- filter(tourney_full,
                                season < s,
                                season > 2001)
                    test <- filter(tourney_full,
                                   season == s)
                    rf <- randomForest(data = d,
                                       as.factor(win) ~
                                         tempo1.x + adjtempo1.x +
                                         oe1.x + adjoe1.x +
                                         de1.x + adjde1.x +
                                         adjem1.x +
                                         tempo1.y + adjtempo1.y +
                                         oe1.y + adjoe1.y +
                                         de1.y + adjde1.y +
                                         adjem1.y,
                                       ntree = 220)
                    test %<>% mutate(pred2 = predict(rf,
                                                     newdata = test,
                                                     type = 'prob')[,2])
                    #LogLoss(test$pred2, test$win)
                    #mod
                    test
                  })
# mean(unlist(rf_list))

final_rf <- randomForest(data = filter(tourney_full,
                                       season > 2001),
                         as.factor(win) ~
                           tempo1.x + adjtempo1.x +
                           oe1.x + adjoe1.x +
                           de1.x + adjde1.x +
                           adjem1.x +
                           tempo1.y + adjtempo1.y +
                           oe1.y + adjoe1.y +
                           de1.y + adjde1.y +
                           adjem1.y,
                         ntree = 220,
                         importance = T)
final_rf$importance
varImpPlot(final_rf)
plot(final_rf)

test_set <- merge(bind_rows(seasons[18:33]),
                  bind_rows(rf_list),
                  by = c('season', 'team1', 'team2'))

blend_fn_logloss <- function(ab) {
  blend_pred <- ab*test_set$pred1 + (1-ab)*test_set$pred2
  return(LogLoss(blend_pred, test_set$win.x))
}

optimize(blend_fn_logloss, c(0, 1))

o <- optimize(blend_fn_logloss, c(0, 1))$minimum
o

##### Predict Final Results #####

