# CBE 12/14/2020


library(dplyr)
library(ggplot2)
library(cbe)
library(rstan)
library(readr)
years <- 2019:2020
gr19 <- lapply(years, function(year) {read_csv(file=paste0("./data/GameResults/GameResults",year,".csv"))}) %>%
  bind_rows() %>% 
  mutate(pt_diff=home_points - away_points)
gr19 %>% head
gr19 %>% tail
gr19 %>% pw

# ggplot(gr19, aes(home_points, away_points)) + geom_point()
gr19 %>% group_by(home_points, away_points) %>% summarize(N=n()) %>% 
  ggplot(aes(home_points, away_points, size=N)) + geom_point() + scale_size_area()

gamesplayed <- gr19 %>% select(team=home_team, id=home_id, season) %>% 
  bind_rows(gr19 %>% select(team=away_team, id=away_id, season)) %>% group_by(team, id, season) %>% 
  summarize(N=n()) %>% ungroup

confyears <- gr19 %>% select(conf=home_conference, season=season) %>% 
  bind_rows(gr19 %>% select(conf=away_conference, season=season)) %>% 
  unique %>% 
  mutate(conf=ifelse(is.na(conf), "FCS", conf)) %>% arrange(season, conf) %>% 
  mutate(confid=1:n()) %>% 
  {left_join(., mutate(., season=season+1), by=c("conf", "season"), suffix=c("", "_prev"))}
confyears %>% pn
# confs <- tibble(conf=c(gr19$home_conference, gr19$away_conference, "FCS") %>% unique) %>% 
#   filter(!is.na(conf)) %>% 
#   mutate(confid=1:n())
# confs

teamyears <- gr19 %>% select(team=home_team, id=home_id, conf=home_conference, season) %>% 
  bind_rows(gr19 %>% select(team=away_team, id=away_id, conf=away_conference, season)) %>% 
  unique %>% 
  arrange(id) %>% 
  mutate(tyid=1:n(), conf=ifelse(is.na(conf), "FCS", conf)) %>% 
  left_join(confyears, c("conf", "season"))
teamyears
# teams <- teamyears %>% select(team, id) %>% unique
# Make sure each team only has one conference listed
stopifnot(all((teamyears %>% group_by(team, season) %>% tally %>% pull(n)) == 1))
teamyears %>% filter(conf == "FCS") %>% pull(team)
# Check that all FBS teams have many games, only FCS should have few games
teamyears %>% left_join(gamesplayed) %>% with(table(conf == "FCS", N))
gamesplayed %>% left_join(teamyears, c("team", 'id', 'season')) %>% filter(conf=="FCS") %>% arrange(-N)

gr19b <- gr19 %>% 
  left_join(teamyears %>% select(home_id=id, home_id2=tyid, home_confid=confid, season), c("home_id", "season")) %>% 
  left_join(teamyears %>% select(away_id=id, away_id2=tyid, away_confid=confid, season), c("away_id", "season"))
stopifnot(nrow(gr19b) == nrow(gr19))
gr19b

games_include <- !is.na(gr19b$pt_diff)
gr19datlist <- list(
  Ngames_obs = sum(games_include), #nrow(gr19b),
  Ngames_mis = sum(!games_include), #nrow(gr19b),
  Nteams = nrow(teamyears), #length(teams$id2),
  Nconfs = nrow(confyears), #nrow(confs),
  home_id_obs = gr19b$home_id2[games_include],
  away_id_obs = gr19b$away_id2[games_include],
  home_id_mis = gr19b$home_id2[!games_include],
  away_id_mis = gr19b$away_id2[!games_include],
  # home_confid = gr19b$home_confid,
  # away_confid = gr19b$away_confid,
  team_confid = teamyears$confid, #teams$confid,
  pt_diff_obs = gr19b$pt_diff[games_include],
  neutral_site_obs = gr19b$neutral_site[games_include] %>% as.integer(),
  neutral_site_mis = gr19b$neutral_site[!games_include] %>% as.integer()
)


timestamp()
gr19out <- rstan::stan("./GameResults_ptdiff_4_predict.stan", data=gr19datlist)
timestamp()
# gr19out
plot(gr19out)

sumtib <- gr19out %>% summary %>% .[[1]] %>% {cbind(as_tibble(.), varname=rownames(.))} %>% as_tibble()
sumtib
sumtib %>% tail
sumtib %>% arrange(n_eff)
sumtib %>% arrange(-mean)

sumteam <- sumtib %>% filter(stringi::stri_startswith(sumtib$varname, fixed="team_strength[")) %>% 
  mutate(tyid=as.integer(substring(varname, first=nchar("team_strength[")+1, last=nchar(varname)-1))) %>% 
  left_join(teamyears, 'tyid') %>% arrange(-mean) %>% 
  left_join(gamesplayed, c("team", "id", 'season'))
sumteam
sumteam %>% tail
sumteamconf <- sumtib %>% filter(stringi::stri_startswith(sumtib$varname, fixed="team_strength_around_conf[")) %>% 
  mutate(tyid=as.integer(substring(varname, first=nchar("team_strength_around_conf[")+1, last=nchar(varname)-1))) %>% 
  left_join(teamyears, 'tyid') %>% arrange(-mean) %>% 
  left_join(gamesplayed, c("team", "id", 'season'))


sumconf <- sumtib %>% filter(stringi::stri_startswith(sumtib$varname, fixed="conf_strength")) %>% 
  mutate(confid=as.integer(substring(varname, first=nchar("conf_strength_")+1, last=nchar(varname)-1))) %>% 
  left_join(confyears, 'confid') %>% arrange(-mean)
sumconf %>% pnw

source("./R/PlotIntervals.R"); source("./R/GetLogoURL.R")
plot_intervals(sumteam %>% sample_n(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(season==2019) %>% head(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(season==2020) %>% head(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% tail(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
# plot_intervals(sumteam %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
#                'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(season==2020,conf=="SEC") %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteamconf %>% filter(season==2020, conf=="SEC") %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(conf=="Big Ten") %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumconf %>% arrange((mean)), 
               'mean', '`2.5%`', '`97.5%`', yname="conf")

gr19b[games_include,] %>% filter(home_team=="Alabama") %>% select(home_team, home_points, away_team, away_points)
gr19b[games_include,] %>% filter(away_team=="Alabama") %>% select(home_team, home_points, away_team, away_points)

HFA <- sumtib %>% filter(varname=="HFA") %>% pull(mean)
HFA

gr19c <- gr19b %>% 
  left_join(sumteam %>% select(home_id=id, home_team_strength=mean), "home_id") %>% 
  left_join(sumteam %>% select(away_id=id, away_team_strength=mean), "away_id") %>% 
  mutate(pred_pt_diff=home_team_strength - away_team_strength + HFA*(!neutral_site))
# See if predictions are well calibrated. Looks good. Residuals are approx normal.
ggplot(gr19c, aes(pred_pt_diff, pt_diff)) + geom_point() + geom_abline(slope=1, intercept=0, color="red") + stat_smooth()
ggplot(gr19c, aes(pred_pt_diff - pt_diff)) + geom_histogram() #+ geom_abline(slope=1, intercept=0, color="red") + stat_smooth()
ggplot(gr19c, aes(pred_pt_diff,pred_pt_diff - pt_diff)) + geom_point() + geom_abline(slope=0, intercept=0, color="red") + stat_smooth()

# RMSE of 12.77 points
with(gr19c, sqrt(mean((pred_pt_diff - pt_diff)^2)))

sumteam %>% arrange(-mean) %>% mutate(r95=`97.5%` - `2.5%`) %>% arrange(desc(r95)) %>% ggplot(aes(mean, r95)) + geom_point()
# Higher range means fewer games played
sumteam %>% arrange(-mean) %>% mutate(r95=`97.5%` - `2.5%`) %>% arrange(desc(r95)) %>% ggplot(aes(N, r95)) + geom_point()


# Find win prob based on point differential.
gr19c %>% ggplot(aes(pt_diff, home_points > away_points)) + geom_point() + stat_smooth()
gr19c %>% ggplot(aes(pred_pt_diff, as.integer(home_points > away_points))) + geom_point() + stat_smooth()
winprobmod <- glm(data=gr19c %>% mutate(home_win=home_points > away_points),
    formula = home_win ~ pred_pt_diff, family=binomial())
winprobmod
# HFA is 2.2 points, but is makes win prob up to 56%
predict(winprobmod, data.frame(pred_pt_diff=0), type='response')
predict(winprobmod, data.frame(pred_pt_diff=-1.5), type='response')
curve(predict(winprobmod, data.frame(pred_pt_diff=x), type='response'), from=-40, to=40, ylab="Win prob")
gr19c %>% ggplot(aes(pred_pt_diff, as.integer(home_points > away_points))) + geom_point() + 
  stat_smooth() + 
  stat_function(fun=function(x){predict(winprobmod, data.frame(pred_pt_diff=x), type='response')},color='red')


