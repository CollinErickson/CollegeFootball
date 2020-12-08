# CBE 12/8/2020


library(dplyr)
library(ggplot2)
library(cbe)
library(rstan)
library(readr)
gr19 <- read_csv(file="./data/GameResults/GameResults2019.csv") %>% 
  mutate(pt_diff=home_points - away_points,
         home_win = home_points > away_points)
gr19 %>% head
gr19 %>% pw

ggplot(gr19, aes(home_points, away_points)) + geom_point()
gr19 %>% group_by(home_points, away_points) %>% summarize(N=n()) %>% 
  ggplot(aes(home_points, away_points, size=N)) + geom_point() + scale_size_area()

gamesplayed <- gr19 %>% select(team=home_team, id=home_id) %>% 
  bind_rows(gr19 %>% select(team=away_team, id=away_id)) %>% group_by(team, id) %>% summarize(N=n())

teams <- gr19 %>% select(team=home_team, id=home_id) %>% 
  bind_rows(gr19 %>% select(team=away_team, id=away_id)) %>% 
  unique %>% 
  arrange(id) %>% 
  mutate(id2=1:n())
teams

gr19b <- gr19 %>% 
  left_join(teams %>% select(home_id=id, home_id2=id2), "home_id") %>% 
  left_join(teams %>% select(away_id=id, away_id2=id2), "away_id")


gr19datlist <- list(
  Ngames = nrow(gr19b),
  Nteams = length(teams$id2),
  home_id = gr19b$home_id2,
  away_id = gr19b$away_id2,
  home_win = gr19b$home_win %>% as.integer(),
  neutral_site = gr19b$neutral_site %>% as.integer()
)


timestamp()
gr19out <- rstan::stan("./GameResults19_2_winpct.stan", data=gr19datlist)
timestamp()
gr19out
plot(gr19out)

sumtib <- gr19out %>% summary %>% .[[1]] %>% {cbind(as_tibble(.), varname=rownames(.))} %>% as_tibble()
sumtib
sumtib %>% tail
sumtib %>% arrange(-mean)

sumteam <- sumtib %>% filter(stringi::stri_startswith(sumtib$varname, fixed="team_")) %>% 
  mutate(id2=as.integer(substring(varname, first=nchar("team_strength_")+1, last=nchar(varname)-1))) %>% 
  left_join(teams, 'id2') %>% arrange(-mean) %>% 
  left_join(gamesplayed, c("team", "id"))
sumteam
# stringi::stri_extract_first(sumtib$varname, regex="team_stren.*")
sumteam %>% tail

source('~/Documents/GitHub/CollegeFootball/R/GetLogoURL.R')
source('~/Documents/GitHub/CollegeFootball/R/PlotIntervals.R')
plot_intervals(sumteam %>% sample_n(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% head(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% tail(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")

HFA <- sumtib %>% filter(varname=="HFA") %>% pull(mean)
HFA

gr19c <- gr19b %>% 
  left_join(sumteam %>% select(home_id=id, home_team_strength=mean), "home_id") %>% 
  left_join(sumteam %>% select(away_id=id, away_team_strength=mean), "away_id") %>% 
  mutate(pred_pt_diff=home_team_strength - away_team_strength + HFA*(!neutral_site))
# See if predictions are well calibrated
ggplot(gr19c, aes(pred_pt_diff, pt_diff)) + geom_point() + geom_abline(slope=1, intercept=0, color="red") + stat_smooth()


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


