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

teams <- gr19 %>% select(team=home_team, id=home_id, conf=home_conference) %>% 
  bind_rows(gr19 %>% select(team=away_team, id=away_id, conf=away_conference)) %>% 
  unique %>% 
  arrange(id) %>% 
  mutate(id2=1:n())
teams
# Make sure each team only has one conference listed
stopifnot(all((teams %>% group_by(team) %>% tally %>% pull(n)) == 1))
teams %>% filter(is.na(conf)) %>% pull(team)
# Check that all FBS teams have many games, only FCS should have few games
teams %>% left_join(gamesplayed) %>% with(table(is.na(conf), N))

teams$isFBS <- !is.na(teams$conf)
teams$beta_shape1 <- ifelse(teams$isFBS, 2, 1)
teams$beta_shape2 <- ifelse(teams$isFBS, 2, 7)
teams

gr19b <- gr19 %>% 
  left_join(teams %>% select(home_id=id, home_id2=id2), "home_id") %>% 
  left_join(teams %>% select(away_id=id, away_id2=id2), "away_id")

# gr19b <- bind_rows(gr19b, gr19b, gr19b, gr19b, gr19b)
# gr19b %>% dim

stopifnot(all(teams$id2 == 1:nrow(teams)))
gr19datlist <- list(
  Ngames = nrow(gr19b),
  Nteams = length(teams$id2),
  home_id = gr19b$home_id2,
  away_id = gr19b$away_id2,
  home_win = gr19b$home_win %>% as.integer(),
  neutral_site = gr19b$neutral_site %>% as.integer(),
  beta_shape1 = teams$beta_shape1,
  beta_shape2 = teams$beta_shape2
)


timestamp()
gr19out <- rstan::stan("./GameResults_winpct_1.stan", data=gr19datlist) #, include=T, pars=c("team_strength", "HFA_logodds"))
timestamp()
# gr19out
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
sumteam %>% ggplot(aes(mean, fill=is.na(conf))) + geom_histogram(alpha=.6) + facet_grid(is.na(conf)~.)

source('~/Documents/GitHub/CollegeFootball/R/GetLogoURL.R')
source('~/Documents/GitHub/CollegeFootball/R/PlotIntervals.R')
plot_intervals(sumteam %>% sample_n(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% head(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% tail(15) %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
# plot_intervals(sumteam %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
#                'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(conf=="SEC") %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")
plot_intervals(sumteam %>% filter(conf=="Sun Belt") %>% arrange((mean)) %>% mutate(logo=schoolnametologo(team)), 
               'mean', '`2.5%`', '`97.5%`', yname="team", imgurl = "logo")

sumtib %>% filter(varname=="HFA_logodds")
HFA_logodds <- sumtib %>% filter(varname=="HFA_logodds") %>% pull(mean)
HFA_logodds
curve(1/(1+exp(-HFA_logodds)*(1-x)/x)); abline(a=0,b=1,col=2)
curve(1/(1+exp(-HFA_logodds)*(1-x)/x) - x); 
sumtib %>% filter(varname=="stretch_mult")
stretch_mult <- sumtib %>% filter(varname=="stretch_mult") %>% pull(mean)
stretch_mult
sumtib %>% filter(varname=="shrink_factor")
shrink_factor <- sumtib %>% filter(varname=="shrink_factor") %>% pull(mean)
shrink_factor

source("./R/log5rule.R")
gr19c <- gr19b %>% 
  left_join(sumteam %>% select(home_id=id, home_team_strength=mean), "home_id") %>% 
  left_join(sumteam %>% select(away_id=id, away_team_strength=mean), "away_id") %>% 
  mutate(home_win_prob_noHFA=log5rule(home_team_strength, away_team_strength),
         home_win_prob_before = 1/(1+exp(-HFA_logodds*(!neutral_site)) * (1/home_win_prob_noHFA - 1)),
         home_win_prob = 1/(1+exp(-stretch_mult * log(home_win_prob_before/(1-home_win_prob_before))))*shrink_factor + 
           (1-shrink_factor)/2,
         resid = home_win_prob - home_win)
gr19c %>% filter(home_team=="LSU") %>% pw
# See if predictions are well calibrated
ggplot(gr19c, aes(home_win_prob, as.integer(home_win))) + geom_point() + stat_smooth() + geom_abline(slope=1,intercept=0,color='red')
ggplot(gr19c, aes(home_win_prob, resid)) + geom_point() + stat_smooth()
ggplot(gr19c, aes(home_team_strength, resid)) + geom_point() + stat_smooth()
ggplot(gr19c, aes(away_team_strength, resid)) + geom_point() + stat_smooth()
gr19c %>% ggplot(aes(home_team_strength, away_team_strength, fill=home_win_prob)) + geom_tile()

gr19c$home_win_prob %>% hist

sumteam %>% arrange(-mean) %>% mutate(r95=`97.5%` - `2.5%`) %>% arrange(desc(r95)) %>% ggplot(aes(mean, r95)) + geom_point()
# Higher range means fewer games played
sumteam %>% arrange(-mean) %>% mutate(r95=`97.5%` - `2.5%`) %>% arrange(desc(r95)) %>% ggplot(aes(N, r95)) + geom_point()


# # Find win prob based on point differential.
# gr19c %>% ggplot(aes(pt_diff, home_points > away_points)) + geom_point() + stat_smooth()
# gr19c %>% ggplot(aes(pred_pt_diff, as.integer(home_points > away_points))) + geom_point() + stat_smooth()
# winprobmod <- glm(data=gr19c %>% mutate(home_win=home_points > away_points),
#                   formula = home_win ~ pred_pt_diff, family=binomial())
# winprobmod
# # HFA is 2.2 points, but is makes win prob up to 56%
# predict(winprobmod, data.frame(pred_pt_diff=0), type='response')
# predict(winprobmod, data.frame(pred_pt_diff=-1.5), type='response')
# curve(predict(winprobmod, data.frame(pred_pt_diff=x), type='response'), from=-40, to=40, ylab="Win prob")
# gr19c %>% ggplot(aes(pred_pt_diff, as.integer(home_points > away_points))) + geom_point() + 
#   stat_smooth() + 
#   stat_function(fun=function(x){predict(winprobmod, data.frame(pred_pt_diff=x), type='response')},color='red')


sumgame <- sumtib %>% filter(stringi::stri_startswith(sumtib$varname, fixed="home_win_prob[")) %>% 
  mutate(home_id2=gr19datlist$home_id, away_id2=gr19datlist$away_id,
         home_win=gr19datlist$home_win) %>%
  # mutate(id2=as.integer(substring(varname, first=nchar("home_win_prob[")+1, last=nchar(varname)-1))) %>% 
  left_join(teams %>% select(home_team=team, home_id=id, home_id2=id2), 'home_id2') %>% 
  left_join(teams %>% select(away_team=team, away_id=id, away_id2=id2), 'away_id2') #%>% 
  # arrange(-mean) %>%
  # left_join(gamesplayed, c("team", "id"))
sumgame
sumgame %>% ggplot(aes(mean, home_win)) + geom_point() + stat_smooth()
sumgame2 <- bind_rows(sumgame %>% select(winprob=mean, win=home_win,   id2=home_id2, team=home_team),
          sumgame %>% transmute(winprob=1-mean, win=!home_win, id2=away_id2, team=away_team))
sumgame2 %>% filter(team=="Vanderbilt")
sumgame2team <- sumgame2 %>% group_by(id2, team) %>% summarize(winprob=mean(winprob), win=mean(win), N=n())
sumgame2team %>% ggplot(aes(winprob, win, size=N)) + geom_point() + geom_abline(slope=1,intercept = 0, color='red')

sumgame2bin <- sumgame2 %>% mutate(win_prob_rd=round(winprob*50)/50) %>% group_by(win_prob_rd) %>% 
  summarize(N=n(), prop=mean(win))
sumgame2bin
sumgame2bin %>% ggplot(aes(win_prob_rd, prop, size=N)) + geom_point() + stat_smooth() + geom_abline(slope=1,intercept=0,color='red')

# Heat map of residuals by home/away team strength. Not useful
gr19c %>% mutate(home_team_strength=round(5*home_team_strength)/5, away_team_strength=round(5*away_team_strength)/5) %>% 
  group_by(home_team_strength, away_team_strength) %>% summarize(home_win_prob=mean(home_win_prob), resid=mean(resid), N=n()) %>% 
  mutate(Nalpha=N/max(N)) %>% filter(N>20) %>%  
  ggplot(aes(home_team_strength, away_team_strength, fill=resid)) + geom_tile(aes(alpha=Nalpha)) + 
  scale_fill_gradientn(colors=c('red','green'))
