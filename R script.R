library(ggplot2)
library(haven)
library(owtheme)
library(pano)
library(tidyr)
library(ggalluvial)
library(dplyr)
library(pollster)

bes <- read_dta("BES2019_W21_Panel_v21.0.dta")
bes$onscode <- constid(bes$panoW21, "pa_id", "ons_id", warn = TRUE)

irrelevant_waves <- c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11",
                      "W12","W13","W14","W15","W16","W17","W18","W19","W20")

current <- endsWith(colnames(bes),"W1")

for (wave in irrelevant_waves){
  new <- endsWith(colnames(bes),wave)
  current <- rowAny(cbind(current,new))
}

bes <- bes[,!current]

remove <- c("E14000625","E14000667","E14000668","E14000669","E14000848",
            "E14001006","E14000853",NA)

sample_sizes <- bes %>% summarise(london = sum(!is.na(londonFirstChoiceW21)),
                                  westMid = sum(!is.na(westMidFirstChoiceW21)),
                                  westEng = sum(!is.na(westEngFirstChoiceW21)),
                                  gm = sum(!is.na(gmFirstChoiceW21)),
                                  liverpoolRegion = sum(!is.na(liverpoolRegionFirstChoiceW21)),
                                  cambridge = sum(!is.na(cambridgeFirstChoiceW21)),
                                  tees = sum(!is.na(teesFirstChoiceW21)),
                                  westYork = sum(!is.na(westYorkFirstChoiceW21)))

bes$vote2019 <- NA
bes$vote2019[which(!is.na(bes$p_past_vote_2019))] <- "Other"
bes$vote2019[which((bes$p_past_vote_2019==1))] <- "Conservative"
bes$vote2019[which((bes$p_past_vote_2019==2))] <- "Labour"
bes$vote2019[which((bes$p_past_vote_2019==3))] <- "Liberal Democrat"
bes$vote2019[which((bes$p_past_vote_2019==7))] <- "Green Party"
bes$vote2019[which((bes$p_turnout_2019==0))] <- "Did Not Vote"

bes$metroMayorVote <- NA
bes$metroMayorVote[which(bes$londonTurnoutW21==2)] <- "Did Not Vote"
bes$metroMayorVote[which(bes$mayorTurnoutW21==0)] <- "Did Not Vote"

bes$metroMayorVote[which(bes$londonFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$londonFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$londonFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$londonFirstChoiceW21==7)] <- "Green Party"
bes$metroMayorVote[which(bes$londonFirstChoiceW21==9)] <- "Other"

bes$metroMayorVote[which(bes$gmFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$gmFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$gmFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$gmFirstChoiceW21==7)] <- "Green Party"
bes$metroMayorVote[which(bes$gmFirstChoiceW21==12)] <- "Other"
bes$metroMayorVote[which((bes$gmFirstChoiceW21>12)&(bes$gmFirstChoiceW21<999))] <- "Other"

bes$metroMayorVote[which(bes$teesFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$teesFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote[which(bes$westEngFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$westEngFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$westEngFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$westEngFirstChoiceW21==7)] <- "Green Party"

bes$metroMayorVote[which(bes$westMidFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$westMidFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$gmFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$westMidFirstChoiceW21==7)] <- "Green Party"
bes$metroMayorVote[which(bes$westMidFirstChoiceW21==12)] <- "Other"

bes$metroMayorVote[which(bes$westYorkFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$westYorkFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$westYorkFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$westYorkFirstChoiceW21==7)] <- "Green Party"
bes$metroMayorVote[which(bes$westYorkFirstChoiceW21==12)] <- "Other"
bes$metroMayorVote[which((bes$westYorkFirstChoiceW21>12)&(bes$westYorkFirstChoiceW21<999))] <- "Other"

bes$metroMayorVote[which(bes$cambridgeFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$cambridgeFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$cambridgeFirstChoiceW21==3)] <- "Liberal Democrat"

bes$metroMayorVote[which(bes$liverpoolRegionFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote[which(bes$liverpoolRegionFirstChoiceW21==2)] <- "Labour"
bes$metroMayorVote[which(bes$liverpoolRegionFirstChoiceW21==3)] <- "Liberal Democrat"
bes$metroMayorVote[which(bes$liverpoolRegionFirstChoiceW21==7)] <- "Green Party"

#2nd Round

bes$metroMayorVote_2 <- NA
bes$metroMayorVote_2[which(bes$londonTurnoutW21==2)] <- "Did Not Vote"
bes$metroMayorVote_2[which(bes$mayorTurnoutW21==0)] <- "Did Not Vote"

bes$metroMayorVote_2[which(bes$londonSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$londonSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$londonSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$londonFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$londonFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$gmSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$gmSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$gmSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$gmFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$gmFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$teesFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$teesFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$westEngSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$westEngSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westEngSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$westEngFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westEngFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$westMidSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$westMidSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westMidSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$westMidFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westMidFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$westYorkSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$westYorkSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westYorkSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$westYorkFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$westYorkFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$cambridgeSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$cambridgeSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$cambridgeSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$cambridgeFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$cambridgeFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote_2[which(bes$liverpoolRegionSecondChoiceW21>2)] <- "Wasted"
bes$metroMayorVote_2[which(bes$liverpoolRegionSecondChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$liverpoolRegionSecondChoiceW21==2)] <- "Labour"
bes$metroMayorVote_2[which(bes$liverpoolRegionFirstChoiceW21==1)] <- "Conservative"
bes$metroMayorVote_2[which(bes$liverpoolRegionFirstChoiceW21==2)] <- "Labour"

bes$metroMayorVote[which(bes$onscode %in% remove)] <- NA
bes$metroMayorVote_2[which(bes$onscode %in% remove)] <- NA

freq <- bes[which((!is.na(bes$vote2019))&(!is.na(bes$metroMayorVote))&(!is.na(bes$metroMayorVote_2))),] %>% group_by(vote2019, metroMayorVote, metroMayorVote_2) %>%
  summarise(n = sum(wt_new_W21))

freq_lodes <- to_lodes_form(freq, axes=c(1:3))

levels(freq_lodes$x)[levels(freq_lodes$x)=="vote2019"] <- "2019 General Election"
levels(freq_lodes$x)[levels(freq_lodes$x)=="metroMayorVote"] <- "Mayoral 1st Round"
levels(freq_lodes$x)[levels(freq_lodes$x)=="metroMayorVote_2"] <- "Mayoral 2nd Round"

plot <- ggplot(freq_lodes, aes(x=x,y=n,stratum=stratum,alluvium=alluvium,fill=stratum)) +
  geom_flow(size=0,width = 1/12) +
  geom_stratum(width=1/12) +
  scale_fill_manual(values=c(ow_cols("cons","light blue"),ow_cols("green","lab","ld"),"grey","black")) +
  ggtitle("Vote switching between the 2019 General Election\nand 2021 Metro Mayor Elections") +
  theme_ow() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank())

finalise_plot(plot, "Data from British Election Study (2021)", save_filepath="overall.png")

# LONDON

london <- bes[which(bes$gorW22==7),]

london$vote2019 <- NA
london$vote2019[which(!is.na(london$p_past_vote_2019))] <- "Other"
london$vote2019[which((london$p_past_vote_2019==1))] <- "Conservative"
london$vote2019[which((london$p_past_vote_2019==2))] <- "Labour"
london$vote2019[which((london$p_past_vote_2019==3))] <- "Liberal Democrat"
london$vote2019[which((london$p_past_vote_2019==7))] <- "Green Party"
london$vote2019[which((london$p_turnout_2019==0))] <- "Did Not Vote"

#London Mayoral

london$vote2021 <- NA
london$vote2021[which(london$londonTurnoutW21==2)] <- "Did Not Vote"
london$vote2021[which(london$londonFirstChoiceW21==9)] <- "Other"
london$vote2021[which((london$londonFirstChoiceW21==1))] <- "Conservative"
london$vote2021[which((london$londonFirstChoiceW21==2))] <- "Labour"
london$vote2021[which((london$londonFirstChoiceW21==3))] <- "Liberal Democrat"
london$vote2021[which((london$londonFirstChoiceW21==7))] <- "Green Party"

london$vote2021_2 <- NA
london$vote2021_2[which(london$londonTurnoutW21==2)] <- "Did Not Vote"
london$vote2021_2[which(london$londonSecondChoiceW21>2)] <- "Wasted"
london$vote2021_2[which(london$londonSecondChoiceW21==1)] <- "Conservative"
london$vote2021_2[which(london$londonSecondChoiceW21==2)] <- "Labour"
london$vote2021_2[which(london$londonFirstChoiceW21==1)] <- "Conservative"
london$vote2021_2[which(london$londonFirstChoiceW21==2)] <- "Labour"

london_freq <- london[which((!is.na(london$vote2019))&(!is.na(london$vote2021))&(!is.na(london$vote2021_2))),] %>% group_by(vote2019, vote2021, vote2021_2) %>%
  summarise(n = sum(wt_new_W21))

london_freq_lodes <- to_lodes_form(london_freq, axes=1:3)

levels(london_freq_lodes$x)[levels(london_freq_lodes$x)=="vote2019"] <- "2019 General Election"
levels(london_freq_lodes$x)[levels(london_freq_lodes$x)=="vote2021"] <- "1st Round"
levels(london_freq_lodes$x)[levels(london_freq_lodes$x)=="vote2021_2"] <- "2nd Round"

plot <- ggplot(london_freq_lodes, aes(x=x,y=n,stratum=stratum,alluvium=alluvium,fill=stratum)) +
  geom_flow(size=0,width = 1/12) +
  geom_stratum(width=1/12) +
  scale_fill_manual(values=c(ow_cols("cons","light blue"),ow_cols("green","lab","ld"),"grey","black")) +
  ggtitle("Vote switching between the 2019 General Election\nand 2021 London Mayoral Election") +
  theme_ow() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank())

finalise_plot(plot, "Data from British Election Study (2021)", save_filepath="london.png")

#West Mids Mayoral

westmids <- bes[which(bes$gorW22==5),]

westmids$vote2019 <- NA
westmids$vote2019[which(!is.na(westmids$p_past_vote_2019))] <- "Other"
westmids$vote2019[which((westmids$p_past_vote_2019==1))] <- "Conservative"
westmids$vote2019[which((westmids$p_past_vote_2019==2))] <- "Labour"
westmids$vote2019[which((westmids$p_past_vote_2019==3))] <- "Liberal Democrat"
westmids$vote2019[which((westmids$p_past_vote_2019==7))] <- "Green Party"
westmids$vote2019[which((westmids$p_turnout_2019==0))] <- "Did Not Vote"

westmids$vote2021 <- NA
westmids$vote2021[which(westmids$mayorTurnoutW21==0)] <- "Did Not Vote"
westmids$vote2021[which(westmids$westMidFirstChoiceW21==9)] <- "Other"
westmids$vote2021[which((westmids$westMidFirstChoiceW21==1))] <- "Conservative"
westmids$vote2021[which((westmids$westMidFirstChoiceW21==2))] <- "Labour"
westmids$vote2021[which((westmids$westMidFirstChoiceW21==3))] <- "Liberal Democrat"
westmids$vote2021[which((westmids$westMidFirstChoiceW21==7))] <- "Green Party"
westmids$vote2021[which((westmids$westMidFirstChoiceW21==12))] <- "Reform UK"

westmids$vote2021_2 <- NA
westmids$vote2021_2[which(westmids$mayorTurnoutW21==0)] <- "Did Not Vote"
westmids$vote2021_2[which(westmids$westMidSecondChoiceW21>2)] <- "Wasted"
westmids$vote2021_2[which(westmids$westMidSecondChoiceW21==1)] <- "Conservative"
westmids$vote2021_2[which(westmids$westMidSecondChoiceW21==2)] <- "Labour"
westmids$vote2021_2[which(westmids$westMidFirstChoiceW21==1)] <- "Conservative"
westmids$vote2021_2[which(westmids$westMidFirstChoiceW21==2)] <- "Labour"

westmids_freq <- westmids[which((!is.na(westmids$vote2019))&(!is.na(westmids$vote2021))&(!is.na(westmids$vote2021_2))),] %>% group_by(vote2019, vote2021, vote2021_2) %>%
  summarise(n = sum(wt_new_W21))

westmids_freq_lodes <- to_lodes_form(westmids_freq, axes=1:3)

levels(westmids_freq_lodes$x)[levels(westmids_freq_lodes$x)=="vote2019"] <- "2019 General Election"
levels(westmids_freq_lodes$x)[levels(westmids_freq_lodes$x)=="vote2021"] <- "1st Round"
levels(westmids_freq_lodes$x)[levels(westmids_freq_lodes$x)=="vote2021_2"] <- "2nd Round"

plot <- ggplot(westmids_freq_lodes, aes(x=x,y=n,stratum=stratum,alluvium=alluvium,fill=stratum)) +
  geom_flow(size=0,width = 1/12) +
  geom_stratum(width=1/12) +
  scale_fill_manual(values=c(ow_cols("cons","light blue","green","lab","ld"),"grey",ow_cols("reform"),"black")) +
  ggtitle("Vote switching between the 2019 General Election\nand 2021 West Midlands Mayoral Election") +
  theme_ow() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank())

finalise_plot(plot, "Data from British Election Study (2021)", save_filepath="westmids.png")

#Greater Manchester Mayoral

in_gm <- unique(bes$onscode[which((!is.na(bes$gmFirstChoiceW21))&(!is.na(bes$onscode)))])
gm <- bes[which((bes$gorW22==2)&(bes$onscode %in% in_gm)),]

gm$vote2019 <- NA
gm$vote2019[which(!is.na(gm$p_past_vote_2019))] <- "Other"
gm$vote2019[which((gm$p_past_vote_2019==1))] <- "Conservative"
gm$vote2019[which((gm$p_past_vote_2019==2))] <- "Labour"
gm$vote2019[which((gm$p_past_vote_2019==3))] <- "Liberal Democrat"
gm$vote2019[which((gm$p_past_vote_2019==7))] <- "Green Party"
gm$vote2019[which((gm$p_turnout_2019==0))] <- "Did Not Vote"

gm$vote2021 <- NA
gm$vote2021[which(gm$mayorTurnoutW21==0)] <- "Did Not Vote"
gm$vote2021[which(gm$gmFirstChoiceW21==9)] <- "Other"
gm$vote2021[which((gm$gmFirstChoiceW21==1))] <- "Conservative"
gm$vote2021[which((gm$gmFirstChoiceW21==2))] <- "Labour"
gm$vote2021[which((gm$gmFirstChoiceW21==3))] <- "Liberal Democrat"
gm$vote2021[which((gm$gmFirstChoiceW21==7))] <- "Green Party"
gm$vote2021[which((gm$gmFirstChoiceW21==12))] <- "Reform UK"
gm$vote2021[which((gm$gmFirstChoiceW21>12)&(gm$gmFirstChoiceW21<999))] <- "Other"

gm$vote2021_2 <- NA
gm$vote2021_2[which(gm$mayorTurnoutW21==0)] <- "Did Not Vote"
gm$vote2021_2[which(gm$gmSecondChoiceW21>2)] <- "Wasted"
gm$vote2021_2[which(gm$gmSecondChoiceW21==1)] <- "Conservative"
gm$vote2021_2[which(gm$gmSecondChoiceW21==2)] <- "Labour"
gm$vote2021_2[which(gm$gmFirstChoiceW21==1)] <- "Conservative"
gm$vote2021_2[which(gm$gmFirstChoiceW21==2)] <- "Labour"

gm_freq <- gm[which((!is.na(gm$vote2019))&(!is.na(gm$vote2021))&(!is.na(gm$vote2021_2))),] %>% group_by(vote2019, vote2021, vote2021_2) %>%
  summarise(n = sum(wt_new_W21))

gm_freq_lodes <- to_lodes_form(gm_freq, axes=1:3)

levels(gm_freq_lodes$x)[levels(gm_freq_lodes$x)=="vote2019"] <- "2019 General Election"
levels(gm_freq_lodes$x)[levels(gm_freq_lodes$x)=="vote2021"] <- "1st Round"
levels(gm_freq_lodes$x)[levels(gm_freq_lodes$x)=="vote2021_2"] <- "2nd Round"

plot <- ggplot(gm_freq_lodes, aes(x=x,y=n,stratum=stratum,alluvium=alluvium,fill=stratum)) +
  geom_flow(size=0,width = 1/12) +
  geom_stratum(width=1/12) +
  scale_fill_manual(values=c(ow_cols("cons","light blue","green","lab","ld"),"grey",ow_cols("reform"),"black")) +
  ggtitle("Vote switching between the 2019 General Election\nand 2021 Greater Manchester Mayoral Election") +
  theme_ow() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank())

finalise_plot(plot, "Data from British Election Study (2021)", save_filepath="gm.png")

#West Yorkshire Mayoral

in_westYork <- unique(bes$onscode[which((!is.na(bes$westYorkFirstChoiceW21))&(!is.na(bes$onscode)))])
westYork <- bes[which((bes$gorW22==3)&(bes$onscode %in% in_westYork)),]

westYork$vote2019 <- NA
westYork$vote2019[which(!is.na(westYork$p_past_vote_2019))] <- "Other"
westYork$vote2019[which((westYork$p_past_vote_2019==1))] <- "Conservative"
westYork$vote2019[which((westYork$p_past_vote_2019==2))] <- "Labour"
westYork$vote2019[which((westYork$p_past_vote_2019==3))] <- "Liberal Democrat"
westYork$vote2019[which((westYork$p_past_vote_2019==7))] <- "Green Party"
westYork$vote2019[which((westYork$p_turnout_2019==0))] <- "Did Not Vote"

westYork$vote2021 <- NA
westYork$vote2021[which(westYork$mayorTurnoutW21==0)] <- "Did Not Vote"
westYork$vote2021[which(westYork$westYorkFirstChoiceW21==9)] <- "Other"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==1))] <- "Conservative"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==2))] <- "Labour"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==3))] <- "Liberal Democrat"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==7))] <- "Green Party"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==12))] <- "Reform UK"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==14))] <- "Yorkshire Party"
westYork$vote2021[which((westYork$westYorkFirstChoiceW21==13))] <- "Other"

westYork$vote2021_2 <- NA
westYork$vote2021_2[which(westYork$mayorTurnoutW21==0)] <- "Did Not Vote"
westYork$vote2021_2[which(westYork$westYorkSecondChoiceW21>2)] <- "Wasted"
westYork$vote2021_2[which(westYork$westYorkSecondChoiceW21==1)] <- "Conservative"
westYork$vote2021_2[which(westYork$westYorkSecondChoiceW21==2)] <- "Labour"
westYork$vote2021_2[which(westYork$westYorkFirstChoiceW21==1)] <- "Conservative"
westYork$vote2021_2[which(westYork$westYorkFirstChoiceW21==2)] <- "Labour"

westYork_freq <- westYork[which((!is.na(westYork$vote2019))&(!is.na(westYork$vote2021))&(!is.na(westYork$vote2021_2))),] %>% group_by(vote2019, vote2021, vote2021_2) %>%
  summarise(n = sum(wt_new_W21))

westYork_freq_lodes <- to_lodes_form(westYork_freq, axes=1:3)

levels(westYork_freq_lodes$x)[levels(westYork_freq_lodes$x)=="vote2019"] <- "2019 General Election"
levels(westYork_freq_lodes$x)[levels(westYork_freq_lodes$x)=="vote2021"] <- "1st Round"
levels(westYork_freq_lodes$x)[levels(westYork_freq_lodes$x)=="vote2021_2"] <- "2nd Round"

plot <- ggplot(westYork_freq_lodes, aes(x=x,y=n,stratum=stratum,alluvium=alluvium,fill=stratum)) +
  geom_flow(size=0,width = 1/12) +
  geom_stratum(width=1/12) +
  scale_fill_manual(values=c(ow_cols("cons","light blue","green","lab","ld"),"grey",ow_cols("reform","light yellow"),"black")) +
  ggtitle("Vote switching between the 2019 General Election\nand 2021 West Yorkshire Mayoral Election") +
  theme_ow() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank())

finalise_plot(plot, "Data from British Election Study (2021)", save_filepath="westYork.png")
