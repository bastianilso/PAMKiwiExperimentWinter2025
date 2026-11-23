library(plotly)
library(tidyverse)
library(gsheet)
#library(lme4)
#library(MuMIn)
options("digits.secs"=6)
options(max.print=1000)

source("utils/visutils.R")
source("utils/beeswarm_values.R")

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

# Make PDF export work
reticulate::use_virtualenv("/home/bastianilso/R/python-env-plotly/", required=T)


load('data_all.rda')

#fig %>% add_trace(data=St, x=~PercNormalized, y=~FunNormalized,type="scatter",mode="markers") %>%
 # add_trace(data=p_lin(St, "FunNormalized","PercNormalized"), x=~x,y=~y,mode="lines",type="scatter")

#############
# Summaries
#############

St <- D %>% group_by(Participant, Condition) %>% 
  filter(Event == "GameDecision") %>%
  summarise(Participant = unique(Participant),
            Game = unique(Game),
            ConditionLabel = unique(ConditionLabel),
            Gender = unique(Gender),
            Condition = unique(Condition),
            Order = unique(Order),
            rejInput = sum(TrialResult == "RejInput", na.rm=T),
            accInput = sum(TrialResult == "AccInput", na.rm=T),
            helpInput = sum(TrialResult %in% c("FabInput","AugSuccess","OverrideInput","MitigateFail"), na.rm=T),
            totalTrials = sum(!is.na(TrialResult), na.rm=T),
            rej_rate = rejInput / totalTrials,
            acc_rate = accInput / totalTrials,
            help_rate = helpInput / totalTrials,
            acchelp_rate = acc_rate+help_rate,
            PercNormalized = unique((`Hvor meget kontrol over spillet havde du i denne omgang?`-1)/6),
            ActionPercNormalized = unique((`Jeg følte jeg have kontrol over hvornår på trampolinen kiwi fuglen hoppede.`-1)/6),
            FrustNormalized = unique((`Hvor frustreret følte du dig da du spillede denne omgang, alt i alt?`-1)/6),
            FunNormalized = unique((`Det var sjovt at spille`-1)/6)
            )

St <- St %>% arrange(Participant,Game,Order) %>%
  group_by(Participant) %>%
  ungroup() %>%  # Ungroup to avoid issues when adding new rows
  bind_rows(   # Add a new row for each group
    St %>%
      summarize(Participant2 = 1, .groups = 'drop')
  ) %>% arrange(Participant,Game,Order)

St <- St %>% arrange(Participant,Game,Order) %>%
  rownames_to_column("Chronological") %>%
  mutate(Chronological = as.numeric(Chronological))






St <- St %>% arrange(Game,Condition) %>%
  rownames_to_column("ByCondition") %>%
  mutate(ByCondition = as.numeric(ByCondition))

St = St %>% arrange(Chronological)

#############
# Load Ranking Data
#############
R <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1_4b7ZV6QOW0KvtCOBV0L4MxS7sf-vq2r4WkrJ-haSTI/edit?gid=1433724657#gid=1433724657')

R = R %>% mutate(across(everything(), as.numeric))

Rg = R %>% select(Participant, GMF,GAS,GIO,GHIO) %>% pivot_longer(cols = starts_with('G'))
Rg = Rg %>% mutate(
  name.f = as.numeric(factor(name)),
  value_jit = value
)

Rgs = Rg %>% group_by(name) %>% rename(help = name) %>% summarize(
  "R1" = sum(value == 1),
  "R2" = sum(value == 2),
  "R3" = sum(value == 3),
  "R4" = sum(value == 4)
) %>% ungroup() %>%pivot_longer(cols = starts_with("R"))

Rgs = Rgs %>% mutate(value_pct = (value/11)*100,
                     value_pct = case_when(name == "R3" ~ -value_pct,
                                           name == "R4" ~ -value_pct,
                                           TRUE ~ value_pct),
                     name_ord = case_when(name == "R4" ~ "L3",
                                          name == "R3" ~ "L4",
                                          name == "R2" ~ "L1",
                                          name == "R1" ~ "L2"),
                     help = case_when(help == "GMF" ~ "Mit. Failure",
                                     help == "GIO" ~ "Helping NPC",
                                     help == "GHIO" ~ "Hidden Help",
                                     help == "GAS" ~ "Boost"))

custom_colors <- c("#93c777ff", "#c6e299ff","#f6c0cdff","#ec8ba5ff")

helpstyles = fig %>%
  add_trace(name="1", data=Rgs %>% filter(name == "R1"), y=~value_pct, x=~help, color=I("#93c777ff"), opacity=.8,base=Rgs %>% filter(name == "R2") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="2", data=Rgs %>% filter(name == "R2"), y=~value_pct, x=~help, color=I("#c6e299ff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="3", data=Rgs  %>% filter(name == "R3"), y=~value_pct, x=~help, color=I("#f6c0cdff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="4", data=Rgs %>% filter(name == "R4"), y=~value_pct, x=~help, color=I("#ec8ba5ff"), opacity=.8,base=Rgs %>% filter(name == "R3") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),
         barmode = 'stack',showlegend=F,
         title = 'General Help Preference',
         yaxis=list(range=c(-60.5,60.5),title="",showticklabels = FALSE,ticks=""),
         xaxis=list(title=""))
helpstyles
save_image(helpstyles, "fig/rank_helpstyles.pdf", width=335, height=335)

Rk = R %>% select(Participant, `Kiwi (0)`,`Kiwi (15)`,`Kiwi (30)`) %>% pivot_longer(cols = starts_with('K'))
Rks = Rk %>% group_by(name) %>% rename(help = name) %>% drop_na() %>% summarize(
  "R1" = sum(value == 1),
  "R2" = sum(value == 2) / 2,
  "R3" = sum(value == 2) / 2,
  "R4" = sum(value == 3),
) %>% ungroup() %>%pivot_longer(cols = starts_with("R"))

Rks = Rks %>% mutate(value_pct = (value/11)*100,
                     value_pct = case_when(name == "R3" ~ -value_pct,
                                           name == "R4" ~ -value_pct,
                                           TRUE ~ value_pct),
                     help = case_when(help == "Kiwi (0)" ~ "No Help",
                                      help == "Kiwi (15)" ~ "15% Help",
                                      help == "Kiwi (30)" ~ "30% Help"),
                     help = factor(help, levels=c("No Help","15% Help","30% Help")))

kiwi = fig %>%
  add_trace(name="1", data=Rks %>% filter(name == "R1"), y=~value_pct, x=~help, color=I("#93c777ff"), opacity=.8,base=Rks %>% filter(name == "R2") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="2", data=Rks %>% filter(name == "R2"), y=~value_pct, x=~help, color=I("#f3e4d5ff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value*2)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="3", data=Rks  %>% filter(name == "R3"), y=~value_pct, x=~help, color=I("#f3e4d5ff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value)),textposition='auto') %>%
  add_trace(name="4", data=Rks %>% filter(name == "R4"), y=~value_pct, x=~help, color=I("#ec8ba5ff"), opacity=.8,base=Rks %>% filter(name == "R3") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),
         barmode = 'stack',showlegend=F,
         title = 'Kiwi Game Preferences',
         yaxis=list(range=c(-60.5,60.5),title="",showticklabels = FALSE,ticks=""),
         xaxis=list(title=""))
kiwi
save_image(kiwi, "fig/rank_kiwi_game.pdf", width=285, height=335)

Rp = R %>% select(Participant, NO,MF,IO,AS) %>% pivot_longer(cols = c("NO","MF","IO","AS"))
Rps = Rp %>% group_by(name) %>% rename(help = name) %>% drop_na() %>% summarize(
  "R1" = sum(value == 1),
  "R2" = sum(value == 2),
  "R3" = sum(value == 3),
  "R4" = sum(value == 4),
) %>% ungroup() %>%pivot_longer(cols = starts_with("R"))

Rps = Rps %>% mutate(help = case_when(help == "AS" ~ "Boost",
                                      help == "IO" ~ "Helping NPC",
                                      help == "MF" ~ "Mit. Failure",
                                      help == "NO" ~ "No Help"),
                     help = factor(help, levels=c("No Help","Helping NPC","Boost","Mit. Failure")))

Rps = Rps %>% mutate(value_pct = (value/11)*100,
                     value_pct = case_when(name == "R3" ~ -value_pct,
                                           name == "R4" ~ -value_pct,
                                           TRUE ~ value_pct))

fish = fig %>%
  add_trace(name="1", data=Rps %>% filter(name == "R1"), y=~value_pct, x=~help, color=I("#93c777ff"), opacity=.8,base=Rps %>% filter(name == "R2") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="2", data=Rps %>% filter(name == "R2"), y=~value_pct, x=~help, color=I("#c6e299ff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="3", data=Rps  %>% filter(name == "R3"), y=~value_pct, x=~help, color=I("#f6c0cdff"), opacity=.8,base=0,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="4", data=Rps %>% filter(name == "R4"), y=~value_pct, x=~help, color=I("#ec8ba5ff"), opacity=.8,base=Rps %>% filter(name == "R3") %>% pull(value_pct),
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),
         barmode = 'stack',showlegend=F,
         title = 'Fish Game Preferences',
         yaxis=list(range=c(-60.5,60.5),title="",showticklabels = FALSE,ticks=""),
         xaxis=list(title=""))
fish
save_image(fish, "fig/rank_fish_game.pdf", width=335, height=335)

fig %>%
  add_trace(name="1", data=Rps %>% filter(name == "R1"), y=~value, x=~help, color=I("#93c777ff"), opacity=.8,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="2", data=Rps %>% filter(name == "R2"), y=~value, x=~help, color=I("#c0e482ff"), opacity=.8,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="3", data=Rps  %>% filter(name == "R3"), y=~value, x=~help, color=I("#d6eeb0ff"), opacity=.8,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  add_trace(name="4", data=Rps  %>% filter(name == "R4"), y=~value, x=~help, color=I("#f3f8ebff"), opacity=.8,
            type='bar', text=~paste0(round(value)),textposition='auto', textfont = list(color = "black", size = 14)) %>%
  layout(margin=list(l=0,r=0,t=55,b=0),
         barmode = 'stack',showlegend=F,
         title = 'Ranking Fishing Game',
         yaxis=list(range=c(-0.5,8.5),title="",showticklabels = FALSE,ticks=""),
         xaxis=list(title=""))

Rt = R %>% select(Participant, `Prefered game (1 Kiwi-2 Fish)`) %>% pivot_longer(cols = c("Prefered game (1 Kiwi-2 Fish)"))
Rts = Rt %>% group_by(name) %>% rename(help = name) %>% drop_na() %>% summarize(
  "Kiwi" = sum(value == 1),
  "Fish" = sum(value == 2),
)

Rts = Rts %>% mutate(value_pct = (value/11)*100,
                     value_pct = case_when(name == "R2" ~ -value_pct,
                                           TRUE ~ value_pct))


#############
# Visualize
#############  
#St = St %>% filter(!Condition %in% c("NO","0"))
fig %>%
  add_trace(data=St, x=~Chronological, y=~acc_rate+help_rate, color=~Participant, opacity=.6,
            type='scattergl', mode='markers+text', marker=list(size=20), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  add_trace(name="PercC. (1-7)", x=St$Chronological, y=St$PercNormalized, color=I('blue'), opacity=.6,
            type='scattergl', mode='markers+lines') %>%
  layout(yaxis=list(range=c(-0.1,1.1)))

fig %>%
  add_trace(data=St, x=~jitter(acc_rate+help_rate,amount=0.03), y=~jitter(FrustNormalized,amount=0.03), color=~Participant, opacity=.6,
            type='scattergl', mode='markers+text', marker=list(size=8), hoverinfo="text", text=paste(St$Participant, St$Condition)) %>%
  layout(yaxis=list(range=c(-0.1,1.1)))

St = St %>% group_by(Participant) %>% arrange(Chronological) %>%
  mutate(row_num = row_number())
  
St <- St %>% 
  mutate(
    Participant = case_when(
      Participant > 5 ~ Participant-1,
      TRUE ~ Participant
    )
  )

St <- St %>% 
 mutate(
   p_text = NA,
   p_text = case_when(
     row_num == 1 ~ " ",
     row_num == 2 ~ " ",
     row_num == 3 ~ " ",
     row_num == 4 ~ paste0("P",as.character(Participant)),   # assumes `Participant` is a column in St
     row_num == 5 ~ " ",
     row_num == 6 ~ " ",
     row_num == 7 ~ " ",
     TRUE ~ p_text      # fallback (keeps existing value or use NA_character_)
   )
 )



custom_colors <- c("#906762ff", "#215d81ff")

fig_c = fig %>%
  add_trace(data=St, x=~Chronological, y=~acc_rate+0.01, color=~Game,
            type='bar', showlegend=F, colors = custom_colors) %>%
  add_trace(data=St, x=~Chronological, y=~help_rate, color=~Game, showlegend=F,
            type='bar', opacity=0.5, colors = custom_colors) %>%
  add_trace(x=St$Chronological, y=-0.04, text=~p_text, color=I('black'), showlegend=F,
            type='scatter',mode="text") %>%
  add_trace(x=c(0,max(St$Chronological)), y=mean(St$acc_rate,na.rm=T), color=I('black'), showlegend=F,
            type='scatter', mode='lines', line=list(width=1.5), opacity=0.8) %>%
  add_trace(x=c(0,max(St$Chronological)), y=mean(St$acc_rate+St$help_rate,na.rm=T), color=I('black'), showlegend=F,
            type='scatter', mode='lines', line=list(dash="dot", width=1.5), opacity=0.8) %>%
  layout(margin=list(l=0,r=0,t=25,b=0),
         yaxis=list(range=c(-0.06,1.05), title="", tickformat = ".0%"), barmode = 'stack',
         xaxis=list(range=c(0,72), title="", ticks="",showline=F,showticklabels=F),
         title="BCI input recognition rate and added help during each experienced session"
         )
fig_c
save_image(fig_c, "fig/recognition.pdf", width=1050, height=230)



St %>% group_by(Participant) %>%
  summarize(range_acc = max(acc_rate, na.rm=T)-min(acc_rate, na.rm=T),
            sd_acc = sd(acc_rate, na.rm=T),
            ) %>% ungroup() %>% summarize(mean(sd_acc))

St %>% group_by(Participant) %>% 
  summarize(mean_acc = mean(acc_rate, na.rm=T))  %>%
  summarize(sd_acc = sd(mean_acc, na.rm=T))

#Using the BCI.
# On average each participant had X % recognition and received X% help in help conditions.
# Within each participant, the recognition varied by X%.


#############
# Level of Control to Perceived Control
#############

St = St %>% filter(Participant < 10)

PercLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"PercNormalized", "acchelp_rate", b=0.7),
                 "AS" = p_supsmu(St %>% filter(Condition == "AS"),"PercNormalized", "acchelp_rate", b=0.7),
                 "MF" = p_supsmu(St %>% filter(Condition == "MF"),"PercNormalized", "acchelp_rate", b=0.7),
                 "IO" = p_supsmu(St %>% filter(Condition == "IO"),"PercNormalized", "acchelp_rate", b=0.7))

FrustLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"FrustNormalized", "acchelp_rate", b=0.7),
                  "AS" = p_supsmu(St %>% filter(Condition == "AS"), "FrustNormalized", "acchelp_rate", b=0.7),
                  "MF" = p_supsmu(St %>% filter(Condition == "MF"),"FrustNormalized", "acchelp_rate", b=0.7),
                  "IO" = p_supsmu(St %>% filter(Condition == "IO"),"FrustNormalized", "acchelp_rate", b=0.7))

CombLine <- list("NO" = p_supsmu(St %>% filter(Condition == "NO"),"FrustNormalized", "PercNormalized", b=0.7),
                 "AS" = p_supsmu(St %>% filter(Condition == "AS"), "FrustNormalized", "PercNormalized", b=0.7),
                 "MF" = p_supsmu(St %>% filter(Condition == "MF"),"FrustNormalized", "PercNormalized", b=0.7),
                 "IO" = p_supsmu(St %>% filter(Condition == "IO"),"FrustNormalized", "PercNormalized", b=0.7))

# Perceived Control to Condition

fig_c <- lapply(unique(St$Condition), function(cond) {
  fig %>%
    add_trace(data=St %>% filter(Condition != cond), name=cond,
              marker=list(size=7), x=~acchelp_rate, y=~jitter(PercNormalized,amount=.02), color=I('rgba(0.8,0.8,0.8,0.15)'), 
              type='scatter', mode='markers', showlegend=F) %>%
    add_trace(data=PercLine[["NO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["AS"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["MF"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=PercLine[["IO"]], x=~x, y=~y, type='scatter', mode='lines', color=I('rgba(0.8,0.8,0.8,0.20)'), showlegend=F) %>%
    add_trace(data=St %>% filter(Condition == cond), name=cond,
              marker=list(size=7), x=~acchelp_rate, y=~jitter(PercNormalized,amount=.02), color=I('black'), 
              type='scatter', mode='markers') %>%
    layout(annotations=list(showarrow=F,x=-0.05,y=1.08,text=paste0(cond)),
           xaxis=list(zeroline=F,showgrid=F,title='Positive Feedback', range=c(-0.,1.1)),
           yaxis=list(zeroline=F,showgrid=F,title='Perceived Control', range=c(-0.1,1.1)))
}) %>% subplot(., nrows=1) %>% layout(showlegend=F, yaxis=list(title="Perceived Control"), xaxis=list(title="Positive Feedback"))
fig_c
save_image(fig_c, "fig/perc_control_pos_feedback.pdf", width=1150, height=350)
