library(tidyverse)
deg_vars <- c(
  "p_id",
  "complete",
  "time_started",
  "time_ended",
  "DEG.gender",
  "DEG.age",
  "DEG.born_here",
  "gender",
  "age",
  "GDE.fse_recent",
  "bat_id",
  "school_id",
  "n_data", "n_gender", 
  "test_seq", "time_point", 
  "doublet", "test_seq2", 
  "keep"
)

dance_vars <- c(
  #"GDE.fse_recent",
  #"GDE.dance_lessons_recent",
  "GDE.dance_lessons_total",
  "GDE.dance_fun",
  "GDE.watch_dance_media",
  "GDS.urge_to_dance",
  "GDS.dance_training",
  "GDE.watch_dance_live", 
  "GDE.dance_style",
  
  "DER.perc_correct" 
)

cog_vars <- c("JAJ.ability", 
              #"JAJ.ability_sem", 
              #"JAJ.num_items", 
              "SEM.attentiveness", 
              "SEM.self_regulated_learning", 
              "SEM.cognitive_strategy_use",
              "TOI.theory_of_inteligence", 
              "TOI.goals_choice")

big5_vars <- c(
  "TPI.extraversion",
  "TPI.agreeableness",
  "TPI.conscientiousness",
  "TPI.emotional_stability",
  "TPI.openness_to_experiences"
)

misc_vars <- c("PAC.general", "CCM.general", "CCM.extra")
general_vars <- c(
  "HOP.general",
  "LON.total",
  "SWL.general",
  "SWB.total",
  "TOI.total",
  "JAJ.ability",
  "GDS.urge_to_dance",
  "DER.perc_correct"
)

wb_vars <- c(
  "SDQ.prosocial",
  "SDQ.emotional_problems",
  "SDQ.peer_problems",
  "LON.emotional_loneliness",
  "LON.social_loneliness",
  #"SCS.friends_met_num",
  #"SCS.friends_avoided_num",
  "SWB.life_satisfaction",
  "SWB.worthwhile",
  "SWB.happiness",
  "SWB.anxiety",
  "HOP.general",
  "SWL.general"
)
wb_polarity <- c(
  "SDQ.prosocial" = 1,
  "SDQ.emotional_problems" = -1,
  "SDQ.peer_problems" = -1,
  "LON.emotional_loneliness" = -1,
  "LON.social_loneliness" = -1,
  #"SCS.friends_met_num",
  #"SCS.friends_avoided_num",
  "SWB.life_satisfaction" = 1,
  "SWB.worthwhile" = 1,
  "SWB.happiness" = 1,
  "SWB.anxiety" = -1,
  "HOP.general" = 1,
  "SWL.general" = 1,
  "LON.total" = -1,
  "SWL.general" = 1,
  "SWB.total" = 1
  
)

socio_emo_vars <- c(
  "SEM.attentiveness",
  "SEM.self_regulated_learning",
  "SEM.cognitive_strategy_use",
  "SDQ.prosocial",
  "SDQ.emotional_problems",
  "SDQ.peer_problems",
  "TOI.theory_of_inteligence",
  "TOI.goals_choice"
#  "SCS.friends_met",
#  "SCS.friends_avoided",
  #"SCS.friends_met_num",
  #"SCS.friends_avoided_num"
)

well_being_vars <- c(
  "LON.emotional_loneliness",
  "LON.social_loneliness",
  "SWB.life_satisfaction",
  "SWB.worthwhile",
  "SWB.happiness",
  "SWB.anxiety",
  "SWL.general",
  "HOP.general"
)

ef_vars <- c("JAJ.ability", "GDS.urge_to_dance", "DER.perc_correct")

standard_controls <- c("DEG.born_here", "gender", "age",  "PAC.general")

get_polarity<- Vectorize(function(v){
  polarity <- 1
  if(v %in% names(wb_polarity)){
    polarity <- wb_polarity[v] %>% as.numeric()
  }
  polarity
})

treatment_ind <- c("p_group", "class_type", "semesters", "total_training")

wb_factors <- list("WBE.socialness"  =  c("SDQ.prosocial", "-LON.social_loneliness", "SWB.anxiety"),
"WBE.emo_problems"  =  c("SDQ.emotional_problems",  "SDQ.peer_problems", "LON.emotional_loneliness"),
"WBE.life_satisfaction"  =  c("SWB.life_satisfaction", "SWB.worthwhile", "SWB.happiness"))

wb_factors2 <- list("WBE.socialness"  =  c("SDQ.prosocial", "-LON.social_loneliness",  "HOP.general", "SWL.general", "SWB.anxiety"),
                   "WBE.emo_problems"  =  c("SDQ.emotional_problems",  "SDQ.peer_problems", "LON.total"),
                   "WBE.life_satisfaction"  =  c("SWB.life_satisfaction", "SWB.worthwhile", "SWB.happiness"))

ega_factors <- list(
  "FA.learning" = c("SEM.self_regulated_learning", "SEM.cognitive_strategy_use"),
  "FA.emo_problems" = c("SDQ.emotional_problems", "SDQ.peer_problems", "LON.emotional_loneliness"),
  "FA.growth_mindset" = c("TOI.theory_of_inteligence", "TOI.goals_choice"),
  #"FA.theory_of_intelligence"  = "TOI.theory_of_inteligence", 
  #"FA.performance_orientiation" = "TOI.goals_choice",
  "FA.life_satisfaction"  = c("-LON.social_loneliness", "SWB.life_satisfaction", "SWB.worthwhile", 
                            "SWB.happiness", "SWB.anxiety", "SWL.general", "HOP.general"),
  "FA.JAJ_ability" = "JAJ.ability",
  "FA.urge_to_dance" = "GDS.urge_to_dance",
  "FA.dance_emotion_recognition" = "DER.perc_correct"
  #,"FA.executive_dance" = c("JAJ.ability", "GDS.urge_to_dance", "DER.perc_correct")
  )

analysis_vars <- c(names(ega_factors))
