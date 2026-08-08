library(dagitty)
library(ggdag)
library(ggplot2)

backdoor_example <- function(){
  example_dag <- dagitty('dag{
  Y <- A -> X
  B <- X -> Y -> C
  Y <- B -> C
  }')
  ggdag(example_dag) + theme_dag() + geom_dag_node(color = "white") +geom_dag_text(color = "black", size = 10) 
  adjustmentSets(example_dag, exposure = "X", outcome =  "Y")
  adjustmentSets(example_dag, exposure = "X", outcome =  "C")
}

# Define the DAG using dagitty syntax
longgold_dag_example <- function(){
  music_dag_v2 <- dagitty('dag {
    SES.educational_degree -> MHE.general_score
    SES.educational_degree -> MIQ.score
    SES.educational_degree -> GMS.musical_training
    gender -> CCM 
    gender -> TOM.improvement 

    TOI.incremental_theory -> TOM.improvement
    TOI.incremental_theory -> GRT
    TOM.improvement -> GRT
    MHE.general_score -> GMS.active_engagement
    MHE.general_score -> GMS.musical_training
    MHE.general_score -> SEM.behavioral_engagement

    GRT -> SEM.behavioral_engagement
    GRT -> GMS.musical_training

    GMS.musical_training -> CCM
    GMS.musical_training -> GMS.active_engagement
    GMS.musical_training -> MDI.score 
    CCM -> MDI.score 

    GMS.active_engagement <- GMS.musical_training
}')
  

  ggdag(music_dag_v2) + theme_dag() + 
    geom_dag_node(color = "green")+
    geom_dag_text(color = "black", size = 4) +
    labs(title = "Corrected Causal Model (Parental SES Exogenous)")
  
} 
