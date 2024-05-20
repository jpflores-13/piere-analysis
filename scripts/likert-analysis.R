## 5-point Likert Scale Analysis for PIERE data
library(likert)
library(tidyverse)
library(janitor)

## Import & clean data
res <- readxl::read_xlsx("data/PIERE+Post-Assessment_April+1,+2024_06.58.xlsx", 
                         skip = 1) |> 
  clean_names() |> 
  select(-c(1:17)) |> 
  select(-c(19:20)) |>
  na.omit()

levels <- c("Not knowledgeable at all", "Slightly knowledgeable", "Moderately knowledgeable",
            "Very knowledgeable", "Extremely knowledgeable")

## Add levels to each factor column in res_knowledge
res_knowledge <- res |> 
  mutate(across(everything(), factor, levels = levels))

# Likert Scale for Knowledge ----------------------------------------------
## Knowledge of people's identities influence their experience in higher education and STEM
res_knowledge_id_before <- res_knowledge |> 
  select(1) |> 
  mutate(intervention = "before") |> 
  rename("Knowledge of people's identities influence their experience in higher education and STEM" = 1)

res_knowledge_id_after <- res_knowledge |> 
  select(2) |> 
  mutate(intervention = "after") |> 
  rename("Knowledge of people's identities influence their experience in higher education and STEM" = 1)

res_knowledge_id <- rbind(res_knowledge_id_before, res_knowledge_id_after)

## Knowledge of barriers that exclude people with marginalized identities from higher education and STEM
res_knowledge_barriers_before <- res_knowledge |> 
  select(3) |> 
  rename("Knowledge of barriers that exclude people with marginalized identities from higher education and STEM" = 1)

res_knowledge_barriers_after <- res_knowledge |> 
  select(4) |> 
  rename("Knowledge of barriers that exclude people with marginalized identities from higher education and STEM" = 1)

res_knowledge_barriers <- rbind(res_knowledge_barriers_before, res_knowledge_barriers_after)

## Knowledge of the impact of biases, discrimination, and microaggressions on individuals, organizations, and society
res_knowledge_impact_before <- res_knowledge |> 
  select(5) |> 
  rename("Knowledge of the impact of biases, discrimination, and microaggressions on individuals, organizations, and society" = 1)

res_knowledge_impact_after <- res_knowledge |> 
  select(6) |> 
  rename("Knowledge of the impact of biases, discrimination, and microaggressions on individuals, organizations, and society" = 1)

res_knowledge_impact <- rbind(res_knowledge_impact_before, res_knowledge_impact_after)

res_knowledge <- cbind(res_knowledge_id, res_knowledge_barriers, res_knowledge_impact)

likert_knowledge <- likert(res_knowledge[,c(1, 3, 4)], grouping = res_knowledge$intervention)

summary(likert_knowledge)

plot(likert_knowledge, legend.position = "right")

## Check for Statistical significance 


# Likert Scale for Ability ------------------------------------------------

res_ability <- res |> 
  select(c(7:18))

levels <- c("None", "Low", "Average",
            "Moderately high", "Very high")

res_ability <- res_ability |> 
  mutate(across(everything(), factor, levels = levels))

## Ability to use inclusive language (ex. partner, unhoused) in different personal and professional contexts
res_ability_language_before <- res_ability |> 
  select(1) |> 
  mutate(intervention = "before") |>
  rename("Ability to use inclusive language (ex. partner, unhoused) in different personal and professional contexts" = 1)

res_ability_language_after <- res_ability |> 
  select(2) |> 
  mutate(intervention = "after") |>
  rename("Ability to use inclusive language (ex. partner, unhoused) in different personal and professional contexts" = 1)

res_ability_language <- rbind(res_ability_language_before, res_ability_language_after)

## Ability to seek out and embracing unique perspectives of those with different backgrounds and respect their lived experiences
res_ability_seek_before <- res_ability |> 
  select(3) |> 
  rename("Ability to seek out and embracing unique perspectives of those with different backgrounds and respect their lived experiences" = 1)

res_ability_seek_after <- res_ability |> 
  select(4) |> 
  rename("Ability to seek out and embracing unique perspectives of those with different backgrounds and respect their lived experiences" = 1)

res_ability_seek <- rbind(res_ability_seek_before, res_ability_seek_after)

## Ability to adopt an open mindset of continuous learning to promote an environment where we can and do learn from our mistakes
res_ability_adopt_before <- res_ability |> 
  select(5) |> 
  rename("Ability to adopt an open mindset of continuous learning to promote an environment where we can and do learn from our mistakes" = 1)

res_ability_adopt_after <- res_ability |> 
  select(6) |> 
  rename("Ability to adopt an open mindset of continuous learning to promote an environment where we can and do learn from our mistakes" = 1)

res_ability_adopt <- rbind(res_ability_adopt_before, res_ability_adopt_after)

## Ability to speak out against harmful language whether or not an individual from the marginalized group is present
res_ability_speak_before <- res_ability |> 
  select(7) |> 
  rename("Ability to speak out against harmful language whether or not an individual from the marginalized group is present" = 1)

res_ability_speak_after <- res_ability |> 
  select(8) |> 
  rename("Ability to speak out against harmful language whether or not an individual from the marginalized group is present" = 1)

res_ability_speak <- rbind(res_ability_speak_before, res_ability_speak_after)

## Ability to advocate for opportunities for individuals with marginalized identities
res_ability_advocate_before <- res_ability |> 
  select(9) |> 
  rename("Ability to advocate for opportunities for individuals with marginalized identities" = 1)

res_ability_advocate_after <- res_ability |> 
  select(10) |> 
  rename("Ability to advocate for opportunities for individuals with marginalized identities" = 1)

res_ability_advocate <- rbind(res_ability_advocate_before, res_ability_advocate_after)

## Ability to facilitate an environment where everyone has a voice at the table
res_ability_facilitate_before <- res_ability |> 
  select(11) |> 
  rename("Ability to facilitate an environment where everyone has a voice at the table" = 1)

res_ability_facilitate_after <- res_ability |> 
  select(12) |> 
  rename("Ability to facilitate an environment where everyone has a voice at the table" = 1)

res_ability_facilitate <- rbind(res_ability_facilitate_before, res_ability_facilitate_after)

res_ability <- cbind(res_ability_language, res_ability_seek, res_ability_adopt,
                     res_ability_speak, res_ability_advocate, res_ability_facilitate)

likert_ability <- likert(res_ability[,c(1, 3:7)], grouping = res_ability$intervention)

summary(likert_ability)

plot(likert_ability, legend.position = "right")
