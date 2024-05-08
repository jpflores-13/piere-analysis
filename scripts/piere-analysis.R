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

# Likert Scale for Knowledge ----------------------------------------------
## Knowledge of people's identities influence their experience in higher education and STEM
## Knowledge of barriers that exclude people with marginalized identities from higher education and STEM
## Knowledge of the impact of biases, discrimination, and microaggressions on individuals, organizations, and society

res_knowledge <- res |> 
  select(c(1:6))

res_knowledge <- as.data.frame(unclass(res_knowledge),
                               stringsAsFactors = TRUE)

names(res_knowledge) <- c("identities_before", "identities_after", "barriers_before",
                          "barriers_after", "impact_before", "impact_after")

levels <- c("Not knowledgeable at all", "Slightly knowledgeable", "Moderately knowledgeable",
            "Very knowledgeable", "Extremely knowledgeable")

## Add levels to each factor column in res_knowledge
res_knowledge <- res_knowledge |> 
  mutate(across(everything(), factor, levels = levels))

likert_knowledge <- likert(res_knowledge)

summary(likert_knowledge)

plot(likert_knowledge,
     group.order = names(res_knowledge))

# Likert Scale for Ability ------------------------------------------------
## Ability to use inclusive language (ex. partner, unhoused) in different personal and professional contexts
## Ability to seek out and embracing unique perspectives of those with different backgrounds and respect their lived experiences
## Ability to adopt an open mindset of continuous learning to promote an environment where we can and do learn from our mistakes
## Ability to speak out against harmful language whether or not an individual from the marginalized group is present
## Ability to advocate for opportunities for individuals with marginalized identities
## Ability to facilitate an environment where everyone has a voice at the table

res_ability <- res |> 
  select(c(7:18))

res_ability <- as.data.frame(unclass(res_ability),
                             stringsAsFactors = TRUE)

names(res_ability) <- c("inclusiveLanguage_before", "inclusiveLanguage_after",
                        "seekPerspectives_before","seekPerspectives_after",
                        "openMindset_before", "openMindset_after",
                        "againstHarmfulLang_before", "againstHarmfulLang_after",
                        "advocate_before", "advocate_after",
                        "facilitateEnvironment_before", "facilitateEnvironment_after")

levels <- c("None", "Low", "Average",
            "Moderately high", "Very high")

res_ability <- res_ability |> 
  mutate(across(everything(), factor, levels = levels))

likert_ability <- likert(res_ability)

summary(likert_ability)
plot(likert_ability,
     group.order = names(res_ability))

## Test for significance
## Average/Median scores of before and after for each question?
## Wilcoxon signed-rank test or Mann-Whitney U test?


