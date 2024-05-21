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

levels_knowledge <- c("Not knowledgeable at all", "Slightly knowledgeable", "Moderately knowledgeable",
            "Very knowledgeable", "Extremely knowledgeable")

labels <- 1:5

## Add levels to each factor column in res_knowledge
res_knowledge <- res |> 
  mutate(across(everything(), factor, levels = levels_knowledge))

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
stats_knowledge <- res |> 
  mutate(across(everything(), ~ factor(.x, levels = levels_knowledge, labels = labels))) |> 
  mutate(across(everything(), as.numeric)) 

## Knowledge of the impact of biases, discrimination, and microaggressions on individuals, organizations, and society
wilcox.test(stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_the_impact_of_biases_discrimination_and_microaggressions_on_individuals_organizations_and_society_before_this_class,
            stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_the_impact_of_biases_discrimination_and_microaggressions_on_individuals_organizations_and_society_after_this_class,
            paired = TRUE)

## Knowledge of how peoples' identities influence their experience in higher education and STEM 
wilcox.test(stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_how_peoples_identities_influence_their_experience_in_higher_education_and_stem_before_this_class,
            stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_how_peoples_identities_influence_their_experience_in_higher_education_and_stem_after_this_class,
            paired = TRUE)

## Knowledge of barriers that exclude people with marginalized identities from higher education and STEM
wilcox.test(stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_barriers_that_exclude_people_with_marginalized_identities_from_higher_education_and_stem_before_this_class,
            stats_knowledge$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_knowledge_of_barriers_that_exclude_people_with_marginalized_identities_from_higher_education_and_stem_after_this_class,
            paired = TRUE)

# Likert Scale for Ability ------------------------------------------------

res_ability <- res |> 
  select(c(7:18))

levels_ability <- c("None", "Low", "Average",
            "Moderately high", "Very high")

## Add levels to each factor column in res_knowledge
res_ability <- res_ability |> 
  mutate(across(everything(), factor, levels = levels_ability))

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

## stats
stats_ability <- res |>
  mutate(across(everything(), ~ factor(.x, levels = levels_ability, labels = labels))) |>
  mutate(across(everything(), as.numeric)) |> 
  select(-c(1:6))

## Ability to
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_use_inclusive_language_ex_partner_unhoused_in_different_personal_and_professional_contexts_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_use_inclusive_language_ex_partner_unhoused_in_different_personal_and_professional_contexts_after_this_class,
            paired = TRUE)

## Ability to
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_seek_out_and_embracing_unique_perspectives_of_those_with_different_backgrounds_and_respect_their_lived_experiences_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_seek_out_and_embracing_unique_perspectives_of_those_with_different_backgrounds_and_respect_their_lived_experiences_after_this_class,
            paired = TRUE)

## Ability to 
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_adopt_an_open_mindset_of_continuous_learning_to_promote_an_environment_where_we_can_and_do_learn_from_our_mistakes_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_adopt_an_open_mindset_of_continuous_learning_to_promote_an_environment_where_we_can_and_do_learn_from_our_mistakes_after_this_class,
            paired = TRUE)

## Ability to 
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_speak_out_against_harmful_language_whether_or_not_an_individual_from_the_marginalized_group_is_present_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_speak_out_against_harmful_language_whether_or_not_an_individual_from_the_marginalized_group_is_present_after_this_class,
            paired = TRUE)

## Ability to 
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_advocate_for_opportunities_for_individuals_with_marginalized_identities_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_advocate_for_opportunities_for_individuals_with_marginalized_identities_after_this_class,
            paired = TRUE)

## Ability to 
wilcox.test(stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_facilitate_an_environment_where_everyone_has_a_voice_at_the_table_before_this_class,
            stats_ability$thinking_about_yourself_now_and_before_this_class_how_do_you_rate_your_ability_to_facilitate_an_environment_where_everyone_has_a_voice_at_the_table_after_this_class,
            paired = TRUE)
