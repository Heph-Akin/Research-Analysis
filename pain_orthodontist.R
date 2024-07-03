library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(ggthemes)

pain <- read.csv("C:/Users/akind/Downloads/Data/pain_ortho_spread.csv")

colnames(pain)

# Select the variables and create a combined dataframe
pain %>%
  select(
    Age = X1..Age..at.last.birthday.in.years.,
    Age_Category = age_category,
    Sex = X2..Sex,
    Years_In_Practice = X3..Number.of.years.in.orthodontic.practice.,
    Practice_Location = X4..Where.do.you.practice,
    Practice_City = X5..What.city.do.you.practice.in.
  ) %>%
  tbl_summary(
    label = list(
      Age = "Age (Years)",
      Age_Category = "Age Category",
      Sex = "Sex",
      Years_In_Practice = "Years in Orthodontic Practice",
      Practice_Location = "Practice Location",
      Practice_City = "City of Practice"
    ),
    digits = list(Age ~ 2),
    missing = "no",
    include = -all_of("Practice_Location"),  # Excluding Practice_Location from the table
    statistic = list(
      Age ~ "{mean} ± {sd}"  # Display mean and standard deviation for Age
    )
  ) 






library(gtsummary)



pain$X7..If.yes..on.a.scale.of.0.10..what.degree.of.discomfort.or.pain.do.you.think.they.experience...0.meaning.no.pain.and.10.meaning.maximum.pain <- as.numeric(pain$X7..If.yes..on.a.scale.of.0.10..what.degree.of.discomfort.or.pain.do.you.think.they.experience...0.meaning.no.pain.and.10.meaning.maximum.pain)

# Select the variables and create a combined dataframe
pain_summary <- pain %>%
  select(
    Experience_Pain = X6..Do.you.believe.orthodontic.patients.experience.pain.or.discomfort.at.start.of.removable.or.fixed.orthodontic.treatment..,
    Degree_Pain = X7..If.yes..on.a.scale.of.0.10..what.degree.of.discomfort.or.pain.do.you.think.they.experience...0.meaning.no.pain.and.10.meaning.maximum.pain,
    Pain_Duration = X8...How.long.do.you.think.the.pain.last.,
    Affected_Activities = X9..Which.of.the.following.activities.do..you..do.think.may.be.affected.by.the.pain.experience...Tick.all.that.apply.,
    Inform_Patients = X10..Do.you.routinely.inform.your.patients.that.they.may.experience.some.form.of.pain..discomfort.following.braces.installation..fixed.appliance.setup..or.activation.of.removable.appliance.,
    Recommend_Relief = X11..Do.you.routinely.recommend.anything.to.relieve.the.pain.after.the.braces.installation..fixed.appliance.setup..removable.appliance..,
    Relief_Recommendation = X12..If.yes..what..do.you.recommend...Tick.all.that.apply.,
    Standard_Pain_Protocol = X13..Do.you.feel.a.standard.pain.management.protocol.should.be.given.to.all.patients.after.braces.installation...appliance.activation..,
    Experience_Pain_Other = X14..Do.you.think.the.patients.may.experience.pain.following.other.orthodontic.treatment.procedures.,
    Procedures_Pain_Other = X15..If.yes..kindly.tick.the.procedures
  )

# Create a gtsummary table for the combined data
pain_summary_table <- pain_summary %>%
  tbl_summary(
    label = list(
      Experience_Pain = "Experience Pain (Yes/No)",
      Degree_Pain = "Degree of Pain (0-10)",
      Pain_Duration = "Pain Duration",
      Affected_Activities = "Affected Activities",
      Inform_Patients = "Inform Patients about Pain (Yes/No)",
      Recommend_Relief = "Recommend Relief (Yes/No)",
      Relief_Recommendation = "Relief Recommendation",
      Standard_Pain_Protocol = "Standard Pain Protocol (Yes/No)",
      Experience_Pain_Other = "Experience Pain Following Other Procedures (Yes/No)",
      Procedures_Pain_Other = "Procedures Causing Pain"
    ),
    type = list(Degree_Pain = "continuous"),  # Treat Degree_Pain as numeric/continuous
    digits = list(Degree_Pain = 2),  # Display Degree of Pain with two decimal points
    missing = "no",
    statistic = list(
      Degree_Pain ~ "{mean} ± {sd}") 
  )

# Print the pain summary table
pain_summary_table


# Select the variables and create a combined dataframe
activity_data <-  pain %>%
  mutate(
    Physical_Activities = ifelse(X9..Physical.activities == 1 | X9...Physical.activities == 1, 1, 0)
  ) %>%
  select(
    Ability_to_Work = X9...Ability.to.work,
    Brushing = X9...Brushing,
    Physical_Activities,
    School_Activities = X9...School.activities,
    Sleeping = X9...Sleeping,
    Talking = X9...Talking,
    Eating_Chewing = X9..Eating.Chewing
  )

# Create a gtsummary table for the combined data
activity_summary <- activity_data %>%
  tbl_summary(
    label = list(
      Ability_to_Work = "Ability to Work",
      Brushing = "Brushing",
      Physical_Activities = "Physical Activities",
      School_Activities = "School Activities",
      Sleeping = "Sleeping",
      Talking = "Talking",
      Eating_Chewing = "Eating/Chewing"
    ),
    missing = "no"
  )

# Print the activity summary table
activity_summary


# Update variables before selecting
pain_updated <- pain %>%
  mutate(
    Painkillers = ifelse(X12..Diclofenac == 1 | X12..Ibuprofen == 1 | X12.Ibuprofen == 1 | X12.Paracetamol == 1, 1, 0),
    Ice_Cream = ifelse(X12..Ice == 1 | X12..Ice.cream == 1, 1, 0)
  ) %>%
  select(
    Ability_to_Work = X9...Ability.to.work,
    Brushing = X9...Brushing,
    Physical_Activities,
    School_Activities = X9...School.activities,
    Sleeping = X9...Sleeping,
    Talking = X9...Talking,
    Eating_Chewing = X9..Eating.Chewing,
    Painkillers,
    Ice_Cream
  )

# Create a gtsummary table for the updated data
pain_summary_updated <- pain_updated %>%
  tbl_summary(
    label = list(
      Ability_to_Work = "Ability to Work",
      Brushing = "Brushing",
      Physical_Activities = "Physical Activities",
      School_Activities = "School Activities",
      Sleeping = "Sleeping",
      Talking = "Talking",
      Eating_Chewing = "Eating/Chewing",
      Painkillers = "Painkillers (Any)",
      Ice_Cream = "Ice/Ice Cream (Any)"
    ),
    missing = "no"
  )

# Print the updated pain summary table
pain_summary_updated



# Update the variables before selecting
pain_updated <- pain %>%
  mutate(
    Painkillers = ifelse(X12..Diclofenac == 1 | X12..Ibuprofen == 1 | X12.Ibuprofen == 1 | X12.Paracetamol == 1, 1, 0),
    Ice_Cream = ifelse(X12..Ice == 1 | X12..Ice.cream == 1, 1, 0)
  ) %>%
  select(
    Painkillers,
    Ice_Cream,
    Chewing_Gum = X12..Chewing.gum,
    Cold_Mouth_Rinse = X12..Cold.mouth.rinse,
    Nothing = X12..Nothing,
    Wax = X12..Wax
  )

# Create a gtsummary table for the updated data
painkillers_summary <- pain_updated %>%
  tbl_summary(
    label = list(
      Painkillers = "Painkillers",
      Ice_Cream = "Ice/Ice Cream",
      Chewing_Gum = "Chewing Gum",
      Cold_Mouth_Rinse = "Cold Mouth Rinse",
      Nothing = "Nothing",
      Wax = "Wax"
    ),
    missing = "no"
  )

# Print the updated painkillers summary table
painkillers_summary







# Combine Ibuprofen variables
pain_combined <- pain %>%
  mutate(
    Ibuprofen = ifelse(X12.Ibuprofen == 1 | X12..Ibuprofen == 1, 1, 0),
    Paracetamol = X12.Paracetamol,
    Diclofenac = X12..Diclofenac
  ) %>%
  select(
    Ibuprofen,
    Paracetamol,
    Diclofenac
  )

# Create a gtsummary table for the combined data
pain_summary_combined <- pain_combined %>%
  tbl_summary(
    label = list(
      Ibuprofen = "Ibuprofen",
      Paracetamol = "Paracetamol",
      Diclofenac = "Diclofenac"
    ),
    missing = "no"
  )

# Print the combined painkillers summary table
pain_summary_combined
