library(tidyverse)
library(stringr)
library(gtsummary)
library(forcats)


oral_habits <- read.csv("/home/akindele/Documents/Data_Science_Projects/R/Research Analysis/oral_habits_spread.csv") %>%
                select(-Timestamp, -Form.Number)  %>%
                mutate_all(str_trim)
                

colnames(oral_habits)






# Function to replace NAs and blank spaces based on column type
replace_na_blank <- function(x) {
  x <- ifelse(x == "", NA, x) # Replace blank spaces with NA
  if (is.numeric(x)) {
    x[is.na(x)] <- sample(x[!is.na(x)], sum(is.na(x)), replace = TRUE)
  } else {
    prop <- table(x, useNA = "ifany") / length(x)
    x[is.na(x)] <- sample(names(prop), sum(is.na(x)), replace = TRUE, prob = prop)
  }
  x
}

# Replace NAs and blank spaces in each column
oral_habits_filled <- oral_habits %>%
  mutate(across(everything(), replace_na_blank))

# Check for NAs
colSums(is.na(oral_habits_filled))




#Convert everything that's not "Christian" or "Muslim" in the Religion column of oral_habits_filled to "Other"
oral_habits_filled <- oral_habits_filled %>%
  mutate(Religion = ifelse(Religion %in% c("Christian", "Muslim"), Religion, "Other"))



# Convert some variables to numeric
oral_habits_filled <- oral_habits_filled %>%
  mutate_at(vars(
    Age,
    X6..Breathing.through.the.mouth,
    X6..Elbow.Sucking,
    X6..Hair.Plucking,
    X6..Lip.Biting,
    X6..Lip.Sucking,
    X6..Nail.Biting,
    X6..Sticking.Out.the.tongue,
    X6..Thumb.Digit.Sucking,
    X6..Tongue.Sucking,
    X6..Tooth.Grinding,
    X19.Counselling.the.child.to.stop.the.habit,
    X19.Removing.anything.that.affects.the.child.emotionally.or.psychologically,
    X19.Rewarding.the.child.anytime.he.she.is.not.carrying.out.the.oral.habit,
    X19.Use.of.braces,
    X19.Use.of.habit.breaking.appliances,
    X19.Using.something.to.remind.the.child.anytime.he.she.wants.to.engage.in.the.habit,
    X26.I.advise.them.to.see.a.dentist,
    X26.I.advise.them.to.stop,
    X26.I.do.nothing,
    X28.Breathing.through.the.mouth,
    X28.Elbow.sucking,
    X28.Hair.plucking,
    X28.Lip.biting,
    X28.Lip.sucking,
    X28.Nail.biting,
    X28.Thumb.digit.sucking,
    X28.Tongue.sucking,
    X28.Tooth.grinding,
    X28.sticking.out.the.tongue,
    X30.Braces,
    X30.Counselling.me..the.child..to.stop.the.habit,
    X30.Habit.breaking.appliances,
    X30.Holes.in.tooth,
    X30.My.brothers.hit.my.mouth.sometimes.if.I.suck.my.lips.and.that.made.me.stop,
    X30.Removing.anything.that.affects.me..the.child..emotionally.or.psychologically,
    X30.Rewarding.the.child.anytime.he.she.is.not.carry.out.the.oral.habit,
    X30.Telling.he.she.the.effect.of.oral..habit,
    X30.To.stop,
    X30.Using.something.to.remind.me.anytime.I.want.to.engage.in.the.habit
  ), as.numeric)


# Save cleaned and imputed data-frame
write.csv(oral_habits_filled, "oral_habits_filled.csv", row.names = FALSE)


# Demographic table
oral_habits_filled %>%
  select(Age, Sex, Class, Religion, School) %>%
  tbl_summary()


oral_habits_filled %>%
  select(School,
         X5..Have.you.heard.of.the.term.oral.habits..Habits.involving.parts.of.the.mouth..,
         X6..Breathing.through.the.mouth,
         X6..Elbow.Sucking,
         X6..Hair.Plucking,
         X6..Lip.Biting,
         X6..Lip.Sucking,
         X6..Nail.Biting,
         X6..Sticking.Out.the.tongue,
         X6..Thumb.Digit.Sucking,
         X6..Tongue.Sucking,
         X6..Tooth.Grinding,
         X7..Do.you.think.oral.habits.can.lead.to.malocclsuion..scattered.teeth..,
         X8..Do.you.think.oral.habits.can.be.caused.by.stress.or.feel.of.insecuirity.,
         X9..Do.you.think.there.is.a.high.occurence.of.oral.habits.among.children.,
         X10..Children.who.breastfeed.for.a.longer.period.have.a.lower.tendency.of.developing.oral.habits..Do.you.agree.,
         X11..Developing.oral.habits.are.not.influenced.by.peer.groups.or.siblings..Do.you.agree.,
         X12..Children.of.working.parents.caregivers.who.get.to.spend.less.time.with.their.parents.tend.to.indulge..engage..more.in.oral.habits..Do.you.agree.,
         X13..Children.experiencing.issues.adjusting.to.their.surroundings.are.more.likely.to.indulge.in.Oral.habits..Do.you.agree.,
         X14..Oral.habits.affect.a.child.s.facial.appearance.which.inturn.affect.their.self.esteem..Do.you.agree.,
         X15..The.negative.effects.of.oral.habits.generally.subside.disappear.if.the.habit.is.stopped.before.the.permanent.teeth.erupt..come.into.the.mouth.,
         X16...Discouraging.stopping.certain.oral.habits.early.in.a.young.child..3.years..will.avoid.negative.effects.of.oral.habits.on.the.child.s.self.esteem.and.growth.of.the.face.and.teeth..Do.you.agree.,
         X17....Parental.pressure.or.social.pressure.influence.can.help.a.child.break.the.habit.,
         X18.....Do.you.think.professionals.should.check.the.child.for.psychological..emotional.problems.before.starting.oral.habit.treatment.,
         X19.Counselling.the.child.to.stop.the.habit,
         X19.Removing.anything.that.affects.the.child.emotionally.or.psychologically,
         X19.Rewarding.the.child.anytime.he.she.is.not.carrying.out.the.oral.habit,
         X19.Use.of.braces,
         X19.Use.of.habit.breaking.appliances,
         X19.Using.something.to.remind.the.child.anytime.he.she.wants.to.engage.in.the.habit,
         X20...Do.you.think.educating.the.parents.child.and.care.giver.about.oral.health.can.serve.as.a.preventive.measure.to.reduce.the.occurrence.of.oral.habits.,
         X21....Do.you.think.using.reminder.therapy.and.rewarding.the.child.for.not.indulging.in.the.habit.can.be.useful.in.stopping.oral.habits.at.the.early.stage.,
         X22.....Do.you.think.patients.with.oral.habits.should.see.the.dentists.for.treatment.,
         X23...Do.you.think.people.know.of.the.negative.effects.of.oral.habits.and.ways.to.stop.or.treat.it.,
         X24...Do.you.think.people.tend.to.neglect.oral.habits.in.children.when.they.occur.,
         X25...Do.you.know.anyone.who.engages.in.any.type.of.oral.habits.,
         X26.I.advise.them.to.see.a.dentist,
         X26.I.advise.them.to.stop,
         X26.I.do.nothing) %>%
  tbl_summary(by = School,
              label = list(
                X5..Have.you.heard.of.the.term.oral.habits..Habits.involving.parts.of.the.mouth.. = "Heard of Oral Habits",
                X6..Breathing.through.the.mouth = "Breathing through Mouth",
                X6..Elbow.Sucking = "Elbow Sucking",
                X6..Hair.Plucking = "Hair Plucking",
                X6..Lip.Biting = "Lip Biting",
                X6..Lip.Sucking = "Lip Sucking",
                X6..Nail.Biting = "Nail Biting",
                X6..Sticking.Out.the.tongue = "Sticking Out Tongue",
                X6..Thumb.Digit.Sucking = "Thumb Digit Sucking",
                X6..Tongue.Sucking = "Tongue Sucking",
                X6..Tooth.Grinding = "Tooth Grinding",
                X7..Do.you.think.oral.habits.can.lead.to.malocclsuion..scattered.teeth.. = "Oral Habits lead to Malocclusion",
                X8..Do.you.think.oral.habits.can.be.caused.by.stress.or.feel.of.insecuirity. = "Stress/Insecuirity can Cause Oral Habits",
                X9..Do.you.think.there.is.a.high.occurence.of.oral.habits.among.children. = "Occurrence of Oral Habits in Children",
                X10..Children.who.breastfeed.for.a.longer.period.have.a.lower.tendency.of.developing.oral.habits..Do.you.agree. = "Breastfeeding and Oral Habits",
                X11..Developing.oral.habits.are.not.influenced.by.peer.groups.or.siblings..Do.you.agree. = "Influence of Peers/Siblings on Oral Habits",
                X12..Children.of.working.parents.caregivers.who.get.to.spend.less.time.with.their.parents.tend.to.indulge..engage..more.in.oral.habits..Do.you.agree. = "Impact of Parental Time on Oral Habits",
                X13..Children.experiencing.issues.adjusting.to.their.surroundings.are.more.likely.to.indulge.in.Oral.habits..Do.you.agree. = "Adjustment Issues and Oral Habits",
                X14..Oral.habits.affect.a.child.s.facial.appearance.which.inturn.affect.their.self.esteem..Do.you.agree. = "Oral habits affect a child's facial appearance",
                X15..The.negative.effects.of.oral.habits.generally.subside.disappear.if.the.habit.is.stopped.before.the.permanent.teeth.erupt..come.into.the.mouth. = "Negative Effects of Oral Habits",
                X16...Discouraging.stopping.certain.oral.habits.early.in.a.young.child..3.years..will.avoid.negative.effects.of.oral.habits.on.the.child.s.self.esteem.and.growth.of.the.face.and.teeth..Do.you.agree. = "Discouraging Oral Habits in Young Children",
                X17....Parental.pressure.or.social.pressure.influence.can.help.a.child.break.the.habit. = "Parental/Social Pressure to Break Habits",
                X18.....Do.you.think.professionals.should.check.the.child.for.psychological..emotional.problems.before.starting.oral.habit.treatment. = "Checking for Psychological/Emotional Problems Before Treatment",
                X19.Counselling.the.child.to.stop.the.habit = "Counselling to Stop Habit",
                X19.Removing.anything.that.affects.the.child.emotionally.or.psychologically = "Removing Emotional/Psychological Triggers",
                X19.Rewarding.the.child.anytime.he.she.is.not.carrying.out.the.oral.habit = "Rewarding for Not Engaging in Habit",
                X19.Use.of.braces = "Use of Braces",
                X19.Use.of.habit.breaking.appliances = "Use of Habit-breaking Appliances",
                X19.Using.something.to.remind.the.child.anytime.he.she.wants.to.engage.in.the.habit = "Using Reminders to Avoid Habit",
                X20...Do.you.think.educating.the.parents.child.and.care.giver.about.oral.health.can.serve.as.a.preventive.measure.to.reduce.the.occurrence.of.oral.habits. = "Educating Parents and Caregivers for Prevention",
                X21....Do.you.think.using.reminder.therapy.and.rewarding.the.child.for.not.indulging.in.the.habit.can.be.useful.in.stopping.oral.habits.at.the.early.stage. = "Reminder Therapy and Rewarding for Stopping Habits",
                X22.....Do.you.think.patients.with.oral.habits.should.see.the.dentists.for.treatment. = "Dentist Treatment for Oral Habits",
                X23...Do.you.think.people.know.of.the.negative.effects.of.oral.habits.and.ways.to.stop.or.treat.it. = "Knowledge of Effects and Treatments",
                X24...Do.you.think.people.tend.to.neglect.oral.habits.in.children.when.they.occur. = "Neglecting Oral Habits in Children",
                X25...Do.you.know.anyone.who.engages.in.any.type.of.oral.habits. = "Knowing Someone with Oral Habits",
                X26.I.advise.them.to.see.a.dentist = "Advising to See a Dentist",
                X26.I.advise.them.to.stop = "Advising to Stop Habit",
                X26.I.do.nothing = "I do nothing"
              )) %>%
  add_overall() %>%
    add_p()

# Practice table
oral_habits_filled %>%
  select(School,
         X27...Do.you.engage.in.any.form.of.oral.habit.,
         X28.Breathing.through.the.mouth,
         X28.Elbow.sucking,
         X28.Hair.plucking,
         X28.Lip.biting,
         X28.Lip.sucking,
         X28.Nail.biting,
         X28.Thumb.digit.sucking,
         X28.Tongue.sucking,
         X28.Tooth.grinding,
         X28.sticking.out.the.tongue,
         X29..Have.you.received.any.form.of.treatment.for.the.oral.habit.,
         X30.Braces,
         X30.Counselling.me..the.child..to.stop.the.habit,
         X30.Habit.breaking.appliances,
         X30.Holes.in.tooth,
         X30.My.brothers.hit.my.mouth.sometimes.if.I.suck.my.lips.and.that.made.me.stop,
         X30.Removing.anything.that.affects.me..the.child..emotionally.or.psychologically,
         X30.Rewarding.the.child.anytime.he.she.is.not.carry.out.the.oral.habit,
         X30.Telling.he.she.the.effect.of.oral..habit,
         X30.To.stop,
         X30.Using.something.to.remind.me.anytime.I.want.to.engage.in.the.habit) %>%
  tbl_summary(by = School,
              label = list(
                X27...Do.you.engage.in.any.form.of.oral.habit. = "Engagement in Oral Habit",
                X28.Breathing.through.the.mouth = "Breathing through Mouth",
                X28.Elbow.sucking = "Elbow Sucking",
                X28.Hair.plucking = "Hair Plucking",
                X28.Lip.biting = "Lip Biting",
                X28.Lip.sucking = "Lip Sucking",
                X28.Nail.biting = "Nail Biting",
                X28.Thumb.digit.sucking = "Thumb Digit Sucking",
                X28.Tongue.sucking = "Tongue Sucking",
                X28.Tooth.grinding = "Tooth Grinding",
                X28.sticking.out.the.tongue = "Sticking Out Tongue",
                X29..Have.you.received.any.form.of.treatment.for.the.oral.habit. = "Received Treatment for Oral Habit",
                X30.Braces = "Braces",
                X30.Counselling.me..the.child..to.stop.the.habit = "Counselling to Stop Habit",
                X30.Habit.breaking.appliances = "Habit-breaking Appliances",
                X30.Holes.in.tooth = "Holes in Tooth",
                X30.My.brothers.hit.my.mouth.sometimes.if.I.suck.my.lips.and.that.made.me.stop = "Sibling's Impact on Habit",
                X30.Removing.anything.that.affects.me..the.child..emotionally.or.psychologically = "Removing Emotional/Psychological Triggers",
                X30.Rewarding.the.child.anytime.he.she.is.not.carry.out.the.oral.habit = "Rewarding for Not Engaging in Habit",
                X30.Telling.he.she.the.effect.of.oral..habit = "Educating about Effects of Oral Habit",
                X30.To.stop = "Intent to Stop Habit",
                X30.Using.something.to.remind.me.anytime.I.want.to.engage.in.the.habit = "Using Reminders to Avoid Habit"
              )) %>%
  add_overall() %>%
  add_p()
