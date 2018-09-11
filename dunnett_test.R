rm(list=ls())

library(DescTools)

#DUNNETT TEST ON ALL DATA (MALE AND FEMALE MICE COMBINED)
final_unscaled <- read.csv("final_unscaled.csv")

#p values computed in the following code are adjusted for multiple comparisons for familywise error rate. Function
# called p.adjust is used in which the default method is Holm's method
dunnetts_sleep_percent <- DunnettTest(Results.Sleep.Daily.Percent ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_light_sleep_percent <- DunnettTest(Results.Sleep.Light.Phase.Percent ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_dark_sleep_percent <- DunnettTest(Results.Sleep.Dark.Phase.Percent ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_sleep_bout_length <- DunnettTest(Results.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_light_sleep_bout_length <- DunnettTest(Results.Light.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_dark_sleep_bout_length <- DunnettTest(Results.Dark.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = final_unscaled)
dunnets_breath_rate <- DunnettTest(Results.Breath.Rate.During.Sleep.Mean ~ Genotype, control = "CONTROL", data = final_unscaled)

#Save output of dunnett test into csv file, which will later be used for making box plots
write.csv(dunnetts_sleep_percent$CONTROL, "dunnetts_sleep_percent.csv")
write.csv(dunnets_light_sleep_percent$CONTROL, "dunnetts_light_sleep_percent.csv")
write.csv(dunnets_dark_sleep_percent$CONTROL, "dunnetts_dark_sleep_percent.csv")
write.csv(dunnets_sleep_bout_length$CONTROL, "dunnetts_sleep_bout_length.csv")
write.csv(dunnets_light_sleep_bout_length$CONTROL, "dunnetts_light_sleep_bout_length.csv")
write.csv(dunnets_dark_sleep_bout_length$CONTROL, "dunnetts_dark_sleep_bout_length.csv")
write.csv(dunnets_breath_rate$CONTROL, "dunnetts_breath_rate.csv")

#DUNNETT TEST FOR MALE SLEEP VARIABLES
males_unscaled <- read.csv("males_unscaled.csv")

#Perform Dunnetts test for males
male_dunnetts_sleep_percent <- DunnettTest(Results.Sleep.Daily.Percent ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_light_sleep_percent <- DunnettTest(Results.Sleep.Light.Phase.Percent ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_dark_sleep_percent <- DunnettTest(Results.Sleep.Dark.Phase.Percent ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_sleep_bout_length <- DunnettTest(Results.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_light_sleep_bout_length <- DunnettTest(Results.Light.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_dark_sleep_bout_length <- DunnettTest(Results.Dark.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = males_unscaled)
male_dunnets_breath_rate <- DunnettTest(Results.Breath.Rate.During.Sleep.Mean ~ Genotype, control = "CONTROL", data = males_unscaled)

#Write csv files
write.csv(male_dunnetts_sleep_percent$CONTROL, "male_dunnetts_sleep_percent.csv")
write.csv(male_dunnets_light_sleep_percent$CONTROL, "male_dunnetts_light_sleep_percent.csv")
write.csv(male_dunnets_dark_sleep_percent$CONTROL, "male_dunnetts_dark_sleep_percent.csv")
write.csv(male_dunnets_sleep_bout_length$CONTROL, "male_dunnetts_sleep_bout_length.csv")
write.csv(male_dunnets_light_sleep_bout_length$CONTROL, "male_dunnetts_light_sleep_bout_length.csv")
write.csv(male_dunnets_dark_sleep_bout_length$CONTROL, "male_dunnetts_dark_sleep_bout_length.csv")
write.csv(male_dunnets_breath_rate$CONTROL, "male_dunnetts_breath_rate.csv")

#DUNNETT TEST FOR FEMALE SLEEP VARIABLES
females_unscaled <- read.csv("females_unscaled.csv")

#Perform Dunnetts test for females
female_dunnetts_sleep_percent <- DunnettTest(Results.Sleep.Daily.Percent ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_light_sleep_percent <- DunnettTest(Results.Sleep.Light.Phase.Percent ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_dark_sleep_percent <- DunnettTest(Results.Sleep.Dark.Phase.Percent ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_sleep_bout_length <- DunnettTest(Results.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_light_sleep_bout_length <- DunnettTest(Results.Light.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_dark_sleep_bout_length <- DunnettTest(Results.Dark.Sleep.Bout.Lengths.Mean ~ Genotype, control = "CONTROL", data = females_unscaled)
female_dunnets_breath_rate <- DunnettTest(Results.Breath.Rate.During.Sleep.Mean ~ Genotype, control = "CONTROL", data = females_unscaled)

#Write csv files
write.csv(female_dunnetts_sleep_percent$CONTROL, "female_dunnetts_sleep_percent.csv")
write.csv(female_dunnets_light_sleep_percent$CONTROL, "female_dunnetts_light_sleep_percent.csv")
write.csv(female_dunnets_dark_sleep_percent$CONTROL, "female_dunnetts_dark_sleep_percent.csv")
write.csv(female_dunnets_sleep_bout_length$CONTROL, "female_dunnetts_sleep_bout_length.csv")
write.csv(female_dunnets_light_sleep_bout_length$CONTROL, "female_dunnetts_light_sleep_bout_length.csv")
write.csv(female_dunnets_dark_sleep_bout_length$CONTROL, "female_dunnetts_dark_sleep_bout_length.csv")
write.csv(female_dunnets_breath_rate$CONTROL, "female_dunnetts_breath_rate.csv")