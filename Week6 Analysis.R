#install.packages("aod")
#install.packages("ggplot2")
library(aod)
library(ggplot2)
library(dplyr)


compas_scores = read.csv("/Users/Jake/Desktop/CU Work Etc/PublicPolicy/Week 6/compas-scores-two-years.csv")

#Comparing risk scores for general crime to overall recidivism
risk_scores_vs_recidivism <- aggregate( two_year_recid ~ decile_score.1, compas_scores, mean )
risk_scores_vs_recidivism$two_year_recid <- 100*risk_scores_vs_recidivism$two_year_recid

ggplot() +
geom_bar(risk_scores_vs_recidivism, 
         mapping = aes(x = decile_score.1, y = two_year_recid), 
         stat='identity') +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  ylim(0, 100) +
  xlab("Risk of Recidivism Score") +
  ylab("Actual Recidivism in 2 years") +
  ggtitle("Recidivism Risk vs Actual Recidivism") +
  
  theme_bw()




#Comparing risk scores for VIOLENT crime to overall recidivism
risk_scores_vs_violent_recidivism <- aggregate( two_year_recid ~ v_decile_score, compas_scores, mean )
risk_scores_vs_violent_recidivism$two_year_recid <- 100*risk_scores_vs_violent_recidivism$two_year_recid

ggplot() +
  geom_bar(risk_scores_vs_violent_recidivism, 
           mapping = aes(x = v_decile_score, y = two_year_recid), 
           stat='identity') +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  ylim(0, 100) +
  xlab("Risk of Violent Recidivism Score") +
  ylab("Actual Recidivism in 2 years") +
  ggtitle("Violent Recidivism Risk vs Actual Recidivism") +
  
  theme_bw()












#palette <- c("#46648B", "#F15511", "#D6E1F1", "#000000", "#FFFFFF")

cols <- c("Risk of Recividism"="#F15511", "Risk of Violent Recidivism"="#46648B")
#Comparing risk scores for GENERAL CRIME and VIOLENT crime to overall recidivism
risk_scores_vs_recidivism <- aggregate( two_year_recid ~ decile_score.1, compas_scores, mean )
risk_scores_vs_violent_recidivism <- aggregate( two_year_recid ~ v_decile_score, compas_scores, mean )
risk_scores_vs_recidivism$two_year_recid_violent <- risk_scores_vs_violent_recidivism$two_year_recid

ggplot(risk_scores_vs_recidivism, aes(x = decile_score.1)) +
  geom_bar(mapping = aes(y = two_year_recid, fill = "Risk of Recividism"), 
           stat='identity', alpha = 0.5) +
  geom_bar(mapping = aes(y = two_year_recid_violent, fill = "Risk of Violent Recidivism"), 
           stat='identity', alpha = 0.5) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  xlab("(Violent) Risk Score") +
  #ylab("Actual Recidivism in 2 years") +
  scale_y_continuous(
    "Actual Recidivism in 2 years", 
   # sec.axis = sec_axis(~ . * 10, name = "Risk Score"),
    limits = c(0, 1)
  ) +
  ggtitle("Number of Priors vs Actual Recidivism") + 
  scale_fill_manual(name="Bar",values=cols) +
  theme_bw()
















cols <- c("Risk Score"="#46648B", "Recidivism"="#F15511")


compas_scores <- compas_scores %>% 
  mutate(priors_group = case_when(priors_count.1 == 0 ~ "0",
                                #  priors_count.1 <= 5 ~ "< 10",
                          priors_count.1 <= 5 & priors_count.1 >= 1 ~ "1 - 5",
                          priors_count.1 <= 10 & priors_count.1 > 5 ~ "5 - 10",
                          priors_count.1 <= 20 & priors_count.1 > 10 ~ "10 - 20",
                          priors_count.1 <= 30 & priors_count.1 > 20 ~ "20 - 30",
                          priors_count.1 <= 40 & priors_count.1 > 30 ~ "> 30"))

#Comparing NUMBER OF PRIORS to overall recidivism
priors_comparisons <- aggregate( two_year_recid ~ priors_group, compas_scores, mean )
priors_vs_score <- aggregate(decile_score.1 ~ priors_group, compas_scores, mean)
priors_comparisons$two_year_recid <- 100*priors_comparisons$two_year_recid
priors_comparisons$decile_score.1 <- 10*priors_vs_score$decile_score.1
priors_comparisons$priors_group <- factor(priors_comparisons$priors_group, levels = c("0","1 - 5", "5 - 10", "10 - 20", "20 - 30", "> 30"))


ggplot(priors_comparisons, aes(x = priors_group, vjust = -0.5)) +
  geom_bar(mapping = aes(y = two_year_recid, fill = "Recidivism"), 
           stat='identity', alpha = 1, width = 0.4, just = 1) +
  geom_bar(mapping = aes(y = decile_score.1, fill = "Risk Score"), 
           stat='identity', alpha = 1, width = 0.4, just = 0) +
  #scale_x_continuous(breaks=seq(0, 10, 1)) +
  xlab("Number of Priors") +
  ylab("Actual Recidivism in 2 years") +
  scale_y_continuous(
    "Actual Recividism in 2 Years (Percentage)", 
    labels = function(x) paste0(x, "%"),
    sec.axis = sec_axis(~ . * 0.1, name = "Risk Score"),
    limits = c(0, 100)
  ) +
  ggtitle("Number of Priors vs Risk Score and Actual Recidivism") + 
  scale_fill_manual(name="Bar",values=cols) +
  # guides(fill = guide_legend(reverse = TRUE)) + #flips the ordering
  theme_bw()





#REORGANIZE AND FACTOR THE AGE CAT

cols <- c("Risk Score"="#46648B", "Recidivism"="#F15511")
#Comparing AGE to overall recidivism
age_comparisons <- aggregate( two_year_recid ~ age_cat, compas_scores, mean )
age_vs_score <- aggregate(decile_score.1 ~ age_cat, compas_scores, mean)
age_comparisons$two_year_recid <- 100*age_comparisons$two_year_recid
age_comparisons$decile_score.1 <- 10*age_vs_score$decile_score.1

age_comparisons$age_cat <- factor(age_comparisons$age_cat, levels = c("Less than 25", "25 - 45", "Greater than 45"))

ggplot(age_comparisons, aes(x = age_cat, vjust = -0.5)) +
  geom_bar(mapping = aes(y = two_year_recid, fill = "Recidivism"), 
           stat='identity', alpha = 1, width = 0.4, just = 1) +
  geom_bar(mapping = aes(y = decile_score.1, fill = "Risk Score"), 
           stat='identity', alpha = 1, width = 0.4, just = 0) +
  #scale_x_continuous(breaks=seq(0, 10, 1)) +
  xlab("Age Category") +
  ylab("Actual Recidivism in 2 years") +
  scale_y_continuous(
    "Actual Recividism in 2 Years (Percentage)", 
    labels = function(x) paste0(x, "%"),
    sec.axis = sec_axis(~ . * 0.1, name = "Risk Score"),
    limits = c(0, 100)
  ) +
  ggtitle("Age vs Risk Score and Actual Recidivism") + 
  scale_fill_manual(name="Bar",values=cols) +
  # guides(fill = guide_legend(reverse = TRUE)) + #flips the ordering
  theme_bw()







#WORK ON
#REORDERING THESE
#PUTTING SOME BARS TO THE LEFT AND SOME TO THE RIGHT


cols <- c("Risk Score"="#46648B", "Recidivism"="#F15511")
#Comparing RACE to overall recidivism
race_comparisons <- aggregate( two_year_recid ~ race, compas_scores, mean )

race_vs_score <- aggregate(decile_score.1 ~ race, compas_scores, mean)
race_comparisons$two_year_recid <- 100*race_comparisons$two_year_recid
race_comparisons$decile_score.1 <- 10*race_vs_score$decile_score.1

race_comparisons$race <- factor(race_comparisons$race, levels = c("Asian", "Hispanic", "Caucasian", "African-American", "Native American", "Other"))



ggplot(race_comparisons, aes(x = race)) +
  geom_bar(mapping = aes(y = two_year_recid, fill = "Recidivism"), 
           stat='identity', alpha = 1, width = 0.4, just = 1) +
  geom_bar(mapping = aes(y = decile_score.1, fill = "Risk Score"), 
          stat='identity', alpha = 1, width = 0.4, just = 0) +
  #scale_x_continuous(breaks=seq(0, 10, 1)) +
  xlab("Race") +
  ylab("Actual Recidivism in 2 years") +
  scale_y_continuous(
    "Actual Recividism in 2 Years (Percentage)", 
    labels = function(x) paste0(x, "%"),
    sec.axis = sec_axis(~ . * 0.1, name = "Risk Score"),
    limits = c(0, 100)
  ) +
  ggtitle("Race vs Risk Score and Actual Recidivism") + 
  scale_fill_manual(name="Bar",values=cols) +
 # guides(fill = guide_legend(reverse = TRUE)) + #flips the ordering
  theme_bw()




#add sample size to the race graph





cols <- c("Recidivism"="#F15511", "Risk Score"="#46648B")
#Comparing CHARGE DESCRIPTION to overall recidivism
charge_comparisons <- aggregate( two_year_recid ~ c_charge_desc, compas_scores, mean )
charge_vs_score <- aggregate(decile_score.1 ~ c_charge_desc, compas_scores, mean)
charge_comparisons$decile_score.1 <- 1/10*charge_vs_score$decile_score.1

ggplot(charge_comparisons, aes(x = c_charge_desc)) +
  geom_bar(mapping = aes(y = two_year_recid, fill = "Recidivism"), 
           stat='identity', alpha = 0.5) +
  geom_bar(mapping = aes(y = decile_score.1, fill = "Risk Score"), 
           stat='identity', alpha = 0.5) +
  #scale_x_continuous(breaks=seq(0, 10, 1)) +
  xlab("Number of Priors") +
  ylab("Actual Recidivism in 2 years") +
  scale_y_continuous(
    "Actual Recidivism", 
    sec.axis = sec_axis(~ . * 10, name = "Risk Score"),
    limits = c(0, 1)
  ) +
  ggtitle("Charge vs Risk Score vs Actual Recidivism") + 
  scale_fill_manual(name="Bar",values=cols) +
  theme_bw()





#do some sort of time analysis?
#also look at juvenile cases?

#a lot of limitations in the dataset
#no info on socioeconomic status, crime relations, location
#maybe its a spacial distribution

