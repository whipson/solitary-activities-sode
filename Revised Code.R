#Analysis script for: Time Alone Well Spent? A Person-Centered Analysis of Adolescents’ Solitary Activities
#Authors: Will E. Hipson, Robert J. Coplan, Morgan Dufour, Katherine R. Wood, & Julie C. Bowker

#R packages loaded.
library(tidyverse)
library(haven)
library(psych)
library(careless)
library(poLCA)
library(reshape2)
library(tidyselect)

##-- Data Preprocessing --##

# Load in raw data
hs_data_raw <- read_sav("High School Data Years 1 and 2 COMPLETE.sav")

# Selecting relevant items from raw dataset. Calculating the amount of string responding using longstring.
hs_items <- hs_data_raw %>%
  dplyr::select(id, solas1:solas17, panas1:panas20, shysoc1:shysoc25, sol_intent1:sol_intent6,
                sol_enjoy1:sol_enjoy9, sol_att1:sol_att10, tipi1:tipi10, norm_beliefs1:norm_beliefs12,
                connect1:connect8, laca1:laca36) %>%
  mutate(string_resp = longstring(.)) %>%
  dplyr::select(string_resp, everything()) %>%
  arrange(desc(string_resp))

string_resp <- hs_items %>%
  dplyr::select(id, string_resp)

# Computing Mahalanobis D from psych package.
hs_data <- hs_data_raw %>%
  dplyr::select(1:8, number_missing:uncodable, timealone:disconnected) %>%
  left_join(string_resp) %>%
  mutate(mahal = outlier(dplyr::select(.,
                                       timealone:sociability
  ), plot = FALSE)) %>%
  dplyr::select(id, year, duration, string_resp, everything()) 

alpha <- .001

cutoff <- (qchisq(p = 1 - alpha, df = 6))

# Remove outliers
hs_data_clean <- hs_data %>%
  filter(mahal <= cutoff,
         string_resp < 20)

# Create indicator variables
hs_data_clean <- hs_data_clean %>%
  mutate(hobbies_i = hobbies,
         reading_i = reading,
         social_media_i = social_media,
         video_games_i = video_games,
         comp_med_int_i = comp_med_int + phone_conv,
         passive_screen_i = passive_screen + screen_unsp,
         music_listen_i = music_listen,
         meditate_spiritual_i = meditate_positive + spiritual,
         relaxation_i = relaxation,
         daydreaming_i = daydreaming,
         planning_i = planning,
         negative_i = negative,
         nothing_i = nothing,
         homework_i = homework,
         exercise_i = exercise,
         outdoors_i = outdoors,
         routine_i = routine)

# Larger categories are dichotomized to either 1 or 2 (poLCA requires 1s and 2s instead of 0s and 1s).
hs_data_clean <- hs_data_clean %>%
  dplyr::select(-sol_intentions) %>%
  mutate_at(vars(matches("_i")), list(~ifelse(. >= 1, 2, 1)))

# Filter cases that did not include any activities.
hs_lpa_data <- hs_data_clean %>%
  filter(number_missing < 3)

# Isolating activity indicators for LCA.
lpa_analysis <- hs_lpa_data %>%
  dplyr::select(matches("_i"), -comp_med_int)

# Setting seed for reproducibility
set.seed(140519)

##-- Latent Class Estimation --##

g <- cbind(hobbies_i, reading_i, social_media_i, video_games_i, comp_med_int_i,
           passive_screen_i, music_listen_i, meditate_spiritual_i, relaxation_i,
           daydreaming_i, planning_i, negative_i, nothing_i, homework_i,
           exercise_i, outdoors_i, routine_i) ~ 1

one_class <- poLCA(g, lpa_analysis, nclass = 1)

two_class <- poLCA(g, lpa_analysis, nclass = 2, nrep = 50, maxiter = 5000, graphs = TRUE)

three_class <- poLCA(g, lpa_analysis, nclass = 3, nrep = 50, maxiter = 5000, graphs = TRUE)

four_class <- poLCA(g, lpa_analysis, nclass = 4, nrep = 50, maxiter = 5000, graphs = TRUE)

five_class <- poLCA(g, lpa_analysis, nclass = 5, nrep = 50, maxiter = 5000, graphs = TRUE)

save(three_class, file = "three_class_v2.RData")

# --Plotting Class Solutions --#

load("three_class_v2.RData")

# 3-class

means3 <- data.frame(three_class$probs, three_class$P, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  dplyr::select(class = rowname, vars_select(names(.), contains("Pr.2.")), probs = 'three_class.P') %>%
  rename("Hobbies" = 2, "Reading" = 3, "Social Media" = 4, "Video Games" = 5, "Computer Interaction" = 6, 
         "Passive Screen" = 7, "Music Listening" = 8, "Meditation" = 9, "Relaxation" = 10, "Daydreaming" = 11,
         "Planning" = 12, "Negative Thinking" = 13, "Nothing" = 14, "Homework" = 15, "Exercise" = 16, "Outdoors" = 17,
         "Routine" = 18) %>%
  melt(id.vars = c("class", "probs"), variable.name = "Activity", value.name = "Probability") %>%
  unite(class, c("class", "probs"), sep = "")

latent_class_probabilities <- means3 %>%
  ggplot(aes(Activity, Probability, group = class, color = class, shape = class)) +
  geom_point(size = 5, alpha = .85) +
  geom_line(size = 1.50, alpha = .85) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#EE3B3B", "#000080", "#A2CD5A"), labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%")) +
  scale_linetype_discrete(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%")) +
  scale_shape_discrete(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%")) +
  labs(x = NULL, y = "Probability of Engaging in Activity") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), legend.position = "top",
        axis.title.y = element_text(size = 16),
        legend.key.size = unit(3.5, "line"),
        panel.grid.major.x = element_blank())

ggsave("probabilities_v2.png", latent_class_probabilities, width = 10.25)

## -- Model Fit Indices -- ##

model_fit <- data.frame(matrix(c(one_class$bic, two_class$bic, three_class$bic, four_class$bic, five_class$bic,
                                 one_class$llik, two_class$llik, three_class$llik, four_class$llik, five_class$llik,
                                 one_class$Chisq, two_class$Chisq, three_class$Chisq, four_class$Chisq, five_class$Chisq,
                                 one_class$aic, two_class$aic, three_class$aic, four_class$aic, five_class$aic), ncol = 4))

model_fit <- model_fit %>%
  mutate(model = row_number()) %>%
  dplyr::select(model, BIC = X1, AIC = X4, 'Log-likelihood' = X2, 'Chi^2' = X3) 

ggplot(model_fit, aes(model, BIC)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  labs(y = "BIC - lower is better") +
  theme_bw()

ggplot(model_fit, aes(model, AIC)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  labs(y = "AIC - lower is better") +
  theme_bw()

ggplot(model_fit, aes(model, `Log-likelihood`)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  labs(y = "LLIK - higher is better") +
  theme_bw()

ggplot(model_fit, aes(model, `Chi^2`)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  labs(y = "Chisquare - lower is better") +
  theme_bw()

# Entropy
relative_entropy <- function(lc) {
  numerator <- -sum(lc$posterior * log(lc$posterior), na.rm = TRUE)
  denominator <- (lc$N * length(lc$P))
  entropy <- 1 - (numerator/denominator)
  return(entropy)
}

relative_entropy(one_class)
relative_entropy(two_class)
relative_entropy(three_class)
relative_entropy(four_class)
relative_entropy(five_class)

##-- Bootstrapped Likelihood Ratio Test --##
poLCA_bootstrap_LRT <- function(formula, data, max_k = NULL, nboot = 999, verbose = TRUE) {
  
  if(is.null(max_k))
    K <- 1:9
  else {
    K <- 1:(max_k + 1)
  }
  
  if(verbose) {
    cat("bootstrapping LRTS ...\n")
    flush.console()
    pbar <- txtProgressBar(min = 0, max = (max_k - 1) * nboot, style = 3)
    on.exit(close(pbar))
  }
  
  obsLRTS <- vector("numeric", length = max_k - 1)
  p.value <- vector("numeric", length = max_k - 1)
  bootLRTS <- matrix(as.double(NA), nrow = nboot, ncol = max_k - 1)
  g <- formula
  
  k <- 0
  continue <- TRUE
  while(k < (max_k - 1) & continue) {
    k <- k + 1
    mod0 <- poLCA(g, data = data, nclass = k, graphs = FALSE, verbose = FALSE, calc.se = FALSE)
    mod1 <- poLCA(g, data = data, nclass = k + 1, graphs = FALSE, verbose = FALSE, calc.se = FALSE)
    obsLRTS[k] <- 2 * (mod1$llik - mod0$llik)
    b <- 0
    while(b < nboot) {
      b <- b + 1
      boot_sample <- poLCA.simdata(N = nrow(data), probs = mod0$probs, nclass = k)
      names(boot_sample$dat) <- names(data)
      bootMod0 <- poLCA(g, boot_sample$dat, nclass = k, graphs = FALSE, verbose = FALSE, calc.se = FALSE)
      bootMod1 <- poLCA(g, boot_sample$dat, nclass = k + 1, graphs = FALSE, verbose = FALSE, calc.se = FALSE)
      LRTS <- 2 * (bootMod0$llik - bootMod1$llik)
      if(is.na(LRTS)) {
        b <- b - 1
        (next)()
      }
      bootLRTS[b, k] <- LRTS
      if(verbose)
        setTxtProgressBar(pbar, (k - 1) * nboot + b)
    }
    p.value[k] <- (1 + sum(bootLRTS[, k] >= obsLRTS[k]))/(nboot + 1)
    if(is.null(max_k) & p.value[k] > .05) {
      continue <- FALSE
      if(verbose)
        setTxtProgressBar(pbar, (max_k - 1) * nboot)
    }
  }
  out <- list(K = 1:k, obs = obsLRTS[1:k], boot = bootLRTS[, 1:k, drop = FALSE], p.value[1:k])
  class(out) <- "poLCA_bootstrap_LRT"
  return(out)
}

poLCA_bootstrap_LRT(g, lpa_analysis, max_k = 5)

#### ---- CLASSIFY and IMPORT ---- ####

# When reproducing the analysis with latent classified data, start from here. 
# Commented out sections are for creating and exporting the data obtained in the first section.

#hs_lpa_data <- cbind(hs_lpa_data, three_class$predclass) %>% rename(profile = 'three_class$predclass')

#write_sav(hs_lpa_data, "highschool activities lca_v2.sav")

##-- Descriptive Statistics and Correlations --##

# Reloading the data with class membership. Recoding into specified order.
hs_lpa_data <- read_sav("highschool activities lca_v2.sav")

# Correlations between Time Alone and Affect:
time_pos_lm1 <- lm(pos_affect ~ I(timealone), data = hs_lpa_data)
summary(time_pos_lm1)

time_pos_lm2 <- lm(pos_affect ~ I(timealone) + I(timealone^2), data = hs_lpa_data)
summary(time_pos_lm2)

anova(time_pos_lm2, time_pos_lm1)

# Scatterplots
hs_lpa_data %>%
  ggplot(aes(x = timealone, y = pos_affect)) +
  geom_point() +
  geom_smooth()

hs_lpa_data %>%
  ggplot(aes(x = timealone, y = neg_affect)) +
  geom_point() +
  geom_smooth()

# Profile breakdown (numeric)
hs_lpa_data %>%
  count(profile)

# Descriptive Statistics
describe(hs_lpa_data)

# Zero-order Correlations with Non-activity variables
hs_lpa_data %>%
  dplyr::select(age, timealone, pos_affect, neg_affect, shyness, sociability, 
                sol_enjoyment, sol_attitudes, loneliness, sol_affinity, sol_aversion,
                disconnected) %>%
  corr.test()

# Age and gender differences

describeBy(hs_lpa_data, "gender")

age_diff <- aov(age ~ factor(profile), data = hs_lpa_data)
summary(age_diff)
TukeyHSD(age_diff)

hs_lpa_data_gender <- hs_lpa_data %>%
  filter(gender %in% c(1, 2))

hs_lpa_data_gender %>%
  describeBy("gender")

t.test(timealone ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(pos_affect ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(neg_affect ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(shyness ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(sociability ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(sol_enjoyment ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(loneliness ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(sol_affinity ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)
t.test(sol_aversion ~ gender, data = hs_lpa_data_gender, var.equal = TRUE)

# Chi-square for gender by class

gender_prop <- chisq.test(hs_lpa_data_gender$gender, hs_lpa_data_gender$profile)
gender_prop

# Scale Reliability

hs_data_raw %>%
  dplyr::select(shysoc1:shysoc20) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>%
  dplyr::select(shysoc21:shysoc25) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>%
  dplyr::select(panas1, panas3, panas5, panas9, panas10, panas12, panas14, panas16, panas17, panas19) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>%
  dplyr::select(panas2, panas4, panas6, panas7, panas8, panas11, panas13, panas15, panas18, panas20) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>%
  dplyr::select(sol_enjoy1:sol_enjoy8) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>% #loneliness
  dplyr::select(laca2, laca3, laca5, laca7, laca12, laca13, laca18, laca21, laca26, 
                laca28, laca32, laca36) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>% #aversion
  dplyr::select(laca6, laca8, laca9, laca11, laca15, laca17, laca19, laca23, laca25, 
                laca27, laca30, laca33) %>%
  alpha(check.keys = TRUE)

hs_data_raw %>% #affinity
  dplyr::select(laca1, laca4, laca10, laca14, laca16, laca20, laca22, laca24, laca29, 
                laca31, laca34, laca35) %>%
  alpha(check.keys = TRUE)

##-- Marascuillo Procedure --##

p <- c(.30, .27, .54, .24, .07, .19, .06, .13, .42, .10, .18) #proportion who engaged in activity
samplesize <- 824
alpha <- .05

N <- length(p)
value <- critvalue <- comparison <- c()
categories <- colnames(hs_lpa_data[,52:62])
categories

# Compute critical values.
for (i in 1:(N - 1)){ 
  for (j in (i + 1):N){
    
    value <- c(value,(abs(p[i] - p[j])))
    critvalue <- c(critvalue,
                  sqrt(qchisq(1 - alpha, N - 1)) * sqrt(p[i] * (1 - p[i])/samplesize + p[j] * (1-p[j])/samplesize))
    comparison <- c(comparison, paste(categories[i], categories[j], sep = "-"))
  }
}
df <- as.data.frame(cbind(comparison, value, critvalue), stringsAsFactors = FALSE)
df$value <- round(as.numeric(df$value), 3) 
df$critvalue <- round(as.numeric(df$critvalue), 3)
df$significant <- ifelse(df$value > df$critvalue, "SIG", "n.s.")

print(df)

####---- Comparing Solitary Activity Classes on Outcomes ----####

# Loading packages for multiple imputation
library(mice)
library(miceadds)
library(emmeans)

# Recoding gender as binary. Gender non-binary is coded as missing.
hs_lpa_data <- hs_lpa_data %>%
  mutate_at(vars(matches("_i")), list(~ifelse(. >= 2, 1, 0))) %>%
  mutate(gender = ifelse(gender > 2, NA, gender))

##-- Positive Affect --##

# impute positive affect

aov_pos_dat <- hs_lpa_data %>%
  dplyr::select(gender, pos_affect, profile, neg_affect, timealone, shyness, sociability) %>%
  mutate(gender = as.factor(gender),
         profile = as.factor(profile))

pos_init <- mice(aov_pos_dat, maxit = 0)
pos_meth <- pos_init$method
pos_predM <- pos_init$predictorMatrix

pos_meth[c("pos_affect", "neg_affect", "timealone", "shyness", "sociability")] <- "norm"
pos_meth[c("gender")] <- "logreg"
pos_meth[c("profile")] <- "polyreg"

pos_imputed <- mice(aov_pos_dat, method = pos_meth, predictorMatrix = pos_predM, m = 50)

# omnibus

aov_pos1 <- mi.anova(mi.res = pos_imputed, formula = "pos_affect ~ profile + timealone + gender", type = 2)
aov_pos2 <- mi.anova(mi.res = pos_imputed, formula = "pos_affect ~ profile * gender + timealone", type = 2)

aov_pos1
aov_pos2

# model comps

aov_pos_comp0 <- with(pos_imputed, lm(pos_affect ~ profile + gender + timealone))
aov_pos_comp1 <- with(pos_imputed, lm(pos_affect ~ profile * gender + timealone))

mitml::testModels(aov_pos_comp1$analyses, aov_pos_comp0$analyses, method = "D2")

##-- Negative Affect --##

# impute negative affect

aov_neg_dat <- hs_lpa_data %>%
  dplyr::select(gender, pos_affect, profile, neg_affect, timealone, shyness, sociability) %>%
  mutate(gender = as.factor(gender),
         profile = as.factor(profile))

neg_init <- mice(aov_neg_dat, maxit = 0)
neg_meth <- neg_init$method
neg_predM <- neg_init$predictorMatrix

neg_meth[c("pos_affect", "neg_affect", "timealone", "shyness", "sociability")] <- "norm"
neg_meth[c("gender")] <- "logreg"
neg_meth[c("profile")] <- "polyreg"

neg_imputed <- mice(aov_neg_dat, method = neg_meth, predictorMatrix = neg_predM, m = 50)

# omnibus

aov_neg1 <- mi.anova(mi.res = neg_imputed, formula = "neg_affect ~ profile + gender + timealone", type = 2)
aov_neg2 <- mi.anova(mi.res = neg_imputed, formula = "neg_affect ~ profile * gender + timealone", type = 2)

# model comps

aov_neg_comp0 <- with(neg_imputed, lm(neg_affect ~ profile + gender + timealone))
aov_neg_comp1 <- with(neg_imputed, lm(neg_affect ~ profile * gender + timealone))

mitml::testModels(aov_neg_comp1$analyses, aov_neg_comp0$analyses, method = "D2")

# post-hoc 1 v 2 & 3

summary(pool(aov_neg_comp0))

# post-hoc 2 v 1 & 3

aov_neg_comp2 <- with(neg_imputed, lm(neg_affect ~ I(profile == 1) + I(profile == 3) + gender))
summary(pool(aov_neg_comp2))

##-- Solitude Enjoyment --##

# impute solitude enjoyment

aov_solenjoy_dat <- hs_lpa_data %>%
  filter(year == 1) %>%
  dplyr::select(gender, sol_enjoyment, profile, timealone, shyness, sociability) %>%
  mutate(gender = as.factor(gender),
         profile = as.factor(profile))

solenjoy_init <- mice(aov_solenjoy_dat, maxit = 0)
solenjoy_meth <- solenjoy_init$method
solenjoy_predM <- solenjoy_init$predictorMatrix

solenjoy_meth[c("sol_enjoyment", "timealone", "shyness", "sociability")] <- "norm"
solenjoy_meth[c("gender")] <- "logreg"
solenjoy_meth[c("profile")] <- "polyreg"

solenjoy_imputed <- mice(aov_solenjoy_dat, method = solenjoy_meth, predictorMatrix = solenjoy_predM, m = 50)

# omnibus

aov_solenjoy1 <- mi.anova(mi.res = solenjoy_imputed, formula = "sol_enjoyment ~ profile + gender + timealone", type = 2)
aov_solenjoy2 <- mi.anova(mi.res = solenjoy_imputed, formula = "sol_enjoyment ~ profile * gender + timealone", type = 2)

aov_enj_comp0 <- with(solenjoy_imputed, lm(sol_enjoyment ~ profile + gender))
aov_enj_comp1 <- with(solenjoy_imputed, lm(sol_enjoyment ~ profile * gender))

mitml::testModels(aov_enj_comp1$analyses, aov_enj_comp0$analyses, method = "D2")

##-- Time Alone --##

# time alone imputed

aov_time_dat <- hs_lpa_data %>%
  dplyr::select(gender, profile, timealone, shyness, sociability) %>%
  mutate(gender = as.factor(gender),
         profile = as.factor(profile))

time_init <- mice(aov_time_dat, maxit = 0)
time_meth <- time_init$method
time_predM <- time_init$predictorMatrix

time_meth[c("timealone", "shyness", "sociability")] <- "norm"
time_meth[c("gender")] <- "logreg"
time_meth[c("profile")] <- "polyreg"

time_imputed <- mice(aov_time_dat, method = time_meth, predictorMatrix = time_predM, m = 50)

# omnibus

aov_time1 <- mi.anova(mi.res = time_imputed, formula = "timealone ~ profile + gender", type = 2)
aov_time2 <- mi.anova(mi.res = time_imputed, formula = "timealone ~ profile * gender", type = 2)

# model comps

aov_time_comp0 <- with(time_imputed, lm(timealone ~ profile + gender))
aov_time_comp1 <- with(time_imputed, lm(timealone ~ profile * gender))

mitml::testModels(aov_time_comp1$analyses, aov_time_comp0$analyses, method = "D2")

# post-hoc 1 v 2 & 3

summary(pool(aov_time_comp0))
summary(pool(aov_time_comp1))

# post-hoc 2 v 1 & 3

aov_time_comp2 <- with(time_imputed, lm(timealone ~ I(profile == 1) + I(profile == 3) + gender))
summary(pool(aov_time_comp2))

##-- Loneliness --##

# impute loneliness

aov_lon_dat <- hs_lpa_data %>%
  filter(year == 2) %>%
  dplyr::select(profile, loneliness, timealone, gender, shyness, sociability) %>%
  mutate(gender = as.factor(gender),
         profile = as.factor(profile))

lon_init <- mice(aov_lon_dat, maxit = 0)
lon_meth <- lon_init$method
lon_predM <- lon_init$predictorMatrix

lon_meth[c("loneliness", "timealone", "shyness", "sociability")] <- "norm"
lon_meth[c("gender")] <- "logreg"
lon_meth[c("profile")] <- "polyreg"

lon_imputed <- mice(aov_lon_dat, method = lon_meth, predictorMatrix = lon_predM, m = 50)

# omnibus

aov_lon1 <- mi.anova(mi.res = lon_imputed, formula = "loneliness ~ profile + gender + timealone", type = 2)
aov_lon2 <- mi.anova(mi.res = lon_imputed, formula = "loneliness ~ profile * gender + timealone", type = 2)

aov_lon_comp0 <- with(lon_imputed, lm(loneliness ~ profile + gender + timealone))
aov_lon_comp1 <- with(lon_imputed, lm(loneliness ~ profile * gender + timealone))

mitml::testModels(aov_lon_comp1$analyses, aov_lon_comp0$analyses, method = "D2")

# post-hoc 1 v 2 & 3

summary(pool(aov_lon_comp0))

# post-hoc 2 v 1 & 3

aov_lon_comp2 <- with(lon_imputed, lm(loneliness ~ I(profile == 1) + I(profile == 3) + gender))
summary(pool(aov_lon_comp2))

#### -- Barplots -- ####

library(ggpubr)

#Figure 2

my_comparisons <- list(c("1", "2"), c("1", "3"), c("2", "3"))
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns"))

describeBy(hs_lpa_data, group = "profile")

pos_plot <- hs_lpa_data %>%
  ggplot(aes(x = factor(profile), y = pos_affect, fill = factor(profile))) +
  stat_summary(geom = "bar", fun.data = mean_sd, width = .5, color = "black", size = 1) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args, size = 5, label.y = c(4, 4.4, 4.8)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, size = 1) +
  scale_fill_manual(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%"),
                    values = c("#EE3B3B", "#000080", "#A2CD5A")) +
  labs(x = "",
       y = NULL,
       title = "Positive Affect",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank())

pos_plot

neg_plot <- hs_lpa_data %>%
  ggplot(aes(x = factor(profile), y = neg_affect, fill = factor(profile))) +
  stat_summary(geom = "bar", fun.data = mean_sd, width = .5, color = "black", size = 1) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args, size = 5, label.y = c(3, 3.4, 3.8)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, size = 1) +
  scale_fill_manual(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%"),
                    values = c("#EE3B3B", "#000080", "#A2CD5A")) +
  labs(x = "",
       y = NULL,
       title = "Negative Affect",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank())

neg_plot

timealone_plot <- hs_lpa_data %>%
  filter(gender %in% c(1, 2)) %>%
  ggplot(aes(x = factor(profile), y = timealone, fill = factor(profile))) +
  stat_summary(geom = "bar", fun.data = mean_sd, width = .5, color = "black", size = 1) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args, size = 5, label.y = c(4.6, 5.1, 5.6)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, size = 1) +
  scale_fill_manual(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%"),
                    values = c("#EE3B3B", "#000080", "#A2CD5A")) +
  labs(x = "",
       y = NULL,
       title = "Time Alone",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank())

timealone_plot

lonely_plot <- hs_lpa_data %>%
  ggplot(aes(x = factor(profile), y = loneliness, fill = factor(profile))) +
  stat_summary(geom = "bar", fun.data = mean_sd, width = .5, color = "black", size = 1) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args, size = 5, label.y = c(3, 3.4, 3.8)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, size = 1) +
  scale_fill_manual(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%"),
                    values = c("#EE3B3B", "#000080", "#A2CD5A")) +
  labs(x = "",
       y = NULL,
       title = "Loneliness",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(hjust = .5),
        panel.grid.major.x = element_blank())

lonely_plot

pref_plot <- hs_lpa_data %>%
  ggplot(aes(x = factor(profile), y = sol_enjoyment, fill = factor(profile))) +
  stat_summary(geom = "bar", fun.data = mean_cl_normal, width = .5, color = "black", size = 1) +
  stat_compare_means(comparisons = my_comparisons, symnum.args = symnum.args, size = 5, label.y = c(4.7, 5.1, 5.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .25, size = 1) +
  scale_fill_manual(labels = c("Engaged: 31.7%", "Thinking: 15.0%", "Passive Media: 53.3%"),
                    values = c("#EE3B3B", "#000080", "#A2CD5A")) +
  labs(x = "",
       y = NULL,
       title = "Preference for Solitude",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(hjust = .5))

pref_plot

barplots <- ggarrange(timealone_plot, neg_plot, lonely_plot, ncol = 3, common.legend = TRUE, legend = "bottom")
barplots

ggsave("barplots.png", barplots)
