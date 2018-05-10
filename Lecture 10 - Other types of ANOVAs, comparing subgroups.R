# Lecture 10: Factorial ANOVA, contrasts, post-hoc tests, ANCOVA, repeated-measures ANOVA

library(tidyverse)
library(broom)
library(ggfortify)
library(stargazer)


# Practice lm!
# Use the multcomp::sbp dataset

library(multcomp)

# Explore our data first
sbp
?sbp

# Make a plot that models the sbp for both genders by age separately
ggplot(sbp) +
    aes(x = age, y = sbp, group = gender, color = gender) +
    geom_point() +
    geom_smooth(method = "lm")

# Statistical modeling
# We can have five different models based on all possible variable constellations
# Model with age as a predictor
lm_age <- lm(sbp ~ age, data = sbp)
# Model with gender as predictor
lm_sex <- lm(sbp ~ gender, data = sbp)
# Model with both age and gender as predictors (main effects)
lm_sex_age <- lm(sbp ~ age + gender, data = sbp)
# Model with both age and gender as predictors (main effects + interaction)
lm_sex_age_int <- lm(sbp ~ age * gender, data = sbp)
# Model with both age and gender as predictors (only interaction)
lm_int <- lm(sbp ~ age : gender, data = sbp)

# Model comparison
# If you don't have hypotheses (not advised strategy), then use backward elimination. Go for the most complicated model to the least complicated, and exclude all that is not adding to your model

# First, just remove the interaction term
anova(lm_sex_age_int, lm_sex_age)
# Also useful to check the AIC and BIC values, and look for a difference of at least 2 between models
glance(lm_sex_age_int)
glance(lm_sex_age)

# The conclusion is that the interaction is not making the model better! So remove it
# Now we assume is that our model is the one without the interaction term
lm_best <- lm_sex_age

# Next, check if age adds to the explained variance
anova(lm_best, lm_age)
# It does, so don't remove it! What about sex?
anova(lm_best, lm_sex)
# Sex is important too, so keep it! Nothing left to remove, so now, we can conclude that we've found the best linear model.

# The best model is lm_sex_age (now it is also called lm_best), that explains ~78% of the variability of sbp. 
glance(lm_best)

stargazer(lm_age, lm_sex, lm_sex_age, lm_int, type = "text")
# You can also write this this table as an html
stargazer(lm_null, lm_age, lm_sex_age, lm_int, type = "html") %>% 
    write_lines("sbp_model.html")

### ANOVA
# Post-hoc tests for one categorical predictor
ggplot(PlantGrowth) +
    aes(x = group, y = weight, fill = group) +
    geom_boxplot()

anova_model <- aov(weight ~ group, data = PlantGrowth)
TukeyHSD(anova_model)

# Btw, you can add significance to your plots using ggsignif::geom_signif()
install.packages("ggsignif")
library(ggsignif)

ggplot(PlantGrowth) +
    aes(x = group, y = weight, fill = group) +
    geom_boxplot() +
    geom_signif(comparisons = list(c("trt1", "trt2")), 
                map_signif_level = TRUE)

# Use another post-hoc test using the multcomp package
post1 <- multcomp::glht(anova_model, linfct = mcp(group = "Dunnett"), base = 1)
summary(post1)
confint(post1)

# Use build in contrasts
# This one is used for comparing the control condition to treatment conditions
contrasts(PlantGrowth$group) <- contr.treatment(3, base = 1)

# But maybe it is better use weights to define the contrasts.
# This will compare the control condition to the treatment conditions
contrast1 <- c(-2,1,1)

# This compares the two treatment conditions
contrast2 <- c(0,-1,1)

contrasts(PlantGrowth$group) <- cbind(contrast1, contrast2)

# You can check that the contrasts are now tied to the variable
PlantGrowth$group

# Let's run the ANOVA
plant_model <- aov(weight ~ group, data = PlantGrowth)

# Test the significance of the contasts by using summary.lm()
# this reveals that there is not sig. difference between the treatments and the control conditions, but there is a difference between treatments
summary.lm(plant_model)

# Using polynomial contrasts (trend analysis)
contrasts(PlantGrowth$group) <- contr.poly(3)
plant_poly <- aov(weight ~ group, data = PlantGrowth)

# the quadratic trend is significant, because the category in the middle is smaller than the ones in the center
summary.lm(plant_poly)


# Factorial ANOVA
# We have several categorical predictors
# Note that you can use the unteraction() function in the group to make subgroups
ggplot(ToothGrowth) +
    aes(x = dose, y = len, fill = supp, group = interaction(supp, dose)) +
    geom_boxplot()

ToothGrowth <-
    ToothGrowth %>% 
    as_tibble() %>% 
    mutate(dose = as.factor(dose))
    
# Post-hoc tests
tooth_model <- aov(len ~ dose*supp, data = ToothGrowth)
summary(tooth_model)

# Investigate post hoc-compaisons for all levels, and tidy-up
TukeyHSD(tooth_model) %>% tidy()

# Check residual diagnostics
autoplot(tooth_model, which = 1:6)


# Repeated measures ANOVA
# We are going to use the ez package for the repeated-measures ANOVA
install.packages("ez")
library(ez)

mtept
?mtept
# Prepare data (tidy up)
df <- 
    multcomp::mtept %>% 
    rownames_to_column("id") %>% 
    gather(time, value, -treatment, -id) %>% 
    mutate(time = str_sub(time, 2)) %>% 
    as_tibble()
    

# Just plot data for each person
 ggplot(df) + 
        aes(x = time, y = value) + 
         geom_point() +
         facet_wrap(~id, scales = "free_y")

 # Perform repeated measures ANOVA
repeated_anova <-
    ezANOVA(
        dv = .(value),
        wid = .(id),
        within = .(time),
        data = df
    )

repeated_anova

# We can also include between subject factors, so it will be a mixed ANOVA
mixed_anova <-
    ezANOVA(
        dv = .(value),
        wid = .(id),
        within = .(time),
        between = .(treatment) ,
        data = df,
        type = 3
    )

mixed_anova

