## Lecture 7 - Other types of regression
install.packages("tidyverse")
install.packages("titanic")
install.packages("AER")
# install.packages("mlogit")

library(tidyverse)
library(broom)
library(titanic)

# Create plot to show why linear regression is not good for binomial data
df_logit <- 
    tibble( y = seq(.0001,.9999,.0001),
            x = psych::logit(y),
    )

df <- 
    tibble( x = c(rnorm(500, -5, 3) , rnorm(500, 5, 3)),
            y = c(rep(0, 500), rep(1,500))
    )

ggplot(df) + 
    aes(x = x, y = y) + 
    geom_point(alpha = .2) +
    geom_point(data = df_logit, size = .1, color = "blue") +
    # geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
    coord_cartesian(ylim = c(-.25, 1.25)) +
    labs(x = "Predictor", y = "Outcome")


# Use case for logistic regression ----------------------------------------
# We will use the titanic dataset
# Make the table printing neat, transform variable names to lowercase
titanic <- 
    titanic_train %>% 
    rename_all(str_to_lower) %>% 
    as_tibble()
    
# Fit logistic binomial regression
surv_fit <- glm(survived ~ fare * sex + sibsp + parch, family = "binomial", data = titanic)

summary(surv_fit)
tidy(surv_fit)
glance(surv_fit)

# To get the odds ratio, use the exp() function on the coefficients
exp(surv_fit$coefficients)
# Calculate confidence intervals for the ORs
exp(confint(surv_fit))

# But instead of the previous, do yourself a favor and use tidy with the following parameters to get ORs and conf int. 
tidy(surv_fit, conf.int = TRUE, exponentiate = TRUE)

# Let's plot the data. Please mind that you need to tweek the arguments for geom_smooth() to fit a binomial logistic function.
ggplot(titanic) +
    aes(y = survived, x = fare, group = sex, color = sex) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    coord_cartesian(ylim = c(0, 1))

# Reporting logistic regression
library(stargazer)
stargazer(surv_fit, type = "text")

# To save it to html, do:
# To convert the coefficients and confidence intervals to OR, do this:
surv_fit_table_html <-
    stargazer(surv_fit,
              align = TRUE,
              ci = TRUE,
              df = TRUE,
              apply.coef = exp,
              apply.se   = exp,
              type = "text")

# You can save the results using the write_lines() function
write_lines(surv_fit_table_html, "surv_fit_table.html")

# TODO: INSTEAD OF MULTINOMIAL REGRESSION, ADD COX REGRESSION FOR ORDINAL OUTPUTS


### Multinomial logistic regression
# Predict the class of the passanger, given the age
# library(mlogit)
# 
# titanic <- 
#     mutate(titanic, pclass = pclass %>% as.factor())
# 
# titanic_mlogit <- mlogit.data(titanic, choice = "pclass", shape = "wide")
# pclass_fit <- mlogit(pclass ~ 1 | age, data = titanic_mlogit, reflevel = 2)
# 
# summary(pclass_fit)
# # We cannot use the broom package for the multinomial regression output, so if we want to get the odds ratios, we need to get the exponential of the coefficients
# exp(pclass_fit$coefficients)

## Poisson regression
# Use poisson regression to predict a count-type variable (integer values, and totally left-skewed)
# We are predicting the number of family members on board, by age

titanic <-
    titanic %>% 
    mutate(family = sibsp + parch)

# Check the distribution of family variable
titanic %>% 
    ggplot() +
    aes(x = family) +
    geom_histogram(bins = 10)

# Yep, definitely poisson distribution
# Fitting a poisson regression is not difficult, just use the family = "poisson" parameter
family_fit_pois <- glm(family ~ age, family = "poisson", data = titanic)
# Check the results. They look very much like the output of logistic regression, only the model summary statistics are different
summary(family_fit_pois)
tidy(family_fit_pois)
glance(family_fit_pois)

# However the poisson regression is not apropriate for data that has a large dispersion
# Dispersion shoul not be significantly larger than 1
# We can test the dispersion like this:
AER::dispersiontest(family_fit_pois)

# We have to run a negative binomial regression, since dispersion is 1.9 (variance is more than 2x the mean). This parameter was calculated using quasipoisson family.
family_fit_nb <- MASS::glm.nb(family ~ age, data = titanic)

# Check the results
summary(family_fit_nb)
tidy(family_fit_nb)
glance(family_fit_nb)

# You can create all the diagnostic values as for linear regression
augment(family_fit_nb)

# Let's plot this. Mind the geom_smooth() parameters!
titanic %>% 
    ggplot() +
    aes(y = family, x = age) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "poisson"))

# We can check the residual plots
autoplot(family_fit_nb, 1:6)

# When reporting poisson/negative binomial regression, you have to report the same things as in logistic regression
# Stargazer does not know the negative binomial regression :(, so it can only create table for the poisson
stargazer(family_fit_pois, type = "text")
stargazer(family_fit_pois,
          align = TRUE,
          ci = TRUE,
          df = TRUE,
          apply.coef = exp,
          apply.se   = exp,
          type = "text")
