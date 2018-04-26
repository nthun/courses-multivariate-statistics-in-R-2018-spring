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
            x = psych::logit(y)
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
              type = "html")

# You can save the results using the write_lines() function
write_lines(surv_fit_table_html, "surv_fit_table.html")


## ordinal (cox) regression - Cox proportional-hazards model (Cox, 1972)
# commonly used in medical research for survival analysis
# works for both quantitative and categorical predictor
# assess simultaneously the effect of several risk factors (e.g., genotype, age)
# provides effect size for each factor

# cox model
# h(t)=h0(t)×exp(b1x1+b2x2+...+bpxp)
# exp(bi), hazard ratios(HR), hazard ratio above 1 indicates a covariate that is positively associated with the event probability, and thus negatively associated with the length of survival.
# HR = 1: No effect
# HR < 1: Reduction in the hazard
# HR > 1: Increase in Hazard

# install and load packages 
# "survival" for computing survival analyses
# "survminer" for visualizing survival analysis results
install.packages(c("survival", "survminer"))
library(survival)
library(survminer)

# We’ll use NCCTG lung cancer data in the survival R package
data("lung")
head(lung)
?lung

## coxph(formula, data, method) fit a cox regression 

# univariate cox regression analyses
# compute univariate cox analyses for one covariate: sex
uni_res_cox <- coxph(Surv(time, status) ~ sex, data = lung)
summary(uni_res_cox)
# interpretation
#1.Statistical significance. Wald statistic value(0.00149) is significant, the variable set have highly statistically significant coefficients.
#2.Regression coefficients(coef). A positive sign means that the hazard (risk of death) is higher. The beta coefficient (coef) sex = -0.53 indicates that females have lower risk of death (lower survival rates) than males.(1: male, 2: female)
#3.Hazard ratios. exp(coef) = exp(-0.53) = 0.59 gives the effect size of covariates.Being female (sex=2) reduces the hazard by a factor of 0.59, or 41%.
#4.Confidence intervals of the hazard ratios. lower 95% bound = 0.4237, upper 95% bound = 0.816.
#5.Global statistical significance of the model. p-values for three alternative tests for overall significance of the model: The likelihood-ratio test, Wald test, and score logrank statistics.

# apply the univariate coxph function to multiple covariates at once:
covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
# interpretation:
# The variables sex, age and ph.ecog have highly statistically significant coefficients, while the coefficient for ph.karno is not significant.
# age and ph.ecog have positive beta coefficients, while sex has a negative coefficient. Thus, older age and higher ph.ecog are associated with poorer survival, whereas being female (sex=2) is associated with better survival.


# Multivariate Cox regression analysis
# describe how mutiple factors jointly impact on survival
# skip variable ph.karno, because it's not significant in the univariate cox analysis
multi_res_cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
summary(multi_res_cox)

# interpretation:
# The p-value for all three overall tests (likelihood, Wald, and score) are significant, indicating that the model is significant.
# sex. The p-value for sex is 0.000986, with a hazard ratio HR = exp(coef) = 0.58, indicating a strong relationship between the patients’ sex and decreased risk of death.
# ph.ecog. The p-value for ph.ecog is 4.45e-05, with a hazard ratio HR = 1.59, indicating a strong relationship between the ph.ecog value and increased risk of death. Holding the other covariates constant, a higher value of ph.ecog is associated with a poor survival.
# age. the p-value for age is now p=0.23. The hazard ratio HR = exp(coef) = 1.01, with a 95% confidence interval of 0.99 to 1.03. Because the confidence interval for HR includes 1, these results indicate that age makes a smaller contribution to the difference in the HR after adjusting for the ph.ecog values and patient’s sex, and only trend toward significance. 


# visualizing the estimated distribution of survival times
# Plot the baseline survival function
ggsurvplot(survfit(multi_res_cox), data = lung, color = "#2E9FDF",
           ggtheme = theme_minimal())


# Assess the impact of the sex on the estimated survival probability: create new data
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)

sex_df

# plot survival curves for new data
fit <- survfit(multi_res_cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, data = sex_df, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal())




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
