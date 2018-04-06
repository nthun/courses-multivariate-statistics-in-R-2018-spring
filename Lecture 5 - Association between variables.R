# Lecture 5 - Association between variables
# Load these packages
library(tidyverse)
library(broom)

# Read the cocktail dataset from my github repo
cocktails <- read_tsv("https://raw.github.com/nthun/cocktail-balance/master/cocktail_data.tsv")
cocktails <- read_tsv("http://bit.ly/2zbj7kA") # Same stuff, but shortened url

# Let's examine how alcohol content(abv), acid content(acid) are associated in the data.
# As a bonus, color the data points by type, and also write the names

# Task solution
cocktails %>% 
    ggplot() +
    aes(x = acid, y = abv, label = name) +
    geom_point(aes(color = type), size = 3) +
    geom_smooth(method = "lm")

# Generate data for plots
set.seed(1) # Sets up the random seed number
rand_num <- rep(1:5*10, 20) # Creates 20 repetitions of the numbers 10 to 50 with 10 increments
# Use random distributions for showing different correlation directions and magnitudes
corr_df <- 
    data_frame(x = rnorm(length(rand_num), rand_num, 5), 
               positive = rnorm(length(rand_num), rand_num, 10), 
               group = rand_num,
               negative = -positive + 50,
               no_correlation = rnorm(length(rand_num), 25, 25),
               weak_correlation = x + rnorm(length(x), 20, 40),
               moderate_correlation = x + rnorm(length(x), 12, 25),
               strong_correlation = positive) %>% 
    gather(key, value, -x, -group) %>% 
    mutate(key = fct_relevel(key, "positive","negative","no_correlation","weak_correlation","moderate_correlation","strong_correlation"))

# Plot correlations of different direction
corr_df %>% 
    filter(key %in% c("positive","negative","no_correlation")) %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~key)

# Plot correlations of different magnitude
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    ggplot() + 
    aes(x = x, y = value) +
    geom_point(alpha = .4) +
    geom_smooth(method = "lm", color = "red", size = 1.5) +
    facet_wrap(~key, scales = "free_y")

# It is possible that a different variable is responsible for the correlation, 
corr_df %>% 
    filter(key == "positive") %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~group, ncol = 5) +
        geom_smooth(method = "lm", color = "red", size = 1.5)

# Plotting the meaning of covariance
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel) # This is used to plot non-overlapping text labels
cocktails %>% 
    select(index, name, abv, sugar) %>% 
    head(10) %>% 
    gather(property, value, abv:sugar) %>% 
    group_by(property) %>% 
    mutate(mean_value = mean(value)) %>% 
    ggplot() +
        aes(x = index, y = value, label = name) +
        geom_point(size = 3) +
        geom_hline(aes(yintercept = mean_value), size = 1.5, alpha = .5) +
        geom_segment(aes(xend = index, yend = mean_value), linetype = "dashed", color = "red", size = 1.2) +
        geom_text_repel() +
        facet_wrap(~property, nrow = 2, scales = "free_y")

### LISTS
# We have a list object, with 4 different plots
# The names of the list elements are the same as the unique values of the Embark variable in titanic_train
plots %>% names()

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

# Indexing a list can be a bit difficult first. There are 3 ways of subsetting a list
# [ returns a sub-list. The result will always be a list
a[1:2] %>% str()

# [[ returns a single component of a list. It removes the level of hierarchy
a[[1]] %>% str()
a[[4]] %>% str() # This remains a list, because this was a list in a list

# You can use $ as a shorthand for extracting named elements
a$a
# is the same as
a[["a"]]

# Let's write these plots to 4 separate files to the test_dir subdirectory
walk(a, ggsave(str_glue()))

## Correlation in R
# The simplest way is the built in cor() function
cor(cocktails$abv, cocktails$sugar)

# To get a correlation matrix, simly do it on a df
# You can set the method as pearson, spearman, or kendall correlation
cocktails %>% 
    select(abv:sugar) %>% 
    cor(method = "spearman")

# But to get p values, you need to run cor.test(). This can only be done in pairs of variables.
# This returns a more verbose output
cor.test(cocktails$abv, cocktails$sugar)

# Calculate the correlations in the tidyverse way, you have to use summarise. But you have to reshape the data first
cocktails %>% 
    select(name:sugar) %>% 
    gather(property, value, -name, -abv) %>% 
    group_by(property) %>% 
    summarise(r = cor(x = abv, y = value) %>% round(2),
              p = cor.test(x = abv, y = value, method = "pearson")$p.value)

# To get significance of the values in the matrix, we should use the psych paclage. corr.test() returns correlations, sample size, and p-values. It can also correct the significance for multipe comparisions, and calculate confidence intervals. 
if (!require(psych)) install.packages("psych")
library(psych)
cocktails %>% 
    select(abv:sugar) %>% 
    corr.test()

# Visualize correlation matrices. Best to use the GGally package.
# By default, ggpairs() shows scatter plots (all variables by all variables), density plots, and the actual correlation values. You can also add several features, check: https://ggobi.github.io/ggally/
if (!require(GGally)) install.packages("GGally")
library(GGally)

cocktails %>% 
    select(abv:sugar) %>% 
    ggpairs(lower = list(continuous = "smooth", color = "red"))

## Normality assumption
# Checking normality     
x <- data_frame(x = rnorm(10000, 0, 1)) # generate 10,000 random numbers
ggplot(x) + geom_histogram(aes(x))
ggplot(x) + geom_density(aes(x), fill = "grey40")
ggplot(x) + geom_qq(aes(sample = x))

# Transform the data
cocktails_trans <- 
    cocktails %>% 
    mutate(abv = abv %>% log())

ggplot(cocktails_trans) + geom_histogram(aes(x = abv), bins = 20)
ggplot(cocktails_trans) + geom_density(aes(x = abv), fill = "grey40")
ggplot(cocktails_trans) + geom_qq(aes(sample = abv))

cocktails %>% 
    pull(abv) %>% 
    shapiro.test()

cocktails %>% 
    select(abv:sugar) %>% 
    cor.ci()

# Do it on the tidyverse way
# Use do if you don't want to cram your results into one predefined variable. This way, you will get a nested dataframe
# Moreover, broom::tidy helps you to uniformize outputs, and put them in a data frame
# You can also create nested data frames, in which your cells will contain data frames
# You can unnest them using the unnest() function
cocktails %>% 
    do(sw = shapiro.test(.$abv) %>% tidy()) %>% 
    unnest(sw)

# This methods shows its real power when you do it on several variables.
# To do that, first you have to put your data into long format using gather()
cocktails %>% 
    gather(key, value, abv:sugar) %>% 
    group_by(key) %>% 
    do(sw = shapiro.test(.$value) %>% tidy()) %>% 
    unnest(sw)

cor.test(cocktails$abv, cocktails$sugar, method = "spearman")

cocktails %>% 
    select(abv:sugar) %>% 
    cor.ci(method = "spearman", n.iter = 1000) # Specify number of iterations


# Compare correlations using psych::paired.r()
paired.r(-.47, -.67, n = 55)

# If you provide n2, you can specify the sample size for the variables independently
paired.r(-.47, -.67, n = 55, n2 = 550)

### LISTS
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

# Indexing a list can be a bit difficult first. There are 3 ways of subsetting a list
# [ returns a sub-list. The result will always be a list
a[1:2] %>% str()

# [[ returns a single component of a list. It removes the level of hierarchy
a[[1]] %>% str()
a[[4]] %>% str() # This remains a list, because this was a list in a list

# You can use $ as a shorthand for extracting named elements
a$a
# is the same as
a[["a"]]

# This can be important, because statistical tests often return results that are lists
# for e.g. the shapiro test returns a list of 4 elements

cocktails %>% 
    pull(abv) %>% 
    shapiro.test() %>%
    str()

# Chi squared test
# Use the survey from the MASS package
?MASS::survey
survey <- MASS::survey

# We want to test whether smoking and exercise frequency are independent of each other

# First we create a contingency table
smoke_exer <- table(survey$Smoke, survey$Exer)

# The order of the categories is scrambled, so we should sort reorder them
survey <-
    survey %>% 
    as_tibble() %>% 
    mutate(Smoke = fct_relevel(Smoke, c("Never", "Occas", "Regul", "Heavy")),
           Exer = fct_relevel(Exer, c("None", "Some", "Freq")))

# Do the table againwith the good order
smoke_exer <- table(survey$Smoke, survey$Exer)

chisq.test(smoke_exer)
# It seems like smoking is not 

# Show the frequencies on a tile plot. I'm log transforming the frequencies to make them show better
survey %>% 
    group_by(Smoke, Exer) %>% 
    count() %>% 
    drop_na() %>% 
    ggplot() +
    aes(x = Exer, y = Smoke, fill = n %>% log()) +
    geom_tile()

