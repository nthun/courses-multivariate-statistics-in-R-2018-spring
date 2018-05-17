# PCA, Factor analysis
library(tidyverse)
library(GGally)
library(psych)
library(broom)

# Principal component analysis
# Using the Eyesenk Personality Inventory + Big5 inventory 
eb <-
    epi.bfi %>% 
    select(epiE:bfopen) %>% 
    as_tibble()

ggpairs(eb)

# Do pca with default settings
eb_pca <- pca(eb)
eb_pca

# Get communality for each variable
eb_pca$communality

# Estimate the optimal number of factors
nfactors(eb)
# Parallel analysis to estimate the optimal number of factors
fa.parallel(eb, fa = "pc")

# Do the pca with 3 factors
pca(eb, nfactors = 3)

# Let's try rotating the factors
eb_pca_rot <- pca(eb, nfactors = 3, rotate = "varimax")
eb_pca_rot



# Show loadings properly
eb_loadings <- 
    loadings(eb_pca_rot) %>% 
    unclass() %>% 
    tidy() %>% 
    mutate_if(is_numeric, round, 2) 

# Create a plot to show component loadings
eb_loadings %>% 
    gather(component, loading, RC1:RC3) %>% 
    mutate(sign = if_else(loading >= 0, "positive", "negative")) %>% 
    ggplot() +
        aes(y = loading %>% abs(), x = .rownames %>% fct_rev(), fill = sign, label = loading) +
        geom_col(position = "dodge") +
        coord_flip() +
        geom_text() +
        facet_wrap(~component) +
        labs(y = "Loading strength", x = "Variable")

# Ok, so how are we using this information?
# Extract component scores into a tibble
scores <- pca(eb, nfactors = 3, rotate = "oblimin")$score %>% 
    as_tibble()

# You can use these loadings from further on as any other varable. 
# E.g. check the correlations with the diagnostic scales from the original dataset
bind_cols(epi.bfi %>% select(bdi:stateanx), scores) %>% 
    ggpairs()

# we can also save the factor loadings as keys for creating scales
factor2cluster(eb_pca_rot, cut = .7)

# Assumptions to check
# KMO test should be 
KMO(eb)
# Bartlett test should be non-significant
cortest.bartlett(eb)


# Exploratory factor analysis
# Using the Big Five Inventory to find the five factors
bfi <- 
    psych::bfi %>% 
    as_tibble() %>% 
    select(A1:O5)

# Finding the optimal number of factors
psych::nfactors(bfi, rotate = "varimax")
psych::fa.parallel(bfi, fa = "fa", fm = "minres")

bfi_efa <- psych::fa(bfi, nfactors = 6, scores = "regression", fm = "ml", rotate = "varimax")

# Show which variables load to which factor the most, and what is the direction
psych::factor2cluster(bfi_efa)
