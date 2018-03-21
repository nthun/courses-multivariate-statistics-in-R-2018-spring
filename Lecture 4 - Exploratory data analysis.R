# Lecture 4 - EDA
library(dplyr)

# Let's use the titanic dataset
install.packages("titanic")
library(titanic)

# Check the codebook to identify variables
?titanic_train

# Solution for Titanic question 1
titanic_train %>% 
    filter(Age == 50 & Sex == "female" & Pclass == 1, SibSp + Parch == 0)

# Data importing
# The easies way is to use data import from the "Files" window


# IMPORTANT!! IN ORDER TO USE THIS CODE, YOU ALSO HAVE TO DOWNLOAD THE DATAFILES FROM THE COURSE'S PAGE, THAT YOU CAN FIND IN THE DATASETS LIBRARY! https://osf.io/xcvn7/

# R has its own data format (.RData), that stores variables from R. You can load an R object using the load() function.
# Usually, it is enough to provide the file name (with the relative or absolute path), but import functions often has optional parameters

load("datasets/movies.RData")

# You can save your variables using the save() function. It requires two parameters: the variable to save, and the filename to save to.

x <- c("apple","orange","pinaple")
save(x, file = "datasets/fruits.RData")

# For reading common file formats, we will use the readr package (from tidyverse)
library(readr)

# A very common file format in the data analysis community is .csv. It means comma separated values, which is a quite literal description. It is a simple text file, but commas distinguish variables and values from each other. You can import a csv file using

read_csv("datasets/movies.csv")

# Note that as we did not assign the result of the function to a variable, it is not stored. So let's try

m1 <- read_csv("datasets/movies.csv")

# To save a dataset to csv format, you can just use the write_csv() function. It requires two parameters: the data frame to save, and the filename to save to.

write_csv(m1, "datasets/movies.csv")

# tsv is almost the same as csv, but tabs are used instead of commas
cocktails <- read_tsv("datasets/cocktail_data.tsv")

# Writing to tsv... yes, you have guessed it: write_tsv()
# Reading text files is also easy. read_lines() will read the file line by line, and create a vector 
# Here, check out the complete works of Shakespeare:

shakespeare <- read_lines(file = "datasets/shakespeare_all_works.txt")

# Remember that you can read files directly from the internet, if you use the url() function:
# We can also skip the first 244 lines as it only contains licence info
shakespeare <- read_lines(url("https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"), skip = 244)

# To read Excel files, we will use the readxl package
library(readxl)

# For excel files, you also have to specify the worksheet that you want to access. You can do it by sheet name or position. So the next 2 commands return the same result
read_excel(path = "datasets/cocktails.xlsx", sheet = 1)
read_excel(path = "datasets/cocktails.xlsx", sheet = "Sheet 1")

# You can also read SPSS (and SAS, etc.) formats by using the haven package
library(haven)

read_spss("datasets/movies.sav")


### Exploratory data analysis
# To examine the distribution of a categorical variable, use a bar chart
?diamonds

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut))

# See the number of observations grouped by the cut variable
diamonds %>% 
    count(cut)

# To examine the distribution of a continuous variable, use a histogram
ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Typical values
summary(diamonds$carat)
ggplot(diamonds) +
    geom_density(aes(x = carat), fill = "red")

# Unusual values
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# To make it easy to see the unusual values, we need to zoom to small values of the y-axis with coord_cartesian()
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))

# Let's save the unusual values that are smaller than 3 and larger than 20
unusual <- 
    diamonds %>% 
    filter(y < 3 | y > 20) %>% 
    select(price, x, y, z) %>%
    arrange(y)
# Let's check the unusual values
unusual

# Instead of excluding the variables entirely, let's just make the outlier values NA
# This way, you keep the other values of the observation, that may not be invalid. Also, you don't have to make note somewhere else that you actually had to remove cases. This information remains in your data.
diamonds2 <- diamonds %>% 
    mutate(y = if_else(y < 3 | y > 20, NA_real_, y))

# Missing values will be always shown as a warning
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
    geom_point()

### COVARIATION
# It is possible to see differing distributions in a continuous vaiable for separate categorical levels
# To check  frequency distribution of price by cut, we can use frequency polygons (similar to histograms)
ggplot(data = diamonds) + 
    aes(x = price) +
    geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# Or you can choose to use density plots, those show relative frequencies (but the purpose is similar)
ggplot(data = diamonds) + 
    aes(x = price, fill = cut, color = cut) +
    geom_density(alpha = .3)

# However, this does not look that great, so let's check out a visualization that shows distributions in a way that is is also readable. For this, we need the ggridges package
install.packages("ggridges")
library(ggridges)
ggplot(diamonds) +
    aes(x = price, y = cut) +
    geom_density_ridges()

# You can also use a violin plot (or bean plot) for the same thing
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_violin()

# Checking the covariation of a continuous and a categorical variable
# To check the typical values along with a distribution summary in a plot, use boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

# We can also reorder values based on the median (central line in boxplot)
ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
    geom_boxplot()

# Two categorical variables
# Comparing how color and cut covary, we can simply calculate the number of cases
diamonds %>% 
    count(color, cut)

# But we are better off if we also visualize the results 
# For e.g. we can use the count plot. This shows the number of cases by size of the dots
ggplot(data = diamonds) +
    geom_count(mapping = aes(x = cut, y = color))

# We can also make a heatmap, where we visualize the frequency of cases as color density
# In this example, darker colors mean less and brither colors mean more cases
diamonds %>% 
    count(color, cut) %>%  
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# Two continuous variables
# The most obvious visualization is the simple scatter plot
ggplot(data = diamonds) +
    geom_point(mapping = aes(x = carat, y = price))

# Or by binning the data points that are close to each other
ggplot(diamonds) +
    geom_bin2d(mapping = aes(x = carat, y = price))

# You can also use hexagon bins instead of rectangles
ggplot(diamonds) +
    geom_hex(mapping = aes(x = carat, y = price))


### PRACTICE ON TITANIC DATA
# Solution for Titanic question 2
titanic_train %>% 
    group_by(Pclass, Sex, Survived) %>% 
    ggplot() +
    aes(x = Pclass, fill = Sex) +
    geom_bar(position = "dodge") +
    facet_wrap(~Survived)

# Solution for Titanic question 3
titanic_df <-
    titanic_train %>% 
    as_tibble() %>% 
    filter(Parch == 0 & SibSp == 0) %>% 
    select(PassengerId, Sex, Age, Pclass, Fare, Survived) %>% 
    drop_na() %>% 
    group_by(Pclass) %>% 
    mutate(med_price = median(Fare)) %>% 
    ungroup()
