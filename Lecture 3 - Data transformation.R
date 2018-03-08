### Lecture 3
# Install the tidyverse package. All the packages that we will use today are included in it
install.packages("tidyverse")

# Let's try the pipe operator
# The pipe is in several packages, for e.g. the magrittr package
library(magrittr)

# Take the following vector
x <- c(55:120, 984, 552, 17, 650)

# Creating a pipeline of commands. Of course, the sorting does not change the result
x %>%
    sort() %>%
    subtract(5) %>%
    divide_by(3) %>%
    sd()

# [1] 46.02999
# Let's load the dplyr package, which is for data transformation in data frames
library(dplyr) 

# Let's use the ToothGrowth data once again, and practice with the pipe opeartor
# Filter the rows that 
ToothGrowth %>%
    filter(supp == "OJ")

# Let's creare a new variable, which which returns tooth length in cm, not mm. Use mutate()
ToothGrowth %>%
    mutate(len_cm = len / 10)

# Let's see the average tooth length in centimeters, use summarise()
# This takes several data and summarizes it accoring to our wishes
# The result will not contain the original data
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm))

# Let's also calculate the nuber of cases, using the function n()
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# It makes the most sense if we also create groups but BEFORE summarise, using group_by()
tooth_results <-
    ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# You ca also use the grouping with mutate. Then it adds the group means and number of cases to the original data
# This way, the result will also contain the original data AND the summary statistics with redundancy
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    mutate(mean_len_cm = mean(len_cm),
           cases = n())

# We can also arrange the results based on a variable
tooth_results %>% 
    arrange(mean_len_cm)


# Practice on the gapminder data. First install it, than load the data 
install.packages("gapminder")
gapminder <- gapminder::gapminder

# Gapminder contains the data of several countries at tifferent times.
# It is in tidy format. Check the codebook
?gapminder::gapminder

# Task 1 solution
solution_1 <-
    gapminder %>% 
    filter(year %in% c(1952, 1957)) %>%
    group_by(continent) %>% 
    summarise(life_exp_med = median(lifeExp)) %>% 
    arrange(-life_exp_med)

# Task 1 data viz   
library(ggplot2)
solution_1 %>% 
    ggplot() +
        aes(x = continent, y = life_exp_med) +
        geom_col()


# Task 2 solution
solution_2 <-
    gapminder %>% 
    filter(country %in% c("Hungary","Slovak Republic","Austria")) %>% 
    group_by(country) %>% 
    mutate(mean_pop = mean(pop),
           cent_pop = pop - mean_pop)

solution_2 %>% 
    ggplot() +
        aes(x = year, y = cent_pop, group = country, color = country) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 0) +
        scale_y_continuous()
    

# Tidy data
library(tidyr)

# We will use the who data from the tidyr package
# Check the codebook
data(who)
?who
# gather arranges data to long format
# you have to give a name that will store

who_long <- 
    who %>% 
    gather(key = variable, value = value, new_sp_m014:newrel_f65)

# You can see a lot of missing values (NA) that you can easily remove
who_long <- 
    who_long %>% 
    drop_na(value)

# According to the codebook, there are several things encoded in these variables, that is not tidy
# For example, ˙new_` in the vairable name does not contain information, so let's remove it
# To make operations on strings, let's use the stringr package, also from the tidyverse

library(stringr)

who_long %>% 
    mutate(variable = str_replace(variable, "new_",""))
    

# This way, the variable contains 3 different information: test result, gender, and age
# Let's separate the test result first

who_long %>% 
    mutate(variable = str_replace(variable, "new_","")) %>% 
    separate(col = variable, into = c("test_result","gender_age"), sep = "_")

# We still need to separate the gender from the age
who_tidy <-
    who_long %>% 
    mutate(variable = str_replace(variable, "new_","")) %>% 
    separate(col = variable, into = c("test_result","gender_age"), sep = "_") %>% 
    mutate(gender = gender_age %>% substring(1,1),
           age = gender_age %>% substring(2))

# Now we can verify what age groups we have
who_tidy %>% 
    distinct(age)

# We can also transform the data to wide format, for e.g. the age groups. 
who_tidy %>% 
    spread(age, value)


    
    
    
    
