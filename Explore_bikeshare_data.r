
library(ggplot2)
library(dplyr)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

str(ny)

head(wash)

str(wash)

head(chi)

str(chi)

#Define empty variable
new <- NA

#Add empty Gender column to wash
wash['Gender'] <- new

#Add empty Birth.Year column to wash
wash['Birth.Year'] <- new

head(wash)

ny['City'] <- 'New York City'
wash['City'] <- 'Washington'
chi['City'] <- 'Chicago'

head(ny)

head(wash)

head(chi)

#Create function that binds df2 to df1
concat <- function(df1, df2){
    return(rbind(df1, df2))
}

#Call concat function to bind wash to ny
df_cities <- concat(ny, wash)

#Call concat function to bind chi to df_cities
df_cities <- concat(df_cities, chi)

head(df_cities)

tail(df_cities)

str(df_cities)

summary(df_cities)

#Find average trip duration using group_by()
avg_table <- df_cities %>% 
    group_by(City) %>%
        summarise(avg_duration = mean(Trip.Duration, na.rm = TRUE))

avg_table

#Visualize average trip duration data
ggplot(aes(x = City, y = avg_duration), data = avg_table) +
    geom_bar(stat = 'summary', fun.y = 'mean', fill = 'dark green') + 
    ggtitle('Average Trip Duration for Users in Various Cities') +
    labs(y = 'Avg Trip Duration', x = 'City') +
    coord_flip()

#Create new dataframe by using concat() function
df_gender <- concat(ny, chi)

df_gender

#Find rider gender counts using group_by()
gender_table <- df_gender %>% 
    group_by(Gender, City) %>%
        summarise(gender_count = n())
gender_table

#Visualize rider gender count data
ggplot(aes(x = Gender, fill = City), data = df_gender) +
    geom_bar(position = 'dodge') +
    ggtitle('Rider Gender Count by City') +
    scale_x_discrete(labels = c('NA', 'Female', 'Male')) +
    labs(y = 'Count of Riders', x = 'Gender') +
    scale_fill_manual('Legend', values = c('Chicago' = 'light blue', 'New York City' = 'orange'))

#Find rider gender percentages using group_by()
gender_percent_table <- gender_table %>% 
    group_by(City) %>%
        mutate(total_count = sum(gender_count)) %>%
            group_by(Gender, add = TRUE) %>%
                mutate(percent = round(100*gender_count/total_count,2))
gender_percent_table

#Visualize rider gender percentage data
ggplot(aes(x = Gender, y = percent, fill = City), data = gender_percent_table) +
    geom_bar(position = 'dodge', stat = 'identity') +
    ggtitle('Rider Gender % by City') +
    scale_x_discrete(labels = c('NA', 'Female', 'Male')) +
    labs(y = 'Percent of Riders', x = 'Gender') +
    scale_fill_manual('Legend', values = c('Chicago' = 'light blue', 'New York City' = 'orange'))

system('python -m nbconvert Explore_bikeshare_data.ipynb')
