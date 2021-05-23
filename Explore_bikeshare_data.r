
n = read.csv('new_york_city.csv')
w = read.csv('washington.csv')
c = read.csv('chicago.csv')
library(ggplot2)

head(n)

head(w)

head(c)

# i. What is the count of each User Type



# New York
table(n$User.Type)

#Chicago
table(c$User.Type)

#Washington
table(w$User.Type)

# ii. what are the counts of each gender

# New York
table(n$Gender)

#Chicago
table(c$Gender)

# iii. what are the ealierst, most recent and most common year of birth

#New York
table(n$Birth.Year)

#Chicago
table(c$Birth.Year)

# i. What is the total travel time for users in different cities

#New York

by(n$Trip.Duration/60, n$User.Type, sum)

#Washington

by(w$Trip.Duration/60, w$User.Type, sum)

#Chicago
by(c$Trip.Duration/60, c$User.Type, sum)
## Histogram of Travel time 

#New York
ggplot(aes(x=Trip.Duration/60), data =n)+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Number of Minutes Spent on Trip')+
   ylab('Number of Users')+
   ggtitle('Histogram of Trip Duration in New York')


#Washington
ggplot(aes(x=Trip.Duration/60), data =w)+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Number of Minutes Spent on Trip')+
   ylab('Number of Users')
   ggtitle('Histogram of Trip Duration in Washington')
   
   
#Chicago
ggplot(aes(x=Trip.Duration/60), data =c)+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Number of Minutes Spent on Trip')+
   ylab('Number of Users')+
   ggtitle('Histogram of Trip Duration in Chicago')



# What is the average travel time for users in different cities

##New York

by(n$Trip.Duration/60, n$User.Type, summary)

## Washington
by(w$Trip.Duration/60, w$User.Type, summary)


## Chicago
by(c$Trip.Duration/60, c$User.Type, summary)


#Histogram of Trip Duration By User Type
#New York
ggplot(aes(x=Trip.Duration/60), data =subset(n, !is.na(User.Type)))+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Number of Minutes Spent on Trip')+
   ylab('Number of Users')+
   ggtitle('Histogram of Trip Duration in New York by User Type')+
   facet_wrap(~User.Type)


#Washington
ggplot(aes(x=Trip.Duration/60), data =subset(w, !is.na(User.Type)))+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Time Spent of Trip in Minutes')+
   ylab('Number of Users')+
   ggtitle('Histogram of Trip Duration in Washington by User Type')+
   facet_wrap(~User.Type)


#Chicago
ggplot(aes(x=Trip.Duration/60), data =subset(c, !is.na(User.Type)))+
   geom_histogram(color='black', fill='red')+
   scale_x_continuous(limits = c(0,60), breaks = seq(0,75, 10))+
   xlab('Number of Minutes Spent on Trip')+
   ylab('Number of Users')+
   ggtitle('Histogram of Trip Duration in Chicago by User Type')+
   facet_wrap(~User.Type)

## New York
qplot(x=User.Type, y=Trip.Duration/60, data = n, geom = 'boxplot')+
   coord_cartesian(ylim = c(0,40))+
   xlab('Type of User')+
   ylab('Trip Duration in Minutes')+
   ggtitle('Distribution of Trip Duration among different users in New York')


## Washington
by(w$Trip.Duration/60, w$User.Type, summary)
qplot(x=User.Type, y=Trip.Duration/60, data = w, geom = 'boxplot')+
   coord_cartesian(ylim = c(0,50))+
   xlab('Type of User')+
   ylab('Trip Duration in Minutes')+
   ggtitle('Distribution of Trip Duration among different users in Washington')


## Chicago
by(c$Trip.Duration/60, c$User.Type, summary)
qplot(x=User.Type, y=Trip.Duration/60, data = c, geom = 'boxplot')+
   coord_cartesian(ylim = c(0,50))+
   xlab('Type of User')+
   ylab('Trip Duration in Minutes')+
   ggtitle('Distribution of Trip Duration among different users in Chicago')



#i. Distribution of Trip Duration by year of birth 

##New York

ggplot(aes(y=Trip.Duration/60, x=Birth.Year), data = n)+
   geom_point(alpha=1/20, position= position_jitter(h=0),
              color='blue')+
   coord_cartesian(ylim = c(0,40),xlim = c(1940,2000))+
   xlab('Birth Year')+
   ylab('Trip Duration')+
   ggtitle('Distribution of Trip Duration by year of birth in New York')


##Chicago
ggplot(aes(y=Trip.Duration/60, x=Birth.Year), data = c)+
   geom_point(alpha=1/10, position= position_jitter(h=0),
              color='blue')+
   coord_cartesian(ylim = c(0,40),xlim = c(1940,2000))+
   xlab('Birth Year')+
   ylab('Trip Duration')+
   ggtitle('Distribution of Trip Duration by year of birth in Chicago')


# Trip Duration by Gender
##New York
ggplot(aes(y=Trip.Duration/60, x=Gender), data = subset(n, !is.na(Gender)))+
   geom_boxplot(color='black', fill='green')+
   coord_cartesian(ylim = c(0,40))+
   xlab('Gender')+
   ylab('Trip Duration')+
   ggtitle('Distribution of Trip Duration by Gender in New York')


by(n$Trip.Duration/60,n$Gender, summary) 

##Chicago
ggplot(aes(y=Trip.Duration/60, x=Gender), data = subset(c, !is.na(Gender)))+
   geom_boxplot(color='black', fill='green')+
   coord_cartesian(ylim = c(0,40))+
   xlab('Gender')+
   ylab('Trip Duration')+
   ggtitle('Distribution of Trip Duration by Gender in Chicago')


by(c$Trip.Duration/60,c$Gender, summary)


## More questions on Stations

#What is the commonest Start station?

# New York Start.Station
table (n$Start.Station)


#Washington Start.Station
table(w$Start.Station)


#chicago Start.Station
table(c$Start.Station)


# What is the commonest End Station?


# New York End.Station
table (n$End.Station)

#Washington End.Station
table(w$End.Station)

#chicago End.Station
table(c$End.Station)