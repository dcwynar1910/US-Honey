#Data importing:
h <- read.csv("US_honey_dataset_updated.csv")

library("usmap")
library("ggplot2")
library("dplyr")
library("psych")



#Data cleaning:
#removes x column
h <- h[,-1]

#checking for na values in columns
any(is.na(h$state))
any(is.na(h$colonies_number))
any(is.na(h$yield_per_colony))
any(is.na(h$production))
any(is.na(h$stocks))
any(is.na(h$average_price))
any(is.na(h$value_of_production))
any(is.na(h$year))

str(h)
sum(is.na(h))

#NO NA VALUES

#Summary information
summary(h)

skew(h$colonies_number)
skew(h$production)
skew(h$yield_per_colony)
# right skew for most variables

var(h$colonies_number)
var(h$production)
var(h$yield_per_colony)
# there is a very high variance between the data points (far away from each other)

# Plot #1
# using all the years combined find the average production for each state
tapp <- tapply(h$production, h$state, mean)
 
p <- data.frame(state = names(tapp), values = tapp)


#adds a space in states with two words
error_state_name <- function(p){
  for(i in 1:nrow(p)){
    if (p[i,1] == "NewYork"){
      p[i,1] <- "New York"
    }
    if (p[i,1] == "NewJersey"){
      p[i,1] <- "New Jersey"
    }
    if (p[i,1] == "NewMexico"){
      p[i,1] <- "New Mexico"
    }
    if (p[i,1] == "NorthDakota"){
      p[i,1] <- "North Dakota"
    }
    if (p[i,1] == "NorthCarolina"){
      p[i,1] <- "North Carolina"
    }
    if (p[i,1] == "SouthCarolina"){
      p[i,1] <- "South Carolina"
    }
    if (p[i,1] == "WestVirginia"){
      p[i,1] <- "West Virginia"
    }
    if (p[i,1] == "SouthDakota"){
      p[i,1] <- "South Dakota"
    }
  }
  return(p)
}
p <- error_state_name(p)
#Plot on a US map, There are 6 states that are not included in the dataset
plot_usmap(data = p, color = "black", exclude =c("NH", "AK","MA", "CT", "DE", "RI"))+
scale_fill_continuous(low = "blue", high = "gold", name = "Honey Production (Pounds)", label = scales::comma)+
labs(title = "Production of Honey in The United States on Average")+
theme(legend.position = "right")

p_list <- p[(order(p$values)),]
p_list <- arrange(p_list, desc(values))

head(p_list, 3)
tail(p_list, 3)


# Plot #2

# The number of stock of each region from 1995 to 2021

region <- function(df){
  df_region <- data.frame(region = c("North East", "Midwest", "South", "West" ), values = c(0,0,0,0) )
  for (i in df$state){
    if ((i == "Arizona") | (i == "Colorado") | (i == "Idaho") |(i == "New Mexico") |(i == "Montana") | (i == "Utah") | (i == "Nevada") | (i == "Wyoming") | (i == "California") | (i == "Hawaii") | (i == "Oregon")| (i == "Washington")){
      #df_region$values[4] <- df_region$values[4] + df[df$state == i, 2]
      df_region$values[4] <- df[df$state == i, 2]
    }
    else if ((i == "Florida") | (i == "Georgia") | (i == "Maryland") |(i == "North Carolina") |(i == "South Carolina") | (i == "Virginia") | (i == "West Virginia") | (i == "Alabama") | (i == "Kentucky") | (i == "Mississippi") | (i == "Tennessee")| (i == "Arkansas")| (i == "Louisiana")| (i == "Oklahoma")| (i == "Texas")){
      df_region$values[3] <- df[df$state == i, 2]
    }
    else if ((i == "Indiana") | (i == "Illinois") | (i == "Michigan") |(i == "Ohio") |(i == "South Carolina") | (i == "Wisconsin") | (i == "Iowa") | (i == "Kansas") | (i == "Minnesota") | (i == "Missouri") | (i == "Nebraska")| (i == "North Dakota")| (i == "South Dakota")){
      df_region$values[2] <- df[df$state == i, 2]
    }
    else{
      df_region$values[1] <- df[df$state == i, 2]
    }
  }
  return(df_region)
}

for (i in unique(h$year)){
  # the 4 column is the number of stock for the region per year
  df_now <- h[c(h$year == i), c(1,4)]
  colnames(df_now)<-c("state", "values")
  df_now <- error_state_name(df_now)

  df_now <- region(df_now)

  per <- round(df_now$values/sum(df_now$values)*100)
  names <- paste(df_now$region, per)
  names <- paste(names,"%",sep="")


  pie(df_now$values,labels = names, main = paste(c("Percent of Stock in Pounds for US Regions", i)))
  Sys.sleep(1)
}


# Exploratory Data Analysis

# Answers the problem asked:
# Has climate change/other factors affected the yield of honey and colony numbers of 
# honeycombs in the U.S.?
# Yes it most likely could be that climate change
# affects colonies because as the people try to save the colonies 
# by growing the number, their yield continues to decline

group_by_year_colony <- function(x){
  by_year <- group_by(x,x$year)
  df <- summarise(by_year, avg_col = mean(colonies_number))
  colnames(df) <- c("year", "value")
  return(df)
}

group_by_year_yield <- function(x){
  by_year <- group_by(x,x$year)
  df <- summarise(by_year, avg_yield = mean(yield_per_colony))
  colnames(df) <- c("year", "value")
  return(df)
}

group_by_year_prod <- function(x){
  by_year <- group_by(x,x$year)
  df <- summarise(by_year, avg_yield = mean(production))
  colnames(df) <- c("year", "value")
  return(df)
}

colonies_growth <- group_by_year_colony(h)

colonies_yield <- group_by_year_yield(h)

colonies_prod <- group_by_year_prod(h)


par(mfcol=c(1,3), mar = c(5,4,4,2))

plot(colonies_growth$year, colonies_growth$value, type = "l", main = "Colony Growth", ylim = c(55000, 70000),xlab = "", ylab = "", col = "purple")
mtext(text = "Colonies", side = 2, line = 2)
mtext(text = "Years(1995-2021)", side = 1, line = 2)

plot(colonies_yield$year, colonies_yield$value, type = "l", main = "Yield Per Colony", xlab = "", ylab = "", col = "green")
mtext(text = "Yield (Pounds)", side = 2, line = 2)
mtext(text = "Years(1995-2021)", side = 1, line = 2)

options(scipen=999)
plot(colonies_yield$year, colonies_prod$value, type = "l", main = "Average Production", xlab = "", ylab = "", col = "red")
# graph is in millions 
mtext(text = "Production (Pounds)", side = 2, line = 2)
mtext(text = "Years(1995-2021)", side = 1, line = 2)
 
 
# YEILD FOR 1995
h_1995 <- filter(h, year == 1995)
yield_1995 <- data.frame(state = h_1995$state, values = h_1995$yield_per_colony)

yield_1995 <- error_state_name(yield_1995)

plot_usmap(data = yield_1995, color = "white", exclude =c("NH", "AK","MA", "CT", "DE", "RI"))+
scale_fill_continuous(low = "black", high = "green", name = "Honey Yield (Pounds)", label = scales::comma)+
labs(title = "Yield of Honey per Colony in The United States (1995)")+
theme(legend.position = "right")


# YEILD FOR 2021
h_2021 <- filter(h, year == 2021)
yield_2021 <- data.frame(state = h_2021$state, values = h_2021$yield_per_colony)

yield_2021 <- error_state_name(yield_2021)

plot_usmap(data = yield_2021, color = "white", exclude =c("NH", "AK","MA", "CT", "DE", "RI"))+
scale_fill_continuous(low = "black", high = "green", name = "Honey Yield (Pounds)", label = scales::comma)+
labs(title = "Yield of Honey per Colony in The United States (2021)")+
theme(legend.position = "right")









