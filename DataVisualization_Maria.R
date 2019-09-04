
#---- Pie Chart ------

# Create data for graph

x <- c(44,39,15,12,53)
P <- c("Mumbai","Dehli","Kolkata","Chennai","Banglor")

# Ex 1
pie(x,P)

# Ex2
pct <- round((x/sum(x)*100))
lbl <- paste(P,pct)  # add percent to label
lbl1 <- paste(lbl,"%",sep = "")  # add % to label
pie(x,labels = lbl1,col = rainbow(length(lbl1)),main = "City_Pie_Chart")

# Ex3 - Pie chart with Legend
legend("topright",c("Mumbai","Dehli","Kolkata","Chennai","Banglor"),cex = 0.5,fill = rainbow(length(x)))

# Ex4 - 3D Pie Chart
library(plotrix)
pie3D(x, labels = lbl1, explode = 0.1, main = "City_Pie_Chart")

# ------ Bar Chart ------

H <- c(24,42,52,22,15)
B <- c("Mumbai","Dehli","Kolkata","Chennai","Banglor")

# Ex1
barplot(H)

# Ex2
barplot(H,xlab = "Month",ylab = "Happiness Index",
        col = "red",names.arg = B,
        main = "Happiness Index",border = "Black")

# Ex3 - Horizontal Bar Chart
barplot(H,xlab = "Month",ylab = "Happiness Index",
        horiz = TRUE,col = "blue",names.arg = B,
        main = "Happiness Index",border = "Black")

# Ex4 - Stacked Bar Chart with colours and Legends
counts <- table(mtcars$vs,mtcars$gear)
barplot(counts,main = "Car Distribution By Gears & VS",
        xlab = "Number of Gears",
        col = c("darkblue","red"),
        legend = rownames(counts))

# Ex5 - Grouped Bar Chart
barplot(counts,main = "Car Distribution By Gears & VS",
        xlab = "Number of Gears",
        col = c("darkblue","red"),
        legend = rownames(counts),
        beside = T)

# -------- Histogram -----------

# Plotting distribution of AirPassengers Data
hist(AirPassengers,
     main = "Histogram for AirPassengers",
     xlab = "Passengers",
     col = "blue",
     xlim = c(100,700),
     breaks = 5)

# --------- BoxPlot ------------

boxplot(data = mtcars, mpg ~ cyl,
        xlab = "Number of Cylinder",
        ylab = "Miles Per Gallon",
        main = "Mileage Data")

# --------- Scatter Polt ------------

# Plot the chart for cars with weight between 2.5 to 5 & Milage between 15 & 30
plot(x = mtcars$wt,
     y = mtcars$mpg,
     xlim = c(3.5,5),
     ylim = c(15,30),
     main = "Weight Vs Milage")

# Scatter Plot Matrices - use - to find correlation between variable
pairs(formula = ~wt+mpg+disp+cyl,
      data = mtcars,
      main = "Scatter Plot Matrix")

# --------- Line Plot --------------

# Ex1 - Simple Line
L <- c(12,42,72,15,16,27)
L1 <- c(15,14,11,3,62,12)
l1 <- c(1:10)
plot(L)
plot(L,type = "o")

# Ex2 
plot(L,type = "o",
     col = "red",
     xlab = "Month",
     ylab = "Rain Fall",
     main = "Rain Fall Chart")

# Ex3 - Multiple Line Plot
plot(L, type = "o",
     col = "blue",
     xlab = "Month",
     ylab = "Rain Fall",
     main = "Rain Fall Chart")
lines(L1,type = "o",col = "red")


# ------------------------- GGPLOT(Grammer of Graphics) ---------------------------------

# Setup
options(scipen=999)  # turn off scientific notation like 1e+06 
library(ggplot2)        # for generating the visualizations
library(dplyr)          # for data manipulation
library(tidyr)          # for data tidying
library(corrplot)
library(plyr)
library("corrgram")

#load the data
midwest <- read.csv("http://goo.gl/G1K41K")
summary(midwest)
names(midwest)

ggplot(midwest,aes(x=area,y=poptotal))

# --- Scatter Plot ----

# we mention the type of plot
# we mention the x & y axes in aes()
# The below gives scatter plot for total population by area
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()
g

#1st: Dataset
#2nd: aes() <- variables
#3rd: chart type
#4th: options for your chart

# Lets plot the regression line
g <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(shape=10,size=3,col="dark grey",fill="orange") + 
  geom_smooth(method="lm", linetype="dashed",col="red")  
plot(g)

# Plotting in with full syntax with labs()
dev.off()
gg <- g + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population Scatter Plot", 
       subtitle="Using midwest dataset", 
       y="Population", x="Area", 
       caption="Midwest Demographics")
plot(gg)

library(RColorBrewer)
head(brewer.pal.info,10)
tail(brewer.pal.info,10)
gg + scale_color_brewer(palette = "set1")
gg + scale_color_brewer(palette = "set2")

# Change axis Text
gg1 <- gg + scale_x_continuous(breaks = seq(0,0.1,0.01))
gg1
gg2 <- gg + scale_y_continuous(0,1000000,50000)
gg2

theme_set(theme_classic())
gg + theme_bw()

# ------------------ Histogram ---------------------
# The below gives the histogram showing the count of observations for each states
# We mention the type of plot i.e. geom_hist()
# We set the title of the plot along with setting the x and y labels

gh <- ggplot(midwest, aes(x= state)) + 
  geom_histogram(stat = "count", fill = I("#FF8A06")) + 
  labs(title = "States in the Midwest", x = "States", y = "Count")
plot(gh)

gh1 <- gh + geom_vline(aes(xintercept = mean(poptotal)))
gh1

# For plain background use theme_classic() along with the plot
gh + theme_classic()

# Use below code to introduce multiple lines in the plot background
gh + theme_linedraw()



# We can use geom_bar to plot the distribution as well like histogram
# The below gives category-wise visuals of the state
gh <- ggplot(aes(x=state, fill= category), data = midwest)
gh + geom_bar()


# The below gives a visual of what percent of people are college-educated
ggplot(midwest, aes(x=percollege)) +
  geom_histogram(aes(fill=state), position='dodge', binwidth=5) +
  labs(title='College attendance by state', x='Percent college')


# We can also create density plots using geom_density, using the fill option by state:
ggplot(midwest, aes(x=percollege)) +
  geom_density(aes(fill=state))

# Unfortunately, these density plots overlap each other, so we need to add transparency
ggplot(midwest, aes(x=percollege)) +
  geom_density(aes(fill=state), alpha=0.3)


# We could view the data as a horizontal bar chart as well
# Filter and Select the percollege data for the state of Ohio
ohio_top25 <- midwest %>%
  filter(state == "OH") %>%
  select(county, percollege) %>%
  arrange(desc(percollege)) %>%
  top_n(25) %>%
  arrange(percollege) %>%
  mutate(county = factor(county, levels = .$county))
plot(ohio_top25)

# The below give the horizontal bar graph to view the top 25 counties in Ohio for percentage of college educated folks
ggplot(ohio_top25, aes(county, percollege)) +
  geom_bar(stat = "identity") + 
  coord_flip() + labs(title="Horizontal Bar Graph")

# The below gives the dot plot
ggplot(ohio_top25, aes(percollege, county)) + geom_point() + labs(title="Dot Plot")

# We can further plot a lollipop chart which is a combination of the dot chart and horizontal bar chart
# lollipop chart
ggplot(ohio_top25, aes(percollege, county)) +
  geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point() + labs(title="Lollipop Chart")


# This can further be explored by taking a look at the top-10 counties of each midwest state 
# Additional manipulation is required as each state has couple county
top10 <- midwest %>%
  select(state, county, percollege) %>%
  group_by(state) %>%
  arrange(desc(percollege)) %>%
  top_n(10) %>%
  arrange(percollege) %>%
  unite(county_st, county, state, remove = FALSE) %>%
  mutate(county_st = factor(county_st, levels = .$county_st))
plot(top10)

# The below give top-10 counties for each state
ggplot(top10, aes(percollege, county_st)) +
  geom_segment(aes(x = 0, y = county_st, xend = percollege, yend = county_st), 
  color = "grey50") +
  geom_point() +
  scale_y_discrete(labels = abbreviate) +
  facet_wrap(~ state, scales = "free_y")



# The below gives BoxPlot that provides all outliers for education vs poverty in each state
p <- ggplot(data = midwest, aes(y = percbelowpoverty, x = percollege))
p + geom_boxplot(aes(color = state)) + facet_wrap(~state) + 
  ggtitle("College Education Vs Total Poverty by Each Midwest State") +
  xlab("Percent College Educated") + ylab("Percentage of Total poverty")


#---------------------------------------------------------------------------------------------------------------

names(midwest)

# --------- Bar Plot --------

g <- ggplot(data = midwest,aes(x=state,fill=state))+geom_bar(stat = "count")
g

g1 <- ggplot(data = midwest,aes(x=state,fill=state))+
  geom_bar(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))
g1


#---------- Line Plot --------------------
 L <- ggplot(data=midwest,aes(x=state,y=area, group=1)) +
  geom_line(linetype="dashed", color="red",size=1.5)
plot(L)
