#############################
# Toronto Bike Theft Report #
#############################

## Table 1: Dataset Observations, Data types, and Descriptions
Types = c(class(origin$X_id), class(origin$event_unique_id), class(origin$Primary_Offence), class(origin$Occurrence_Date), class(origin$Occurrence_Year), class(origin$Occurrence_Month), class(origin$Occurrence_DayOfWeek), class(origin$Occurrence_DayOfMonth), class(origin$Occurrence_DayOfYear), class(origin$Occurrence_Hour), class(origin$Report_Date), class(origin$Report_Year), class(origin$Report_Month), class(origin$Report_DayOfWeek), class(origin$Report_DayOfMonth), class(origin$Report_DayOfYear), class(origin$Report_Hour), class(origin$Division), class(origin$City), class(origin$Hood_ID), class(origin$NeighbourhoodName), class(origin$Location_Type), class(origin$Premises_Type), class(origin$Bike_Make), class(origin$Bike_Model), class(origin$Bike_Type), class(origin$Bike_Speed), class(origin$Bike_Colour), class(origin$Cost_of_Bike), class(origin$Status), class(origin$geometry))
Variable = c(colnames(origin))

Description = c("unique entry id order", "unique event id unsorted", "type of crime committed", "date of crime committed", "year of crime committed", "month of crime committed", "days of week crime (Mon-Sun)", "date of month crime (1-31)", "day of year crime (1-365)", "hour of day crime (1-24)", "incident reported to authorities", "year of incident reported", "month of incident reported", "day of week incident (Mon-Sun)", "day of month incident (1-31)", "day of year incident (1-365)", "hour of day incident", "division of location", "city name in Toronto", "hood id for neighborhoods (out of 158)", "neighborhood name in Toronto", "broad crime location", "description of bike theft location", "bike brand in theft", "bike model in theft", "type of bike stolen", "bike color in theft", "bike speed in theft", "bike cost in theft", "current bike status", "precise crime location")

# Combine vectors into a data frame
df <- data.frame(Variable = Variable, Types = Types, Description = Description)

# Convert data frame to a knitr table
knit_table <- knitr::kable(df, align = rep("l", ncol(df)))

# Print the knitr table
print(knit_table)

## Table 2: Total Number of Bike Thefts Each Year
#omit all the entries that has NA inputs, they won't be helpful to our data
data <- na.omit(origin)
summary <- table(data$Occurrence_Year)
print(summary)

## Table 3: Data After Filtering Incomplete Years
data = data %>% filter(Occurrence_Year >= 2014) %>%
  filter(Occurrence_Year < 2022)
summary_table = data %>% count(Occurrence_Year) %>% 
  rename(Year = Occurrence_Year, Theft_Count = n)
print(summary_table)

## Graph 1: Average Cost of Stolen Bikes by Occurrence Year
# Calculate average cost of bike by year
avg_cost_by_year <- data %>%
  group_by(Occurrence_Year) %>%
  summarize(avg_cost = mean(Cost_of_Bike))

# Create plot of average cost of bike by year
ggplot(avg_cost_by_year, aes(x = Occurrence_Year, y = avg_cost)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(x = "Occurrence Year", y = "Average Cost of Bike")

## Graph 2: Number of Thefts During each Hour
ggplot(data = data, aes(x = Occurrence_Hour)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(x = "Occurrence Hour", y = "Number of Thefts") 

## Graph 3: Bike Thefts Count in 2019-2021 by day of year

data2 = data %>% filter(Occurrence_Year %in% c(2019,2020,2021))

ggplot(data2, aes( x=Report_DayOfYear)) +
  geom_histogram(aes(fill=factor(Occurrence_Year)),alpha=0.5)+
  labs(fill="Occurrence_Year") + 
  labs(x = "Day of Year", y = "Counts")

## Table 4: Bike Make and Colour of stolen bikes in Toronto 2021
data3 = data %>%
  filter(Occurrence_Year == c(2021)) %>%
  group_by(Occurrence_Year, Bike_Make, Bike_Colour) %>%
  summarise(total = sum(n=n())) %>%
  arrange(desc(total))
as.data.frame(data3) %>% head(10)

## Figure 1: Neighborhood Group Counts
new_table <- table(data$NeighbourhoodName)
# create data frame with neighborhood names and counts
new_df <- data.frame(nh = names(new_table), count = as.vector(new_table), hood_id = unique(data$Hood_ID[match(names(new_table), data$NeighbourhoodName)]))

new_df$hood_id <- as.numeric(new_df$hood_id)

new_df <- new_df %>% mutate(nh_groups = case_when(new_df$hood_id >= 112 ~ 'East',
                                                  (new_df$hood_id<112 & new_df$hood_id>=84) ~ 'Central',
                                                  (new_df$hood_id<84 & new_df$hood_id>=56)~'Downtown',
                                                  (new_df$hood_id<56 & new_df$hood_id>=28)~'North',
                                                  TRUE~'West'))

grouped_data <- new_df %>% 
  group_by(nh_groups) %>% 
  summarize(count = sum(count)) %>%
  mutate(percentage = percent(count / sum(count)))

# create a pie chart
ggplot(grouped_data, aes(x = "", y = count, fill = nh_groups)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Neighborhood Groups") +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = percent_format()) +
  theme_void() +
  theme(text = element_text(size = 12))

## Figure 2: Bike Stolen Locations in Toronto
# Extracting longitude and latitude using regular expressions
data <- data %>%
  mutate(longitude = as.numeric(sub(".*\\((-?\\d+\\.\\d+),.*", "\\1", geometry)),
         latitude = as.numeric(sub(".*,(\\s?-?\\d+\\.\\d+)\\)}", "\\1", geometry))) %>%
  filter(longitude != 0 & latitude != 0 &
         longitude > -79.7 & longitude < -79.1) %>%
  mutate(cost_lost = case_when(Cost_of_Bike>=600~"high",
                               (Cost_of_Bike<600 & Cost_of_Bike>=400)~"mid",
                               TRUE ~"low"))

img <- readPNG("toronto2.png")
# Plot the data
ggplot(data, aes(x = longitude, y = latitude, color = cost_lost)) +
  background_image(img) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("low" = "red", "mid" = "yellow", "high" = "green"),
                     name = "Cost of Bike") +
  ggtitle("Bike Stolen Locations in Toronto") +
  xlab("Longitude") +
  ylab("Latitude")

## Let's now take a sample of 1000 observations from the dataset and construct a 
## 97% confidence interval for the proportion of bikes stolen on a Monday. 
## Let's also construct a 97% Bootstrap proportion of bikes stolen on a Monday:
samp = data %>% sample_n(1000)

prop.test(x = sum(samp$Occurrence_DayOfWeek == "Monday"), n = length(samp$Occurrence_DayOfWeek), conf.level = 0.97)

boot_function = function(){
  
  boot_data = samp  %>% sample_n(nrow(samp), replace = T)
  
  boot_prop = mean(boot_data$Occurrence_DayOfWeek == "Monday")
  
  return(boot_prop)
  
}

quantile(replicate(1000, boot_function()), c(0.1, 0.9))

## Perform Linear Regression Model
#Linear Regression and Cross Validation part

# Split the data into training and test sets
set.seed(99)
trainIndex <- createDataPartition(summary_table$Theft_Count, p = .8, list = FALSE)
training <- summary_table[trainIndex,]
testing <- summary_table[-trainIndex,]

# Fit a linear regression model to the training data
lm_model <- lm(Theft_Count ~ Year, data = training)

# Print the summary of the model
summary(lm_model)

# Fit the linear regression model using 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)
lmFit <- train(Theft_Count ~ Year, data = training, method = "lm",
               trControl = ctrl)

# Predict values for the training and test sets
training$predicted <- predict(lm_model, newdata = training)
testing$predicted <- predict(lm_model, newdata = testing)

# Plot the results with the line of best fit
ggplot(training, aes(x = Year, y = Theft_Count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", alpha=0.1) +
  labs(title = "Bike Theft in Toronto",
       x = "Year of Bike Theft",
       y = "Number of Bikes Stolen") +
  geom_line(aes(x = Year, y = predicted), data = training, color = "blue") +
  geom_line(aes(x = Year, y = predicted), data = testing, color = "green")
