# Preparation 
library(tidyverse)
library(dplyr)
library(rpart)
library(tidyr)
library(openxlsx)
library(jsonlite)
library(mongolite)
library(base)
library(lubridate)
library(readxl)
library(stringr)
library(randomForest)
library(RSQLite)

# Import airbnb data 
airbnb<- read.xlsx("Airbnb finals.xlsx", na.strings=c("","NA"," "))
airbnb<- airbnb[,-1]

# Tidying all the columns using regular expressions
airbnb$Reviews<- str_replace_all(airbnb$Reviews,c("\\sreviews"="","\\sreview"=""))
airbnb$Ratings<- str_replace_all(airbnb$Ratings,c("\\sout of 5 stars$"="","Average\\s"=""))
airbnb$Bedrooms<- str_replace_all(airbnb$Bedrooms,c("\\s(B|b)edroom(s)?"=""))
airbnb$Beds<- str_replace_all(airbnb$Beds,c("\\s(B|b)ed(s)?"=""))
airbnb$Property.Type<- str_replace_all(airbnb$Property.Type,c("Property type:"="","Monthly Discount:"=""))
airbnb$Pricepernight<- as.numeric(str_replace_all(airbnb$Pricepernight,c("^\\$"="")))
airbnb$`Total.Price`<- as.numeric(str_replace_all(airbnb$Total.Price,c("R?[\\$€]"="")))
airbnb$Cleaning.fee<- (str_replace_all(airbnb$Cleaning.fee,c("^Cleaning fee"="","R?[\\$€]"="")))
airbnb$Service.Fee<- (str_replace_all(airbnb$Service.Fee,c("^Service fee"="","R?[\\$€]"="")))
airbnb$Tax<- (str_replace_all(airbnb$Tax,c("^Occupancy Taxes"="","R?[\\$€]"="")))

# convert columns to numeric format

airbnb$Reviews<- as.numeric(airbnb$Reviews)
airbnb$Beds<- as.numeric(airbnb$Beds)
airbnb$Accommodates<- as.numeric(airbnb$Accommodates)
airbnb$Cleaning.fee<- as.numeric(airbnb$Cleaning.fee)
airbnb$Service.Fee<- as.numeric(airbnb$Service.Fee)
airbnb$Tax<- as.numeric(airbnb$Tax)
airbnb$Bedrooms<- as.numeric(airbnb$Bedrooms)
airbnb$Bathrooms<- as.numeric(airbnb$Bathrooms)
airbnb$Host.Reviews<- as.numeric(airbnb$Host.Reviews)


# Assigning True, False values for ammenities
airbnb$Parking<- if_else(str_detect(airbnb$Amenities,"Free parking on premises"),TRUE,FALSE)
airbnb$Elevator<- if_else(str_detect(airbnb$Amenities,"Elevator"),TRUE,FALSE)
airbnb$Kitchen<- if_else(str_detect(airbnb$Amenities,"Kitchen"),TRUE,FALSE)
airbnb$Family_place<- if_else(str_detect(airbnb$Amenities,"Family/kid friendly"),TRUE,FALSE)
airbnb$Wi_Fi<- if_else(str_detect(airbnb$Amenities,c("Internet","Wireless Internet")),TRUE,FALSE)
airbnb$Hot_tub<- if_else(str_detect(airbnb$Amenities,c("Hot tub","Bathtub")),TRUE,FALSE)
airbnb$Hair_dryer<- if_else(str_detect(airbnb$Amenities,"Hair dryer"),TRUE,FALSE)
airbnb$Laptop<- if_else(str_detect(airbnb$Amenities,"Laptop friendly workspace"),TRUE,FALSE)
airbnb$Intercom<- if_else(str_detect(airbnb$Amenities,"Buzzer/wireless intercom"),TRUE,FALSE)
airbnb$Fireplace<- if_else(str_detect(airbnb$Amenities,"Indoor fireplace"),TRUE,FALSE)
airbnb$Breakfast<- if_else(str_detect(airbnb$Amenities,"Breakfast"),TRUE,FALSE)
airbnb$Smoking<- if_else(str_detect(airbnb$Amenities,"Smoking allowed"),TRUE,FALSE)
airbnb$Essentials<- if_else(str_detect(airbnb$Amenities,"Essentials"),TRUE,FALSE)
airbnb$Events<- if_else(str_detect(airbnb$Amenities,"Suitable for events"),TRUE,FALSE)
airbnb$TV<- if_else(str_detect(airbnb$Amenities,c("Cable TV","TV")),TRUE,FALSE)
airbnb$Doorman<- if_else(str_detect(airbnb$Amenities,"Doorman"),TRUE,FALSE)
airbnb$Iron<- if_else(str_detect(airbnb$Amenities,"Iron"),TRUE,FALSE)
airbnb$Wheelchair<- if_else(str_detect(airbnb$Amenities,"Wheelchair accessible"),TRUE,FALSE)
airbnb$Hangers<- if_else(str_detect(airbnb$Amenities,"Hangers"),TRUE,FALSE)
airbnb$Heating<- if_else(str_detect(airbnb$Amenities,"Heating"),TRUE,FALSE)
airbnb$Dryer<- if_else(str_detect(airbnb$Amenities,"Dryer"),TRUE,FALSE)
airbnb$Air_conditioning<- if_else(str_detect(airbnb$Amenities,"Air conditioning"),TRUE,FALSE)
airbnb$Shampoo<- if_else(str_detect(airbnb$Amenities,"Shampoo"),TRUE,FALSE)
airbnb$Gym<- if_else(str_detect(airbnb$Amenities,"Gym"),TRUE,FALSE)
airbnb$Pool<- if_else(str_detect(airbnb$Amenities,"Pool"),TRUE,FALSE)
airbnb$Washer<- if_else(str_detect(airbnb$Amenities,"Washer"),TRUE,FALSE)
airbnb$Pvt_entrance<- if_else(str_detect(airbnb$Amenities,"Private entrance"),TRUE,FALSE)
airbnb$baby_monitor<- if_else(str_detect(airbnb$Amenities,"Baby monitor"),TRUE,FALSE)
airbnb$Pvt_livingroom<- if_else(str_detect(airbnb$Amenities,"Private living room"),TRUE,FALSE)
airbnb$Room_shades<- if_else(str_detect(airbnb$Amenities,"Room-darkening shades"),TRUE,FALSE)
airbnb$Travel_crib<- if_else(str_detect(airbnb$Amenities,"Pack ’n Play/travel crib"),TRUE,FALSE)
airbnb$Babysitter<- if_else(str_detect(airbnb$Amenities,"Babysitter recommendations"),TRUE,FALSE)
airbnb$Toys<- if_else(str_detect(airbnb$Amenities,"Children’s books and toys"),TRUE,FALSE)
airbnb$High_chair<- if_else(str_detect(airbnb$Amenities,"High chair"),TRUE,FALSE)
airbnb$Fireplace_guards<- if_else(str_detect(airbnb$Amenities,"Fireplace guards"),TRUE,FALSE)
airbnb$Baby_bath<- if_else(str_detect(airbnb$Amenities,"Baby bath"),TRUE,FALSE)
airbnb$Outlet_covers<- if_else(str_detect(airbnb$Amenities,"Outlet covers"),TRUE,FALSE)
airbnb$Game_console<- if_else(str_detect(airbnb$Amenities,"Game console"),TRUE,FALSE)
airbnb$Window_guards<- if_else(str_detect(airbnb$Amenities,"Window guards"),TRUE,FALSE)
airbnb$Crib<- if_else(str_detect(airbnb$Amenities,"Crib"),TRUE,FALSE)
airbnb$Child_dinnerware<- if_else(str_detect(airbnb$Amenities,"Children's dinnerware"),TRUE,FALSE)


airbnb$Index<- 1:nrow(airbnb)

###Storing in SQL

database<-dbConnect(SQLite(),dbname="airbnb_data.sqlite")

location_data<- airbnb%>% 
  select(Name, Location)
location_data$ID<- 1:nrow(location_data)
Property_data<- airbnb%>%
  select(Name, Bedrooms, Bathrooms, Accommodates,Beds, Property.Type,Room.Type,Cancellation,Safety.features,
         Cleaning.fee, Service.Fee, Tax,House.rules)
Property_data$ID<- 1:nrow(Property_data)
Userfeedback<- airbnb%>%
  select(Name, Ratings, Reviews)
Userfeedback$ID<- 1:nrow(Userfeedback)
Hostdata<- airbnb%>%
  select(Name, Host.Response.rate,Host.Reviews,Host.Response.time)
Hostdata$ID<- 1:nrow(Hostdata)
priceinfo<- airbnb%>%
  select(Name, Pricepernight,Total.Price,Price.Currency)
priceinfo$ID<- 1:nrow(priceinfo)
amenitiesinfo<- airbnb%>%
  select(Name, c(17,24:64))
amenitiesinfo$ID<- 1:nrow(amenitiesinfo)

# Writing tables and storing values in SQL
dbWriteTable(conn=database, name="Locations", value=location_data,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database, name="Property", value=Property_data,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database, name="User_feedback", value=Userfeedback,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database, name="Prices", value=priceinfo,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database, name="Amenities", value=amenitiesinfo,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database, name="Host", value=Hostdata,row.names = FALSE, header = TRUE,overwrite=TRUE)


# dbDisconnect(database)
# 
# dbListTables(database)
# dbListFields(database, "User feedback")
# dbClearResult(database)
# dbReadTable(database, "Amenities")

#DATA ANALYSIS
#get data from sql
a<- dbGetQuery(database, "SELECT User_feedback.Name, Ratings, Prices.Pricepernight,Prices.`Total.Price` FROM User_feedback
               INNER JOIN Prices 
               ON User_feedback.ID = Prices.ID")

b<- dbGetQuery(database, "SELECT * FROM Amenities")
c<- dbGetQuery(database, "SELECT * FROM Locations
               INNER JOIN Property ON Property.ID = Locations.ID
               INNER JOIN Prices ON Prices.ID=Locations.ID
               INNER JOIN Amenities ON Amenities.ID=Locations.ID
               INNER JOIN User_feedback ON User_feedback.ID=Locations.ID
               INNER JOIN Host ON Host.ID=Locations.ID")
c<- c[,-c(1:4,9,10,11,12,16:18,20,21:24,66,67,70:72,74,75)]
d<- dbGetQuery(database, "SELECT * FROM Amenities
               INNER JOIN User_feedback ON User_feedback.ID=Amenities.ID
               WHERE Ratings=5 AND Reviews >100")
d<- d[,-c(44,48)]
e<- dbGetQuery(database, "SELECT * FROM Locations
               INNER JOIN Property ON Property.ID = Locations.ID
               INNER JOIN Prices ON Prices.ID=Locations.ID
               INNER JOIN Amenities ON Amenities.ID=Locations.ID
               INNER JOIN User_feedback ON User_feedback.ID=Locations.ID
               INNER JOIN Host ON Host.ID=Locations.ID
               WHERE Ratings=5 AND Reviews >100")
e<- e[,c(5:10,19:21,24,68,69,72,73)]
# creating box plot
ggplot(data = a) + 
  geom_boxplot(mapping = aes(x = Ratings, y = Pricepernight),na.rm=TRUE)+labs(y="Price/night" )+ggtitle("AIRBNB")
ggplot(data = a) + 
  geom_boxplot(mapping = aes(x = Ratings, y = `Total.Price`),na.rm=TRUE)+labs(y="Total Price")+ggtitle("AIRBNB")
# creating histogram 
ggplot(data = e) + 
  geom_histogram(aes(Pricepernight, fill=Pricepernight),binwidth=20,na.rm=TRUE, alpha=0.6)+labs(x="Price/night" )+ggtitle("AIRBNB")
ggplot(data = a) + 
  geom_histogram(aes(Pricepernight, fill=Pricepernight),binwidth=20,na.rm=TRUE, alpha=0.6)+labs(x="Price/night" )+ggtitle("AIRBNB")
ggplot(data = a) + 
  geom_histogram(aes(`Total.Price`, fill=`Total.Price`),binwidth=20,na.rm=TRUE, alpha=0.6)+labs(x="Total Price")+ggtitle("AIRBNB")

## Analysis on Amenities in set d
d %>%
  select(3:43) %>%
  summarise(
    Parking = sum(Parking, na.rm=T),
    Elevator = sum(Elevator, na.rm=T),
    TV = sum(TV, na.rm=T),
    wifi = sum(Wi_Fi, na.rm=T),
    Kitchen = sum(Kitchen, na.rm=T),
    Family = sum(Family_place, na.rm=T),
    Hot_tub = sum(Hot_tub, na.rm=T),
    Hair_dryer = sum(Hair_dryer, na.rm=T),
    Laptop = sum(Laptop, na.rm=T),
    Intercom = sum(Intercom, na.rm=T),
    Fireplace = sum(Fireplace, na.rm=T),
    Breakfast = sum(Breakfast, na.rm=T),
    Smoking = sum(Smoking, na.rm=T),
    Essentials = sum(Essentials, na.rm=T),
    Events = sum(Events, na.rm=T),
    Doorman = sum(Doorman, na.rm=T),
    Iron = sum(Iron, na.rm=T),
    Wheelchair = sum(Wheelchair, na.rm=T),
    Hangers = sum(Hangers, na.rm=T),
    Heating = sum(Heating, na.rm=T),
    Dryer = sum(Dryer, na.rm=T),
    AC = sum(Air_conditioning, na.rm=T),
    Shampoo = sum(Shampoo, na.rm=T),
    Gym = sum(Gym, na.rm=T),
    Pool = sum(Pool, na.rm=T),
    Washer = sum(Washer, na.rm=T),
    Pvt_entrance = sum(Pvt_entrance, na.rm=T),
    Baby_bath = sum(Baby_bath, na.rm=T),
    Babysitter = sum(Babysitter, na.rm=T),
    baby_monitor = sum(baby_monitor, na.rm=T),
    Babysitter = sum(Babysitter, na.rm=T),
    Livingroom = sum(Pvt_livingroom, na.rm=T),
    Room_shades = sum(Room_shades, na.rm=T),
    Travel_crib = sum(Travel_crib, na.rm=T),
    Toys = sum(Toys, na.rm=T),
    High_chair = sum(High_chair, na.rm=T),
    Fireplace_guards = sum(Fireplace_guards, na.rm=T),
    Outlet_covers = sum(Outlet_covers, na.rm=T),
    Game_console = sum(Game_console, na.rm=T),
    Window_guards = sum(Window_guards, na.rm=T),
    Crib = sum(Crib, na.rm=T),
    Child_dinnerware = sum(Child_dinnerware, na.rm=T)
     ) %>% t() %>% as.data.frame() %>%
  
  mutate(amenity = factor(row.names(.), levels = row.names(.),
                            ordered=TRUE)) %>%
  select(amenity, count=V1) %>%
  # create bar plot
  ggplot(aes(x=reorder(amenity, -count),y=count)) +
  geom_bar(stat = 'identity')+coord_flip()+labs(x="Amenities")+ggtitle("AIRBNB")

## Analysis on Amenities in entire dataset
b %>%
  select(3:43) %>%
  summarise(
    Parking = sum(Parking, na.rm=T),
    Elevator = sum(Elevator, na.rm=T),
    TV = sum(TV, na.rm=T),
    wifi = sum(Wi_Fi, na.rm=T),
    Kitchen = sum(Kitchen, na.rm=T),
    Family = sum(Family_place, na.rm=T),
    Hot_tub = sum(Hot_tub, na.rm=T),
    Hair_dryer = sum(Hair_dryer, na.rm=T),
    Laptop = sum(Laptop, na.rm=T),
    Intercom = sum(Intercom, na.rm=T),
    Fireplace = sum(Fireplace, na.rm=T),
    Breakfast = sum(Breakfast, na.rm=T),
    Smoking = sum(Smoking, na.rm=T),
    Essentials = sum(Essentials, na.rm=T),
    Events = sum(Events, na.rm=T),
    Doorman = sum(Doorman, na.rm=T),
    Iron = sum(Iron, na.rm=T),
    Wheelchair = sum(Wheelchair, na.rm=T),
    Hangers = sum(Hangers, na.rm=T),
    Heating = sum(Heating, na.rm=T),
    Dryer = sum(Dryer, na.rm=T),
    AC = sum(Air_conditioning, na.rm=T),
    Shampoo = sum(Shampoo, na.rm=T),
    Gym = sum(Gym, na.rm=T),
    Pool = sum(Pool, na.rm=T),
    Washer = sum(Washer, na.rm=T),
    Pvt_entrance = sum(Pvt_entrance, na.rm=T),
    Baby_bath = sum(Baby_bath, na.rm=T),
    Babysitter = sum(Babysitter, na.rm=T),
    baby_monitor = sum(baby_monitor, na.rm=T),
    Babysitter = sum(Babysitter, na.rm=T),
    Livingroom = sum(Pvt_livingroom, na.rm=T),
    Room_shades = sum(Room_shades, na.rm=T),
    Travel_crib = sum(Travel_crib, na.rm=T),
    Toys = sum(Toys, na.rm=T),
    High_chair = sum(High_chair, na.rm=T),
    Fireplace_guards = sum(Fireplace_guards, na.rm=T),
    Outlet_covers = sum(Outlet_covers, na.rm=T),
    Game_console = sum(Game_console, na.rm=T),
    Window_guards = sum(Window_guards, na.rm=T),
    Crib = sum(Crib, na.rm=T),
    Child_dinnerware = sum(Child_dinnerware, na.rm=T)
  ) %>% t() %>% as.data.frame() %>%
  
  mutate(amenity = factor(row.names(.), levels = row.names(.),
                          ordered=TRUE)) %>%
  select(amenity, count=V1) %>%
  # create bar plot
  ggplot(aes(x=reorder(amenity, -count),y=count)) +
  geom_bar(stat = 'identity')+coord_flip()+labs(x="Amenities")+ggtitle("AIRBNB")


##Random Forest Model 
# summary(c)
# which(is.na(c$Cleaning.fee))
# which(is.na(c$Total.Price))
# which(is.na(c$Host.Reviews))
# which(is.na(c$Tax))

c$Ratings<- as.numeric(c$Ratings)

summary(c)
Ratingfit<- rpart(Ratings~ .,
                  data=c[!is.na(c$Ratings),],
                  method="anova")
c$Ratings[is.na(c$Ratings)] <- predict(Ratingfit, c[is.na(c$Ratings),])


set.seed(415)
`Factors affecting Price` <- randomForest(Pricepernight ~ .,
                    data=c, 
                    importance=TRUE, proximity=TRUE,
                    ntree=200)
varImpPlot(`Factors affecting Price`)


# Import tripadvisor data
tripadv<- read.xlsx("Tripadvisor final.xlsx", na.strings=c("","NA"," "))
tripadv<- tripadv[,c(-1,-4)]

# Tidying all the columns 
tripadv$Bookings<- as.numeric(tripadv$Bookings)
tripadv$Reviews<- str_replace_all(tripadv$Reviews,c("\\sreviews"="","\\sreview"=""))
tripadv$Ratings<- str_replace_all(tripadv$Ratings,c("of 5 bubbles$"=""))
tripadv$House.rules<- str_replace_all(tripadv$House.rules,c("; House rules[(\\s)+(\\w)+([-<>])+]+"="","^House rules"=""))
tripadv$Fees<- str_replace_all(tripadv$Fees,c(" \\/ stay - Fee"=""))
tripadv$Property.type<- str_replace_all(tripadv$Property.type,c(" -"=""))
tripadv$Price<- as.numeric(str_replace_all(tripadv$Price,c("^\\$"="")))

# Separate columns of city, zipcode and all the amenities
tripadv<- tripadv %>% separate(Location.Info, into=c("City","Zipcode"), sep="\\,")
tripadv$Zipcode<- str_replace_all(tripadv$Zipcode,c("District of Columbia "=""))
tripadv<- tripadv %>% separate(Amenities, into=c("Amen1","Amen2","Amen3","Amen4","Amen5","Amen6","Amen7","Amen8"),sep="[,;]") 

# Cleaning the amenities columns 
tripadv$Amen1<- str_replace_all(tripadv$Amen1, "^\\s+","")
tripadv$Amen2<- str_replace_all(tripadv$Amen2, "^\\s+","")
tripadv$Amen3<- str_replace_all(tripadv$Amen3, "^\\s+","")
tripadv$Amen4<- str_replace_all(tripadv$Amen4, "^\\s+","")
tripadv$Amen5<- str_replace_all(tripadv$Amen5, "^\\s+","")
tripadv$Amen6<- str_replace_all(tripadv$Amen6, "^\\s+","")
tripadv$Amen7<- str_replace_all(tripadv$Amen7, "^\\s+","")
tripadv$Amen8<- str_replace_all(tripadv$Amen8, "^\\s+","")

# Assign true, false for all the amenities columns
tripadv<- tripadv%>%
  mutate(
    Airconditioning= if_else(tripadv$Amen1=="Air Conditioning"| tripadv$Amen2=="Air Conditioning"| tripadv$Amen3=="Air Conditioning" |
                               tripadv$Amen4=="Air Conditioning"|tripadv$Amen5=="Air Conditioning"|tripadv$Amen6=="Air Conditioning"|
                               tripadv$Amen7=="Air Conditioning"|tripadv$Amen8=="Air Conditioning", TRUE, FALSE),
    Washer= if_else(tripadv$Amen1=="Washer"| tripadv$Amen2=="Washer"| tripadv$Amen3=="Washer" |
                      tripadv$Amen4=="Washer"|tripadv$Amen5=="Washer"|tripadv$Amen6=="Washer"|
                      tripadv$Amen7=="Washer"|tripadv$Amen8=="Washer", TRUE, FALSE),
    Wi_Fi= if_else(tripadv$Amen1=="Wi-Fi"| tripadv$Amen2=="Wi-Fi"| tripadv$Amen3=="Wi-Fi" |
                     tripadv$Amen4=="Wi-Fi"|tripadv$Amen5=="Wi-Fi"|tripadv$Amen6=="Wi-Fi"|
                     tripadv$Amen7=="Wi-Fi"|tripadv$Amen8=="Wi-Fi", TRUE, FALSE),
    TV= if_else(tripadv$Amen1=="Satellite TV"| tripadv$Amen2=="Satellite TV"| tripadv$Amen3=="Satellite TV" |
                  tripadv$Amen4=="Satellite TV"|tripadv$Amen5=="Satellite TV"|tripadv$Amen6=="Satellite TV"|
                  tripadv$Amen7=="Satellite TV"|tripadv$Amen8=="Satellite TV", TRUE,FALSE),
    Parking= if_else(tripadv$Amen1=="Parking"| tripadv$Amen2=="Parking"| tripadv$Amen3=="Parking" |
                       tripadv$Amen4=="Parking"|tripadv$Amen5=="Parking"|tripadv$Amen6=="Parking"|
                       tripadv$Amen7=="Parking"|tripadv$Amen8=="Parking", TRUE,FALSE),
    Balcony= if_else(tripadv$Amen1=="Balcony"| tripadv$Amen2=="Balcony"| tripadv$Amen3=="Balcony" |
                       tripadv$Amen4=="Balcony"|tripadv$Amen5=="Balcony"|tripadv$Amen6=="Balcony"|
                       tripadv$Amen7=="Balcony"|tripadv$Amen8=="Balcony", TRUE,FALSE),
    DVD_player= if_else(tripadv$Amen1=="DVD Player"| tripadv$Amen2=="DVD Player"| tripadv$Amen3=="DVD Player" |
                          tripadv$Amen4=="DVD Player"|tripadv$Amen5=="DVD Player"|tripadv$Amen6=="DVD Player"|
                          tripadv$Amen7=="DVD Player"|tripadv$Amen8=="DVD Player", TRUE,FALSE),
    Patio= if_else(tripadv$Amen1=="Patio"| tripadv$Amen2=="Patio"| tripadv$Amen3=="Patio" |
                     tripadv$Amen4=="Patio"|tripadv$Amen5=="Patio"|tripadv$Amen6=="Patio"|
                     tripadv$Amen7=="Patio"|tripadv$Amen8=="Patio", TRUE,FALSE),
    Shared_indoor_pool= if_else(tripadv$Amen1=="Shared Indoor Pool"| tripadv$Amen2=="Shared Indoor Pool"| tripadv$Amen3=="Shared Indoor Pool" |
                                  tripadv$Amen4=="Shared Indoor Pool"|tripadv$Amen5=="Shared Indoor Pool"|tripadv$Amen6=="Shared Indoor Pool"|
                                  tripadv$Amen7=="Shared Indoor Pool"|tripadv$Amen8=="Shared Indoor Pool", TRUE,FALSE),
    Private_yard= if_else(tripadv$Amen1=="Private Yard"| tripadv$Amen2=="Private Yard"| tripadv$Amen3=="Private Yard" |
                            tripadv$Amen4=="Private Yard"|tripadv$Amen5=="Private Yard"|tripadv$Amen6=="Private Yard"|
                            tripadv$Amen7=="Private Yard"|tripadv$Amen8=="Private Yard", TRUE,FALSE),
    Shared_yard= if_else(tripadv$Amen1=="Shared Yard"| tripadv$Amen2=="Shared Yard"| tripadv$Amen3=="Shared Yard" |
                           tripadv$Amen4=="Shared Yard"|tripadv$Amen5=="Shared Yard"|tripadv$Amen6=="Shared Yard"|
                           tripadv$Amen7=="Shared Yard"|tripadv$Amen8=="Shared Yard", TRUE,FALSE),
    Grill= if_else(tripadv$Amen1=="Grill"| tripadv$Amen2=="Grill"| tripadv$Amen3=="Grill" |
                     tripadv$Amen4=="Grill"|tripadv$Amen5=="Grill"|tripadv$Amen6=="Grill"|
                     tripadv$Amen7=="Grill"|tripadv$Amen8=="Grill", TRUE,FALSE),
    Outdoor_dining= if_else(tripadv$Amen1=="Outdoor Dining Area"| tripadv$Amen2=="Outdoor Dining Area"| tripadv$Amen3=="Outdoor Dining Area" |
                              tripadv$Amen4=="Outdoor Dining Area"|tripadv$Amen5=="Outdoor Dining Area"|tripadv$Amen6=="Outdoor Dining Area"|
                              tripadv$Amen7=="Outdoor Dining Area"|tripadv$Amen8=="Outdoor Dining Area", TRUE,FALSE),
    Porch= if_else(tripadv$Amen1=="Porch"| tripadv$Amen2=="Porch"| tripadv$Amen3=="Porch" |
                     tripadv$Amen4=="Porch"|tripadv$Amen5=="Porch"|tripadv$Amen6=="Porch"|
                     tripadv$Amen7=="Porch"|tripadv$Amen8=="Porch", TRUE,FALSE),
    Shared_outdoor_pool= if_else(tripadv$Amen1=="Shared Outdoor Pool(Unheated)"| tripadv$Amen2=="Shared Outdoor Pool(Unheated)"| tripadv$Amen3=="Shared Outdoor Pool(Unheated)" |
                                   tripadv$Amen4=="Shared Outdoor Pool(Unheated)"|tripadv$Amen5=="Shared Outdoor Pool(Unheated)"|tripadv$Amen6=="Shared Outdoor Pool(Unheated)"|
                                   tripadv$Amen7=="Shared Outdoor Pool(Unheated)"|tripadv$Amen8=="Shared Outdoor Pool(Unheated)", TRUE,FALSE),
    Central_heating= if_else(tripadv$Amen1=="Central Heating"| tripadv$Amen2=="Central Heating"| tripadv$Amen3=="Central Heating" |
                               tripadv$Amen4=="Central Heating"|tripadv$Amen5=="Central Heating"|tripadv$Amen6=="Central Heating"|
                               tripadv$Amen7=="Central Heating"|tripadv$Amen8=="Central Heating", TRUE,FALSE),
    Internet= if_else(tripadv$Amen1=="Internet Access"| tripadv$Amen2=="Internet Access"| tripadv$Amen3=="Internet Access" |
                        tripadv$Amen4=="Internet Access"|tripadv$Amen5=="Internet Access"|tripadv$Amen6=="Internet Access"|
                        tripadv$Amen7=="Internet Access"|tripadv$Amen8=="Internet Access", TRUE,FALSE),
    Hot_tub= if_else(tripadv$Amen1=="Hot Tub"| tripadv$Amen2=="Hot Tub"| tripadv$Amen3=="Hot Tub" |
                       tripadv$Amen4=="Hot Tub"|tripadv$Amen5=="Hot Tub"|tripadv$Amen6=="Hot Tub"|
                       tripadv$Amen7=="Hot Tub"|tripadv$Amen8=="Hot Tub", TRUE,FALSE),
    Terrace= if_else(tripadv$Amen1=="Terrace"| tripadv$Amen2=="Terrace"| tripadv$Amen3=="Terrace" |
                       tripadv$Amen4=="Terrace"|tripadv$Amen5=="Terrace"|tripadv$Amen6=="Terrace"|
                       tripadv$Amen7=="Terrace"|tripadv$Amen8=="Terrace", TRUE,FALSE),
    Pvt_indoor_pool= if_else(tripadv$Amen1=="Private Indoor Pool"| tripadv$Amen2=="Private Indoor Pool"| tripadv$Amen3=="Private Indoor Pool" |
                               tripadv$Amen4=="Private Indoor Pool"|tripadv$Amen5=="Private Indoor Pool"|tripadv$Amen6=="Private Indoor Pool"|
                               tripadv$Amen7=="Private Indoor Pool"|tripadv$Amen8=="Private Indoor Pool", TRUE,FALSE),
    Secure_Parking= if_else(tripadv$Amen1=="Secure Parking"| tripadv$Amen2=="Secure Parking"| tripadv$Amen3=="Secure Parking" |
                              tripadv$Amen4=="Secure Parking"|tripadv$Amen5=="Secure Parking"|tripadv$Amen6=="Secure Parking"|
                              tripadv$Amen7=="Secure Parking"|tripadv$Amen8=="Secure Parking", TRUE,FALSE),
    Sauna= if_else(tripadv$Amen1=="Sauna"| tripadv$Amen2=="Sauna"| tripadv$Amen3=="Sauna" |
                     tripadv$Amen4=="Sauna"|tripadv$Amen5=="Sauna"|tripadv$Amen6=="Sauna"|
                     tripadv$Amen7=="Sauna"|tripadv$Amen8=="Sauna", TRUE,FALSE)
  )
# Assign false to all the n/a values in amenities columns 
tripadv[ , 24:45][is.na(tripadv[, 24:45])] <- FALSE
tripadvisor<- tripadv[,-c(10:17)]
tripadvisor$Index<- 1:nrow(tripadvisor)

# Convert to numeric 
tripadvisor$Reviews<- as.numeric(tripadvisor$Reviews)
tripadvisor$Bedrooms<- as.numeric(tripadvisor$Bedrooms)
tripadvisor$Bathrooms<- as.numeric(tripadvisor$Bathrooms)
tripadvisor$Sleeps<- as.numeric(tripadvisor$Sleeps)

###Storing in SQL

database1<-dbConnect(SQLite(),dbname="Tripadvisor_data.sqlite")

location_data1<- tripadvisor%>% 
  select(Name, Property.Location, City, Zipcode)
location_data1$ID<- 1:nrow(location_data1)
Property_data1<- tripadvisor%>%
  select(Name, Bedrooms, Bathrooms, Sleeps, Property.type,House.rules,Fees)
Property_data1$ID<- 1:nrow(Property_data1)
Userfeedback1<- tripadvisor%>%
  select(Name, Ratings, Reviews, Rating.Description)
Userfeedback1$ID<- 1:nrow(Userfeedback1)
priceinfo1<- tripadvisor%>%
  select(Name, Price, Bookings)
priceinfo1$ID<- 1:nrow(priceinfo1)
amenitiesinfo1<- tripadvisor%>%
  select(Name, 16:37)
amenitiesinfo1$ID<- 1:nrow(amenitiesinfo1)

# Writing tables and storing values 
dbWriteTable(conn=database1, name="Locations", value=location_data1,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database1, name="Property", value=Property_data1,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database1, name="User_feedback", value=Userfeedback1,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database1, name="Prices", value=priceinfo1,row.names = FALSE, header = TRUE,overwrite=TRUE)
dbWriteTable(conn=database1, name="Amenities", value=amenitiesinfo1,row.names = FALSE, header = TRUE,overwrite=TRUE)


# dbDisconnect(database1)
# 
# dbListTables(database1)
# dbListFields(database1, "User feedback")
# dbClearResult(database1)
# dbReadTable(database1, "Amenities")

#DATA ANALYSIS
a1<- dbGetQuery(database1, "SELECT User_feedback.Name, Ratings, Prices.Price, Prices.Bookings FROM User_feedback
               INNER JOIN Prices 
               ON User_feedback.ID = Prices.ID")

b1<- dbGetQuery(database1, "SELECT * FROM Amenities")
c1<- dbGetQuery(database1, "SELECT * FROM Locations
               INNER JOIN Property ON Property.ID = Locations.ID
               INNER JOIN Prices ON Prices.ID=Locations.ID
               INNER JOIN Amenities ON Amenities.ID=Locations.ID
               INNER JOIN User_feedback ON User_feedback.ID=Locations.ID")
c1<- c1[,-c(1:6,10:14,17,18,41,42,45,46)]
d1<-dbGetQuery(database1, "SELECT * FROM Locations
              INNER JOIN Property ON Property.ID = Locations.ID
              INNER JOIN Prices ON Prices.ID=Locations.ID
              INNER JOIN Amenities ON Amenities.ID=Locations.ID
              INNER JOIN User_feedback ON User_feedback.ID=Locations.ID
              WHERE Ratings>4 AND Reviews >20")
e1<- as.data.frame(dbGetQuery(database1, "SELECT * FROM Amenities
                             INNER JOIN User_feedback 
                             ON User_feedback.ID = Amenities.ID
                             WHERE Ratings>4 AND Reviews >20"))

v1<- d1[,c(2:4,7:10,15,16,43:45)]

summary(a1)
# create box plot
ggplot(data = a1, title="TRIPADVISOR") + 
  geom_boxplot(mapping = aes(x = Ratings, y = Price),na.rm=TRUE)+ggtitle("TRIPADVISOR")
# create box plot 
ggplot(data = a1) + 
  geom_boxplot(mapping = aes(x = Ratings, y = Bookings),na.rm=TRUE)+ggtitle("TRIPADVISOR")

# create histogram 
ggplot(data = a1) + 
  geom_histogram(aes(Price, fill=Price),binwidth=20,na.rm=TRUE, alpha=1/2)+ggtitle("TRIPADVISOR")
ggplot(data = d1) + 
  geom_histogram(aes(Price, fill=Price),binwidth=20,na.rm=TRUE, alpha=1/2)+ggtitle("TRIPADVISOR")


e1<- e1[,-c(24,29)]
# Amenities provided by query result of set e
 
e1 %>%
  select(2:23) %>%
  summarise(
    Airconditioning = sum(Airconditioning, na.rm=T),
    Washer = sum(Washer, na.rm=T),
    TV = sum(TV, na.rm=T),
    wifi = sum(Wi_Fi, na.rm=T),
    Parking = sum(Parking, na.rm=T),
    Balcony = sum(Balcony, na.rm=T),
    `DVD Player` = sum(`DVD_player`, na.rm=T),
    Patio = sum(Patio, na.rm=T),
    `Indoor Pool` = sum(`Shared_indoor_pool`, na.rm=T),
    `Outdoor Pool` = sum(`Shared_outdoor_pool`, na.rm=T),
    `Shrd Yard` = sum(`Shared_yard`, na.rm=T),
    `Pvt Yard` = sum(`Private_yard`, na.rm=T),
    Grill = sum(Grill, na.rm=T),
    Porch = sum(Porch, na.rm=T),
    `Dining Area` = sum(Outdoor_dining, na.rm=T),
    `Central Heating` = sum(Central_heating, na.rm=T),
    `Internet` = sum(Internet, na.rm=T),
    `Hot Tub` = sum(Hot_tub, na.rm=T),
    Terrace = sum(Terrace, na.rm=T),
    `Pvt Indoor Pool` = sum(Pvt_indoor_pool, na.rm=T),
    Sauna = sum(Sauna, na.rm=T)
  ) %>% t()%>% as.data.frame()%>%
  
  mutate(amenity = factor(row.names(.), levels = row.names(.),
                          ordered=TRUE))%>%
  select(amenity, count=V1) %>%
  ggplot(aes(x=reorder(amenity, -count),y=count)) +
  geom_bar(stat = 'identity')+coord_flip()+labs(x="Amenities")+ggtitle("TRIPADVISOR")

# Perform analysis on all the amenities
b1 %>%
  select(2:23) %>%
  summarise(
    Airconditioning = sum(Airconditioning, na.rm=T),
    Washer = sum(Washer, na.rm=T),
    TV = sum(TV, na.rm=T),
    wifi = sum(Wi_Fi, na.rm=T),
    Parking = sum(Parking, na.rm=T),
    Balcony = sum(Balcony, na.rm=T),
    `DVD Player` = sum(`DVD_player`, na.rm=T),
    Patio = sum(Patio, na.rm=T),
    `Indoor Pool` = sum(`Shared_indoor_pool`, na.rm=T),
    `Outdoor Pool` = sum(`Shared_outdoor_pool`, na.rm=T),
    `Shrd Yard` = sum(`Shared_yard`, na.rm=T),
    `Pvt Yard` = sum(`Private_yard`, na.rm=T),
    Grill = sum(Grill, na.rm=T),
    Porch = sum(Porch, na.rm=T),
    `Dining Area` = sum(Outdoor_dining, na.rm=T),
    `Central Heating` = sum(Central_heating, na.rm=T),
    `Internet` = sum(Internet, na.rm=T),
    `Hot Tub` = sum(Hot_tub, na.rm=T),
    Terrace = sum(Terrace, na.rm=T),
    `Pvt Indoor Pool` = sum(Pvt_indoor_pool, na.rm=T),
    Sauna = sum(Sauna, na.rm=T)
  ) %>% t() %>% as.data.frame() %>%
  
  mutate(amenitiess = factor(row.names(.), levels = row.names(.),
                             ordered=TRUE))%>%
  select(amenitiess, count=V1) %>%
  ggplot(aes(x=reorder(amenitiess, -count),y=count)) +
  geom_bar(stat = 'identity')+coord_flip()+labs(x="Amenities")+ggtitle("TRIPADVISOR")



##Random Forest Model 


# which(is.na(c1))
# summary(c1)
tripadvisor$Ratings<- as.numeric(tripadvisor$Ratings)
Ratingfit<- rpart(Ratings~ .,
                  data=c1[!is.na(c1$Ratings),],
                  method="anova")
c1$Ratings[is.na(c1$Ratings)] <- predict(Ratingfit, c1[is.na(c1$Ratings),])
Bookingfit<-rpart(Bookings~ .,
                  data=c1[!is.na(c1$Bookings),],
                  method="anova")
c1$Bookings[is.na(c1$Bookings)] <- predict(Bookingfit, c1[is.na(c1$Bookings),])


set.seed(415)
`Factors affecting Price` <- randomForest(Price~ .,
                                          data=c1, 
                                          importance=TRUE, proximity=TRUE,
                                          ntree=200)
varImpPlot(`Factors affecting Price`)


