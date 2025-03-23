
############################################################################

# OBJECTIVE : Our final objective is to compare and cluster batsmen.
#             This script takes ball-by-ball data and aggregates it so that
#             we get all lifetime performance metrics of one batsman in one row     
# What kind of aggregations  might help ? Let's say -
# total runs, strike rate, avg runs per innings, sixes per match, 
# fours per match, dot balls per 100 runs, 
# not outs per 100 runs, % of runs hit as boundaries.. etc. etc . 
# you can think of many others , some might be more useful 
# Also, don't forget standard deviation across matches which shows consistency 


############################################################################


library(dplyr)
#library(ggplot2)

############################################################################
##  STEP 0 : Read Raw Data as provided 
############################################################################
# Read the delivery data 
delivdf<- read.csv("data/deliveries.csv")
sapply(delivdf,class)
# bad.. but we will fix it after removing  unnecessary columns 


# matchdf 
head(delivdf)
names(delivdf)
sapply(delivdf,class)



############################################################################
##  STEP 1 : Prepare  Your Data 
############################################################################

# We are only interested in batsmen, so let's remove unneeded columns" 

cols_to_keep= c("batter", "match_id", "inning", "over", "batsman_runs", "dismissal_kind")

batterdf<- delivdf[,cols_to_keep]
dim(batterdf)
batterdf[sample(20),]

# Count missing values
#gives for whole sum(is.na(batterdf))
#the following gives for each column individually

batterdf %>% sapply( function(col) { sum(is.na(col))})
batterdf %>% sapply( function(col) { sum(is.null(col))})
#batterdf$dismissal_kind[is.na]
# remove NA values from dismissal_kind 
batterdf$dismissal_kind[is.na(batterdf$dismissal_kind)]<- "not out"
batterdf[sample(20),]


# find data type of each column 
sapply(batterdf,class)
#convert columns to appropriate kind
batterdf$batter<- as.factor(batterdf$batter)
batterdf$dismissal_kind <- as.factor(batterdf$dismissal_kind )

sapply(batterdf,class)

batterdf[sample(20), ] 

## Note:##
# Generally, we  would have converted all elements of 'batter' column to lowercase and removed space
# to so that erratic spaces and capitalization did not make the same batter appear like two different people
# but here the data is clean , so skipping that 

############################################################################
##  Step 2:  Convert this data to aggregate data for each batsman 
############################################################################



# We plan to use  lifetime strike rates etc and also per match averages
# So we will need to aggregate data in multiple ways 

## First we Aggregate lifetime data for each batter. WE want to analyse their overall performance 
##  This can  be done by using aggregate function, or using group_by in dplyr 
## Let's see which one is more convenient 
?aggregate  

bat_aggregated<- aggregate( batsman_runs~batter, data= batterdf, 
                               FUN = function(x)
                                 c( 
                                   "Totalruns" = sum(x), 
                                   "Ballsplayed" = length(x)  #each row is one ball 
                                 )  ) 

head(bat_aggregated) 
View(bat_aggregated)

bat_aggregated%>% dim() #why is it 2 columns? it should be 3! Because..
bat_aggregated[,2] %>% head() # aggregate col2 is made of 2 subcolumns!

# Now let's try the other method, aggregating  with dplyr's  group_by  :
# we first use group_by , and then summarise 
# then convert the result to data.frame 
# (dplyr creates efficient 'tibble' data type. But we convert to data.frame)


bat_aggr_dply <- batterdf %>% group_by( batter) %>% 
  summarise( Totalruns= sum(batsman_runs),
             Ballsplayed = length(batsman_runs),
             Strikerate= 100* sum(batsman_runs)/length(batsman_runs)
  ) %>%  as.data.frame()  
bat_aggr_dply %>% View()  
bat_aggr_dply %>% dim()

# So, we think using dplyr+group_by is better 
# remove previous output, keep this 
rm(bat_aggregated)

## Now, why keep all the players? 
# Just keep batsmen who have played more than 500 balls 

#here 50 is binge that is no of bars 
hist(bat_aggr_dply$Ballsplayed, 50)

hist(bat_aggr_dply$Ballsplayed,breaks = 100,plot = F)

#there are many ways of finding how many such batsmen are there
bat_aggr_dply[bat_aggr_dply$Ballsplayed>500, ] %>% dim()


# so there are 117 such batters. Good for us

#let's filter the original data 
true_batters<- bat_aggr_dply$batter[bat_aggr_dply$Ballsplayed>500]
true_batters

# this is how you can filter using %in% 
1:5 %in% c(3,4)

batterdf_final<- batterdf[batterdf$batter %in%  true_batters,] 

batterdf %>% dim()
batterdf_final %>% dim()
# thus the data is now a lot smaller but not much. Why might that be ? 

#now let's recalculate lifetime performance of batters

bat_aggr_final <- batterdf_final %>% group_by( batter) %>% 
  summarise( Totalruns= sum(batsman_runs),
             Ballsplayed = length(batsman_runs),
             Strikerate= 100* sum(batsman_runs)/length(batsman_runs)
  ) %>%  as.data.frame()  

bat_aggr_final %>% View()  
bat_aggr_final %>% dim() 

# now let's see how many boundaries each one has hit. 
# let's do this -

batter_boundaries_total<- batterdf_final %>% group_by( batter) %>% 
  summarise( num_6= sum(batsman_runs ==6) ,
             num_4= sum (batsman_runs ==4)              
  ) %>% data.frame()

# Q&A : why sum instead of length?! T F F F T  

names(batter_boundaries_total)
View(batter_boundaries_total)
# no wonder , Rohit Sharma and Chris Gayle top the list ! 

#LEt's add one more column to this table
# it's done like this - 
batter_boundaries_total$total_runs_from_boundaries= 
  6*batter_boundaries_total$num_6+
  4 * batter_boundaries_total$num_4

View(batter_boundaries_total)

# now remember ,these total numbers will not be so helpful for us 
# because new players might have fewer runs despite being good 

# We should therefore see how each player has done on per  match  basis 
# so that we can find out his consistency etc 

# But before that, we can merge these two dataframes we have computed 

?cbind.data.frame
?merge

# merge seems better for this . we can tell it which column to use as  key 
batter_lifetime<- merge( bat_aggr_final, batter_boundaries_total, by.x = "batter", by.y = "batter" ) 

# all columns are nnow side by side in one dataframe
lapply(list(bat_aggr_final, batter_boundaries_total, batter_lifetime) , dim)

View(batter_lifetime)

## Now we can do some calculations and  add some more columns to make it an even comparison 
## between players who got to play more matches and maybe new players with few matches

names(batter_lifetime)

# % of runs made as boundaries
pct_runsfrom_boundaries<-  100* batter_lifetime$total_runs_from_boundaries/ batter_lifetime$Totalruns

# hits a boundary  every n balls 
balls_per_6<- batter_lifetime$Ballsplayed/batter_lifetime$num_6
balls_per_4<- batter_lifetime$Ballsplayed/batter_lifetime$num_4

#add these columns to main dataframe
batter_lifetime$pct_runsfrom_boundaries<- pct_runsfrom_boundaries
batter_lifetime$balls_per_6<- balls_per_6
batter_lifetime$balls_per_4<- balls_per_4

View(batter_lifetime)

#interesting. Sunil Narine is already a hero. He won the 2024  IPL for Kolkata 
# For which we have no data 


############################################################################
##  Step 3:  Match level aggregate performance  for each batsman 
############################################################################

# AT this point we can perform cluster analysis if we want
# But to learn more we can go and do some match-level aggregation for each player as well
batterdf_final %>% names()
batterdf_final %>% dim()  # just ensuring we've filtered  data


batter_matchwise<- batterdf_final %>% group_by( batter, match_id ) %>% 
  summarise( Totalruns_m= sum(batsman_runs),
             Ballsplayed_m = length(batsman_runs),
             Strikerate_m= 100* sum(batsman_runs)/length(batsman_runs),
             num6_m= sum(batsman_runs ==6) ,
             num4_m= sum (batsman_runs ==4),
             num0_m= sum (batsman_runs ==0)
  ) %>%  as.data.frame()

# Take a good look at the shape of this table
# now player+match_id  together form a key 

View( batter_matchwise )  

batter_matchwise %>% dim() #10389     8

# now we have to aggregate this data into the shape of lifetime data above 
# tip: now that the data is per match instead of per ball, mean gives you mean  per match
names(batter_matchwise)


batter_matchwise_lifetime<- batter_matchwise %>% group_by( batter) %>% 
  summarise( count_m= length(match_id), 
             avgruns_m= mean(Totalruns_m),
             avgballs_m = mean(Ballsplayed_m),
             avgstrikerate_m= mean(Strikerate_m),
             avg6_m= mean(num6_m) ,
             avg4_m= mean (num4_m),
             avg0_m= mean (num0_m),
             coeffvariance_runs_m=sd(Totalruns_m)/mean(Totalruns_m), #standard deviation of runs per match shows consistency 
             coeffvariance_strike_m=sd(Strikerate_m)/mean(Strikerate_m) # divided by mean it actually is COV . More homogeneous
             # remember  this one is  coefficient  of variance, not covariance 
  ) %>%  as.data.frame()

View(batter_matchwise_lifetime)

# you can see that Rahul Dravid is among the most consistent! 

# A Rider - if you compare these averages with online figures, they might be slightly lower
# eg, virat kohli - https://www.espncricinfo.com/ask/cricket-qna/Virat-Kohli-average-in-IPL&tournament=ipl 
# this is because we are not compensating for not-outs. 
# WE can ignore it for now 
# As a later exercise, try to figure out how many times a player stayed  not out :-) 


batter_matchwise_lifetime %>% dim()

############################################################################
##  Step 4:  Merge the two aggregate data into one final dataframe  
############################################################################

# now that we have one row per batter, we can merge the data 

lapply(list(batter_lifetime, batter_matchwise_lifetime), names)

batter_lifetime_final<- merge( batter_lifetime, batter_matchwise_lifetime, by = "batter") 

View(batter_lifetime_final)

# Now we should remove some columns so that number of matches does not affect player comparison
# though someone can argue that it is important , still  it makes  sense to our guy  
batter_lifetime_final %>% names()

names(batter_lifetime_final)[1:10]


cols_to_drop<- c(  "Totalruns", "Ballsplayed", "Strikerate", "num_6" , "num_4",
                   "total_runs_from_boundaries",  "count_m"  )

cols_to_keep<- setdiff( names(batter_lifetime_final), cols_to_drop )

print( cols_to_keep )

#keep only these columns 
batter_lifetime_final<- batter_lifetime_final[, cols_to_keep]

batter_lifetime_final %>% dim()
sapply( batter_lifetime_final , FUN=class)

#glimpse(batter_lifetime_final)


############################################################################
##  Step 5 :  Save this data  so that later on we can start from  here
############################################################################

# we use dput so that data types are preserved   
dput( batter_lifetime_final, file = "batter_lifetime_final.rd")
write.csv(batter_lifetime_final, file = "batter_lifetime_final.csv",row.names = FALSE)#we don't want row numbers

### END 
