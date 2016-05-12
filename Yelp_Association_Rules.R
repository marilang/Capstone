#=========================================================
#  Cisco Data Science Level 1 Program
#
#	Content: Association Rules for Yelp
#          Capstone Project
#=========================================================

rm(list=ls())
library(arules)
yelp_key_business_data <- read.csv('yelp_reduced_business_data1.csv',header=T,colClasses="factor")

summary(yelp_key_business_data)

####
X                         business_id             name        review_count   attributes.Waiter.Service
10     :    1   #NAME?                :  271   McDonald's :  293   3      : 1840        : 3488              
100    :    1   __etvGuL2dh_a1LOT0gNYQ:    1   Subway     :  273   4      : 1382   FALSE: 6752              
1000   :    1   __kNfrrGoUXoF-BYciMU_Q:    1   Taco Bell  :  154   5      : 1219   TRUE :11652              
10000  :    1   __Y2jjdCFHvq3rzSbpDBlw:    1   Wendy's    :  123   6      : 1048                            
10001  :    1   _-1EgXrkOlKajCsmasuEgg:    1   Pizza Hut  :  119   7      :  879                            
10002  :    1   _-6I6VXjr-NiwIBa_1uI4A:    1   Burger King:  113   8      :  787                            
(Other):21886   (Other)               :21616   (Other)    :20817   (Other):14737                            
hours.Saturday.open hours.Sunday.open attributes.Wi.Fi attributes.Takes.Reservations attributes.Accepts.Credit.Cards
:9080               :10568         :7774             : 2630                        : 1479                    
11:00  :4438        11:00  : 3578     free:4874        FALSE:12329                   {}   :    2                    
10:00  :1318        10:00  : 1214     no  :9081        TRUE : 6933                   FALSE:  822                    
12:00  : 777        12:00  : 1021     paid: 163                                      TRUE :19589                    
17:00  : 767        9:00   :  680                                                                                   
7:00   : 757        7:00   :  678                                                                                   
(Other):4755        (Other): 4153                                                                                   
attributes.Parking.valet attributes.Delivery     stars       starsr     
: 3211                   : 2719         3.5    :6048   High: 8519  
FALSE:18065              FALSE:15548         4      :5768   Low :13373  
TRUE :  616              TRUE : 3625         3      :3939               
4.5    :2362               
2.5    :2108               
2      : 853               
(Other): 814               

#####

yelp_rules <- apriori(yelp_key_business_data)

#####
Apriori

Parameter specification:
confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
0.8    0.1    1 none FALSE            TRUE     0.1      1     10  rules FALSE

Algorithmic control:
filter tree heap memopt load sort verbose
0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 2189 

set item appearances ...[0 item(s)] done [0.00s].
set transactions ...[59673 item(s), 21892 transaction(s)] done [0.05s].
sorting and recoding items ... [25 item(s)] done [0.01s].
creating transaction tree ... done [0.01s].
checking subsets of size 1 2 3 4 5 6 7 done [0.01s].
writing ... [1022 rule(s)] done [0.00s].
creating S4 object  ... done [0.02s].
#####

# 1022 rules is too big a number . Our interest is to find factors that impact the rating of the restaurant
# Hence confining to those rules that has an outcome of higher rating

yelp_rules <- apriori(yelp_key_business_data , appearance = list(rhs= c("starsr=High"), default="lhs"))

######
Apriori

Parameter specification:
confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
0.8    0.1    1 none FALSE            TRUE     0.1      1     10  rules FALSE

Algorithmic control:
filter tree heap memopt load sort verbose
0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 2189 

set item appearances ...[1 item(s)] done [0.00s].
set transactions ...[59673 item(s), 21892 transaction(s)] done [0.06s].
sorting and recoding items ... [25 item(s)] done [0.01s].
creating transaction tree ... done [0.02s].
checking subsets of size 1 2 3 4 5 6 7 done [0.01s].
writing ... [30 rule(s)] done [0.00s].
creating S4 object  ... done [0.02s].

#####

summary(yelp_rules)   

#####
set of 30 rules

rule length distribution (lhs + rhs):sizes
2  3  4  5  6 
2  7 12  8  1 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.000   3.000   4.000   3.967   5.000   6.000 

summary of quality measures:
support         confidence      lift     
Min.   :0.1012   Min.   :1    Min.   :2.57  
1st Qu.:0.1111   1st Qu.:1    1st Qu.:2.57  
Median :0.1284   Median :1    Median :2.57  
Mean   :0.1447   Mean   :1    Mean   :2.57  
3rd Qu.:0.1631   3rd Qu.:1    3rd Qu.:2.57  
Max.   :0.2635   Max.   :1    Max.   :2.57  

mining info:
data ntransactions support confidence
yelp_key_business_data         21892     0.1        0.8

#####

inspect(yelp_rules)

######
   lhs                                       rhs             support confidence     lift
1  {stars=4.5}                            => {starsr=High} 0.1078933          1 2.569785
2  {stars=4}                              => {starsr=High} 0.2634752          1 2.569785
3  {attributes.Wi.Fi=no,                                                                
stars=4}                              => {starsr=High} 0.1250228          1 2.569785
4  {hours.Sunday.open=,                                                                 
stars=4}                              => {starsr=High} 0.1019551          1 2.569785
5  {attributes.Waiter.Service=TRUE,                                                     
stars=4}                              => {starsr=High} 0.1575918          1 2.569785
6  {attributes.Takes.Reservations=FALSE,                                                
stars=4}                              => {starsr=High} 0.1421524          1 2.569785
7  {attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1954138          1 2.569785
8  {attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.2271606          1 2.569785
9  {attributes.Accepts.Credit.Cards=TRUE,                                               
stars=4}                              => {starsr=High} 0.2389914          1 2.569785
10 {attributes.Wi.Fi=no,                                                                
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1011785          1 2.569785
11 {attributes.Wi.Fi=no,                                                                
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1170747          1 2.569785
12 {attributes.Wi.Fi=no,                                                                
attributes.Accepts.Credit.Cards=TRUE,                                               
stars=4}                              => {starsr=High} 0.1168007          1 2.569785
13 {attributes.Waiter.Service=TRUE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1251599          1 2.569785
14 {attributes.Waiter.Service=TRUE,                                                     
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1427462          1 2.569785
15 {attributes.Waiter.Service=TRUE,                                                     
attributes.Accepts.Credit.Cards=TRUE,                                               
stars=4}                              => {starsr=High} 0.1504202          1 2.569785
16 {attributes.Takes.Reservations=FALSE,                                                
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1104970          1 2.569785
17 {attributes.Takes.Reservations=FALSE,                                                
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1316919          1 2.569785
18 {attributes.Takes.Reservations=FALSE,                                                
attributes.Accepts.Credit.Cards=TRUE,                                               
stars=4}                              => {starsr=High} 0.1318290          1 2.569785
19 {attributes.Parking.valet=FALSE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1745843          1 2.569785
20 {attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1838114          1 2.569785
21 {attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.2136397          1 2.569785
22 {attributes.Wi.Fi=no,                                                                
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1094464          1 2.569785
23 {attributes.Waiter.Service=TRUE,                                                     
attributes.Parking.valet=FALSE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1130093          1 2.569785
24 {attributes.Waiter.Service=TRUE,                                                     
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1204550          1 2.569785
25 {attributes.Waiter.Service=TRUE,                                                     
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1368080          1 2.569785
26 {attributes.Takes.Reservations=FALSE,                                                
attributes.Parking.valet=FALSE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1032341          1 2.569785
27 {attributes.Takes.Reservations=FALSE,                                                
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1027773          1 2.569785
28 {attributes.Takes.Reservations=FALSE,                                                
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
stars=4}                              => {starsr=High} 0.1230130          1 2.569785
29 {attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1649004          1 2.569785
30 {attributes.Waiter.Service=TRUE,                                                     
attributes.Accepts.Credit.Cards=TRUE,                                               
attributes.Parking.valet=FALSE,                                                     
attributes.Delivery=FALSE,                                                          
stars=4}                              => {starsr=High} 0.1085785          1 2.569785
> 

######
yelp_rules <- apriori(yelp_key_business_data,  appearance = list(rhs= c("starsr=Low"), default="lhs"))

#####
Apriori

Parameter specification:
confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
0.8    0.1    1 none FALSE            TRUE     0.1      1     10  rules FALSE

Algorithmic control:
filter tree heap memopt load sort verbose
0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 2189 

set item appearances ...[1 item(s)] done [0.00s].
set transactions ...[59673 item(s), 21892 transaction(s)] done [0.05s].
sorting and recoding items ... [25 item(s)] done [0.01s].
creating transaction tree ... done [0.01s].
checking subsets of size 1 2 3 4 5 6 7 done [0.01s].
writing ... [41 rule(s)] done [0.00s].
creating S4 object  ... done [0.02s].

>
####
summary(yelp_rules) 

#####
set of 41 rules

rule length distribution (lhs + rhs):sizes
2  3  4  5  6 
2 12 16  9  2 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.000   3.000   4.000   3.927   5.000   6.000 

summary of quality measures:
support         confidence      lift      
Min.   :0.1004   Min.   :1    Min.   :1.637  
1st Qu.:0.1130   1st Qu.:1    1st Qu.:1.637  
Median :0.1278   Median :1    Median :1.637  
Mean   :0.1436   Mean   :1    Mean   :1.637  
3rd Qu.:0.1612   3rd Qu.:1    3rd Qu.:1.637  
Max.   :0.2763   Max.   :1    Max.   :1.637  

mining info:
data ntransactions support confidence
yelp_key_business_data         21892     0.1        0.8

#####
inspect(yelp_rules)

 lhs                                       rhs            support confidence    lift
1  {stars=3}                              => {starsr=Low} 0.1799287          1 1.63703
2  {stars=3.5}                            => {starsr=Low} 0.2762653          1 1.63703
3  {attributes.Takes.Reservations=FALSE,                                              
stars=3}                              => {starsr=Low} 0.1053810          1 1.63703
4  {attributes.Delivery=FALSE,                                                        
stars=3}                              => {starsr=Low} 0.1278092          1 1.63703
5  {attributes.Parking.valet=FALSE,                                                   
stars=3}                              => {starsr=Low} 0.1522931          1 1.63703
6  {attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3}                              => {starsr=Low} 0.1654486          1 1.63703
7  {hours.Saturday.open=,                                                             
stars=3.5}                            => {starsr=Low} 0.1009044          1 1.63703
8  {attributes.Wi.Fi=no,                                                              
stars=3.5}                            => {starsr=Low} 0.1235154          1 1.63703
9  {hours.Sunday.open=,                                                               
stars=3.5}                            => {starsr=Low} 0.1150649          1 1.63703
10 {attributes.Waiter.Service=TRUE,                                                   
stars=3.5}                            => {starsr=Low} 0.1612004          1 1.63703
11 {attributes.Takes.Reservations=FALSE,                                              
stars=3.5}                            => {starsr=Low} 0.1507857          1 1.63703
12 {attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1980175          1 1.63703
13 {attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.2327791          1 1.63703
14 {attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3.5}                            => {starsr=Low} 0.2529234          1 1.63703
15 {attributes.Takes.Reservations=FALSE,                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3}                              => {starsr=Low} 0.1004477          1 1.63703
16 {attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3}                              => {starsr=Low} 0.1130093          1 1.63703
17 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Delivery=FALSE,                                                        
stars=3}                              => {starsr=Low} 0.1226475          1 1.63703
18 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
stars=3}                              => {starsr=Low} 0.1454413          1 1.63703
19 {hours.Saturday.open=,                                                             
hours.Sunday.open=,                                                               
stars=3.5}                            => {starsr=Low} 0.1004477          1 1.63703
20 {attributes.Wi.Fi=no,                                                              
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1146081          1 1.63703
21 {attributes.Wi.Fi=no,                                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3.5}                            => {starsr=Low} 0.1186278          1 1.63703
22 {attributes.Waiter.Service=TRUE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1262105          1 1.63703
23 {attributes.Waiter.Service=TRUE,                                                   
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1460351          1 1.63703
24 {attributes.Waiter.Service=TRUE,                                                   
attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3.5}                            => {starsr=Low} 0.1556733          1 1.63703
25 {attributes.Takes.Reservations=FALSE,                                              
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1153846          1 1.63703
26 {attributes.Takes.Reservations=FALSE,                                              
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1351635          1 1.63703
27 {attributes.Takes.Reservations=FALSE,                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
stars=3.5}                            => {starsr=Low} 0.1439339          1 1.63703
28 {attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1743559          1 1.63703
29 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1896126          1 1.63703
30 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.2218162          1 1.63703
31 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3}                              => {starsr=Low} 0.1088982          1 1.63703
32 {attributes.Wi.Fi=no,                                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1102229          1 1.63703
33 {attributes.Waiter.Service=TRUE,                                                   
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1144254          1 1.63703
34 {attributes.Waiter.Service=TRUE,                                                   
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1232414          1 1.63703
35 {attributes.Waiter.Service=TRUE,                                                   
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1418326          1 1.63703
36 {attributes.Takes.Reservations=FALSE,                                              
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1040563          1 1.63703
37 {attributes.Takes.Reservations=FALSE,                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1106340          1 1.63703
38 {attributes.Takes.Reservations=FALSE,                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
stars=3.5}                            => {starsr=Low} 0.1303673          1 1.63703
39 {attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1674128          1 1.63703
40 {attributes.Waiter.Service=TRUE,                                                   
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1117303          1 1.63703
41 {attributes.Takes.Reservations=FALSE,                                              
attributes.Accepts.Credit.Cards=TRUE,                                             
attributes.Parking.valet=FALSE,                                                   
attributes.Delivery=FALSE,                                                        
stars=3.5}                            => {starsr=Low} 0.1003563          1 1.63703
> ######