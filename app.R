## app.R ##
library(shinydashboard)
library(ggplot2)
library(scales)
library(ggmap)
library(tidyr)
library(dplyr)
library(shiny)
library(data.table)
library(reshape2)
library(plotly)
##########################################################
### This section is where the data is wrangled for ACT ###
##########################################################

### bring in ACT datasets
file_act_2122 = "https://nep.education.ne.gov/Downloads/20212022/ACT_Composite_20212022.csv"
file_act_2021 = "https://nep.education.ne.gov/Downloads/20202021/ACT_Composite_20202021.csv"
file_act_1819 = "https://nep.education.ne.gov/Downloads/20182019/ACT_Composite_20182019.csv"
file_act_1718 = "https://nep.education.ne.gov/Downloads/20172018/ACT_Composite_20172018.csv"
act_2122 = read.csv(file_act_2122, header=TRUE)
act_2021 = read.csv(file_act_2021, header=TRUE)
act_1819 = read.csv(file_act_1819, header=TRUE)
act_1718 = read.csv(file_act_1718, header=TRUE)
### remove 'data as of' column (which does not exist in 17-18 and 18-19 school years)
act_2122 = act_2122[,-10]
act_2021 = act_2021[,-10]

### combine all the years into one dataframe
act_all_years = do.call("rbind", list(act_2122,act_2021,act_1819,act_1718))
colnames(act_all_years) = tolower(colnames(act_all_years))
act_all_years["average_composite_score"][act_all_years["average_composite_score"] == -1] = NA
act_all_NARM = na.omit(act_all_years)
act_all_NARM = act_all_NARM %>%
   filter(level != "ST" & level != "LC") %>%
   filter(name != "STATE OF NEBRASKA")

### list of Nebraska counties in alphabetical order (corresponding to NE Department of Revenue)
NE_counties = c("Adams","Antelope","Arthur","Banner","Blaine","Boone","Box Butte","Boyd","Brown","Buffalo","Burt","Butler","Cass","Cedar","Chase","Cherry","Cheyenne","Clay","Colfax","Cuming","Custer","Dakota","Dawes","Dawson","Deuel","Dixon","Dodge","Douglas","Dundy","Fillmore","Franklin","Frontier","Furnas","Gage","Garden","Garfield","Gosper","Grant","Greeley","Hall","Hamilton","Harlan","Hayes","Hitchcock","Holt","Hooker","Howard","Jefferson","Johnson","Kearney","Keith","Keya Paha","Kimball","Knox","Lancaster","Lincoln","Logan","Loup","Madison","McPherson","Merrick","Morrill","Nance","Nemaha","Nuckolls","Otoe","Pawnee","Perkins","Phelps","Pierce","Platte","Polk","Red Willow","Richardson","Rock","Saline","Sarpy","Saunders","Scotts Bluff","Seward","Sheridan","Sherman","Sioux","Stanton","Thayer","Thomas","Thurston","Valley","Washington","Wayne","Webster","Wheeler","York")
NE_counties = as.data.frame(tolower(NE_counties))
colnames(NE_counties) = c("county")

### replace county numbers with county names
for(i in 1:nrow(act_all_NARM)){
   x = act_all_NARM[i,4]
   act_all_NARM[i,4] = NE_counties[x,1]
}

### get average act score by county for each year
act_county_mean =  act_all_NARM %>%
   group_by(county, datayears) %>%
   summarize(county_average_score = mean(average_composite_score))
act_county_mean$datayears = as.character(act_county_mean$datayears)

##########################################################
### This section is where the data is wrangled for PPE ###
##########################################################

### bring in Per Pupil Expenditure
file_ppe_2021 = "https://nep.education.ne.gov/Downloads/20202021/Per_Pupil_Expenditures_20202021.csv"
file_ppe_1819 = "https://nep.education.ne.gov/Downloads/20182019/Per_Pupil_Expenditures_20182019.csv"
ppe_2021 = read.csv(file_ppe_2021,header=TRUE)
ppe_1819 = read.csv(file_ppe_1819,header=TRUE)

### remove 'data as of' column
ppe_2021 = ppe_2021[,-14]
ppe_1819 = ppe_1819[,-14]

### make all column names lower-case for merging
colnames(ppe_2021) = tolower(colnames(ppe_2021))
colnames(ppe_1819) = tolower(colnames(ppe_1819))
colnames(act_2021) = tolower(colnames(act_2021))
colnames(act_1819) = tolower(colnames(act_1819))

### select relevant columns
act_2021 = act_2021 %>%
   select(level,datayears,county,district,school,name,average_composite_score,students_with_composite_scores)

act_1819 = act_1819 %>%
   select(level,datayears,county,district,school,name,average_composite_score,students_with_composite_scores)

### merge Per Pupil Expenditure for with ACT scores
### process of merging will filter PPE to only schools with ACT scores
act_ppe_2021 = merge(ppe_2021,act_2021,by = c("county","district","school"))
act_ppe_1819 = merge(ppe_1819,act_1819,by = c("county","district","school"))

### remove duplicate columns
act_ppe_2021 = act_ppe_2021[,c(-5,-6,-14)]
act_ppe_1819 = act_ppe_1819[,c(-5,-6,-14)]

### get all ACT and PPE data into a single DF
act_ppe = rbind(act_ppe_1819,act_ppe_2021)
### replace -1 values with NA
act_ppe["average_composite_score"][act_ppe["average_composite_score"] == -1] = NA
### omit NA values
act_ppe_NARM = na.omit(act_ppe)
### renumber row names
row.names(act_ppe_NARM) = 1:nrow(act_ppe_NARM)

### replace county number with county name
for(i in 1:nrow(act_ppe_NARM)){
   x = act_ppe_NARM[i,1]
   act_ppe_NARM[i,1] = NE_counties[x,1]
}

### melting data so all expenditure types are now factors
act_ppe_select = pivot_longer(act_ppe_NARM,c(5:11),names_to="expenditure","amount")

### loading in NE county boundaries for mapping
mdat = map_data("county") %>% filter(region == "nebraska")

### creating correlations
correlation = round(cor(act_ppe_NARM[,c(5,6,7,8,9,10,11,14)]),3)

###################################################################
### This section is where the data is wrangled for DEMOGRAPHICS ###
###################################################################

### bring in data for ACT_all_subjects
act_score2021=read.csv("https://nep.education.ne.gov/Downloads/20202021/ACT_AllSubjects_20202021.csv")
act_score2019=read.csv("https://nep.education.ne.gov/Downloads/20182019/ACT_AllSubjects_20182019.csv")
act_score2018=read.csv("https://nep.education.ne.gov/Downloads/20172018/ACT_AllSubjects_20172018.csv")
act_score2017=read.csv("https://nep.education.ne.gov/Downloads/20162017/ACT_AllSubjects_20162017.csv")

### select only unique/distinct rows
act_score2021=act_score2021%>% distinct(.keep_all=TRUE)
act_score2019=act_score2019%>% distinct(.keep_all=TRUE)
act_score2018=act_score2018%>% distinct(.keep_all=TRUE)
act_score2017=act_score2017%>% distinct(.keep_all=TRUE)

### select only observations that have reported a score (no -1 in Average Scale Score)
act_score2017=act_score2017[!(act_score2017$AVERAGE_SCALE_SCORE==-1),]
act_score2018=act_score2018[!(act_score2018$AVERAGE_SCALE_SCORE==-1),]
act_score2019=act_score2019[!(act_score2019$AVERAGE_SCALE_SCORE==-1),]
act_score2021=act_score2021[!(act_score2021$AVERAGE_SCALE_SCORE==-1),]

### evaluate at STATE level of aggregation
act_20202021 <- filter(act_score2021, Level == 'ST') 
act_20182019 <- filter(act_score2019, Level == 'ST')
act_20172018 <- filter(act_score2018, Level == 'ST')
act_20162017 <- filter(act_score2017, Level == 'ST')

### remove TESTED_COUNT column
act_20202021=act_20202021[!(colnames(act_20202021) %in% c("TESTED_COUNT"))]

### combine data years into single dataframe
act_score=rbind(act_20162017,act_20172018,act_20182019,act_20202021)

### create subset dataframe for evaluating gender and ACT
genderbased=subset(act_score, SUBGROUP_TYPE=='GENDER')
genderbased=genderbased[-c(11:15)]
genderbased["Datayears"][genderbased["Datayears"] == 20202021] <- "2020-2021"
genderbased["Datayears"][genderbased["Datayears"] == 20182019] <- "2018-2019"
genderbased["Datayears"][genderbased["Datayears"] == 20172018] <- "2017-2018"
genderbased["Datayears"][genderbased["Datayears"] == 20162017] <- "2016-2017"

### create subset dataframe for evaluating race and ACT
racebased=subset(act_score, SUBGROUP_TYPE=='RACE ETHNICITY')
racebased=racebased[-c(11:15)]
racebased["Datayears"][racebased["Datayears"] == 20202021] <- "2020-2021"
racebased["Datayears"][racebased["Datayears"] == 20182019] <- "2018-2019"
racebased["Datayears"][racebased["Datayears"] == 20172018] <- "2017-2018"
racebased["Datayears"][racebased["Datayears"] == 20162017] <- "2016-2017"

#################################################################
### This section is where the data is wrangled for ATTENDANCE ###
#################################################################


AR<-read.csv("https://nep.education.ne.gov/Downloads/20202021/Attendance_Rate_20202021.csv")
AR1=as.data.frame(AR)
AR2<-AR1[,c("School.Year","School","District","Agency.Name","Attendance.Rate")]
AR3<-subset(AR2,School == "1")

# create AR7 for later merging
AR7<-AR3[,c("Agency.Name","District")]
names(AR7)<-c('agency_name','district')

# create AR5 and AR6 for later merging
AR5<-AR3[,c("School.Year","Agency.Name","District","Attendance.Rate")]
names(AR5)<-c('school_year','agency_name','district','attendance_rate')
#AR5 = as.data.table(AR5)
#head(AR5)
AR6<-dcast(AR5,agency_name~school_year)

# merge/join AR6 and AR7
AR8<-merge(AR6,AR7, by=c("agency_name"), all = TRUE)
#left_join(AR6,AR7, by = c("agency_name" = "agency_name"))
AR9<-AR8[!duplicated(AR8$agency_name),]


ARNA1520 <- AR9[,!names(AR9) %in% c("20072008", "20082009", "20092010", "20102011", "20112012", "20122013", "20132014", "20142015")]
row.names(ARNA1520) <- 1:nrow(ARNA1520)

ARNAdf = ARNA1520[!apply(ARNA1520[2:7],1,sjmisc::all_na),]
row.names(ARNAdf) <- 1:nrow(ARNAdf)
ARNAdf[is.na(ARNAdf)] = 0
A1 = ARNAdf
A1$`20152016` <- as.numeric(as.character(A1$`20152016`))
A1$`20162017` <- as.numeric(as.character(A1$`20162017`))
A1$`20172018` <- as.numeric(as.character(A1$`20172018`))
A1$`20182019` <- as.numeric(as.character(A1$`20182019`))
A1$`20192020` <- as.numeric(as.character(A1$`20192020`))
A1$`20202021` <- as.numeric(as.character(A1$`20202021`))
A2 = A1
A2[24,2:7]<-A2[24,2:7]+A2[25,2:7]
A2 = A2[-25,]
row.names(A2) <- 1:nrow(A2)
A3 = A2
A3[40,2:7]<-A3[40,2:7]+A3[39,2:7]
A3 = A3[-39,]
row.names(A3) <- 1:nrow(A3)
A4 = A3
A4[80,2:7]<-A4[80,2:7]+A4[79,2:7]
A4 = A4[-79,]
row.names(A4) <- 1:nrow(A4)
A5 = A4
A5[103,2:7]<-A5[103,2:7]+A5[102,2:7]
A5 = A5[-102,]
row.names(A5) <- 1:nrow(A5)
A6 = A5
A6[118,2:7]<-A6[118,2:7]+A6[119,2:7]
A6 = A6[-119,]
row.names(A6) <- 1:nrow(A6)
A7 = A6
A7[154,2:7]<-A7[154,2:7]+A7[155,2:7]
A7 = A7[-155,]
row.names(A7) <- 1:nrow(A7)
A8 = A7
A8[164,2:7]<-A8[164,2:7]+A8[165,2:7]
A8 = A8[-165,]
row.names(A8) <- 1:nrow(A8)
A9 = A8
A9[172,2:7]<-A9[172,2:7]+A9[171,2:7]
A9 = A9[-171,]
row.names(A9) <- 1:nrow(A9)
A10 = A9
A10[176,2:7]<-A10[176,2:7]+A10[175,2:7]
A10 = A10[-175,]
row.names(A10) <- 1:nrow(A10)
A11 = A10
A11[205,2:7]<-A11[205,2:7]+A11[206,2:7]
A11 = A11[-206,]
row.names(A11) <- 1:nrow(A11)
A12 = A11
A12[207,2:7]<-A12[207,2:7]+A12[206,2:7]
A12 = A12[-206,]
row.names(A12) <- 1:nrow(A12)
A13 = A12
A13[216,2:7]<-A13[216,2:7]+A13[215,2:7]
A13 = A13[-215,]
row.names(A13) <- 1:nrow(A13)
A14 = A13
A14[31,2:7]<-A14[31,2:7]+A14[234,2:7]
A14 = A14[-234,]
row.names(A14) <- 1:nrow(A14)
A15 = A14
A15[240,2:7]<-A15[239,2:7]+A15[240,2:7]
A15 = A15[-239,]
row.names(A15) <- 1:nrow(A15)
A16 = A15
A16 = A16[-214,]
row.names(A16) <- 1:nrow(A16)
A17 = A16
A17 = A17[-48,]
row.names(A17) <- 1:nrow(A17)
A18 = A17
A18 = A18[-73,]
row.names(A18) <- 1:nrow(A18)
A19 = A18
A19[195,2:7]<-A19[195,2:7]+A19[196,2:7]
A19 = A19[-196,]
row.names(A19) <- 1:nrow(A19)
A20 = A19
A20 = A20[-133,]
row.names(A20) <- 1:nrow(A20)
A21 = A20
A21 = A21[-219,]
row.names(A21) <- 1:nrow(A21)
A22 = A21
A22 = A22[-205,]
row.names(A22) <- 1:nrow(A22)
A23 = A22
A23 = A23[-70,]
row.names(A23) <- 1:nrow(A23)
A24 = A23
A24 = A24[-85,]
row.names(A24) <- 1:nrow(A24)
A25 = A24
A25 = A25[-115,]
row.names(A25) <- 1:nrow(A25)
A26 = A25
A26 = A26[-150,]
row.names(A26) <- 1:nrow(A26)
A26$`20152016` <- as.character(A26$`20152016`)
A26$`20162017` <- as.character(A26$`20162017`)
A26$`20172018` <- as.character(A26$`20172018`)
A26$`20182019` <- as.character(A26$`20182019`)
A26$`20192020` <- as.character(A26$`20192020`)
A26$`20202021` <- as.character(A26$`20202021`)
A26[A26 == -1] = 0
A26[A26 == 0] = NA
A26$`20152016` <- as.numeric(as.character(A26$`20152016`))
A26$`20162017` <- as.numeric(as.character(A26$`20162017`))
A26$`20172018` <- as.numeric(as.character(A26$`20172018`))
A26$`20182019` <- as.numeric(as.character(A26$`20182019`))
A26$`20192020` <- as.numeric(as.character(A26$`20192020`))
A26$`20202021` <- as.numeric(as.character(A26$`20202021`))
A26$mean <- rowMeans(subset(A26, select = c("20152016", "20162017", "20172018", "20182019", "20192020", "20202021")), na.rm = TRUE)
deduped.data = unique(A26[ , 8] )
unidis = sort(deduped.data)
A30<-A26 %>% group_by(district) %>% summarise_each(funs(mean=mean(., na.rm=TRUE)))
names(A30)<-c("district","agencyname","20152016","20162017","20172018","20182019","20192020","20202021","mean")
A30$district<-as.character(A30$district)

A25<-A30 %>% summarise_if(is.numeric, mean,na.rm=TRUE)
A26<-A25[,c("20152016","20162017","20172018","20182019","20192020","20202021")]
names(A26)<-c("Average Attendance 2015-2016","Average Attendance 2016-2017","Average Attendance 2017-2018","Average Attendance 2018-2019","Average Attendance 2019-2020","Average Attendance 2020-2021")

A31 = pivot_longer(A30, cols = c(`20152016`,`20162017`,`20172018`, `20182019`, `20192020`, `20202021`), names_to = 'year', values_to = "attendance")


########################################################
### This section is where the dashboard (ui) is made ###
########################################################

## Header content
header =  dashboardHeader(title = "NE ACT Dashboard")

## Sidebar content
sidebar =  dashboardSidebar(
   sidebarMenu(
      menuItem("ACT Dataset", tabName = "act", icon = icon("th")),
      menuItem("ACT and Per Pupil Expenditure", tabName = "ppe", icon = icon("th")),
      menuItem("ACT and Attendance", tabName = "attendance", icon = icon("dashboard")),
      menuItem("ACT and Deomgraphics", tabName = "demographics", icon = icon("dashboard"))
   )
)

## Body content
body =  dashboardBody(
   tabItems(
      #############
      ## ACT TAB ##
      #############
      tabItem(tabName = "act",h2("ACT Dataset"),
      fluidRow(
        tabBox(title="ACT Dataset",id="act_map", width=10,
               #############
               ### TAB 1 ###
               #############
               tabPanel("ACT by County",
                  sidebarLayout(
                     sidebarPanel (
                        selectInput("myYears0",
                           label = "School Year",
                           choices = c("2017-2018" = 20172018,
                                       "2018-2019" = 20182019,
                                       "2020-2021" = 20202021,
                                       "2021-2022" = 20212022),
                           selected = 20212022)),
                     mainPanel(plotOutput("ACTmap"))
                  )
               ),
               #############
               ### TAB 2 ###
               #############
               tabPanel("ACT Timeseries",
                  sidebarLayout(
                     sidebarPanel (
                        selectizeInput(
                           inputId = "counties", 
                           label = "Select a county", 
                           choices = unique(act_all_NARM$county), 
                           selected = "douglas",
                           multiple = TRUE)),
                     mainPanel(plotlyOutput("p"))
                     )
                  )
               )
            )
         ),
      #############
      ## PPE TAB ##
      #############
      tabItem(tabName = "ppe",h2("ACT and Per Pupil Expenditure"),
         fluidRow(
            tabBox(title="ACT vs Per Pupil Expenditure",id="act_ppe_cor", width=10,
               #############
               ### TAB 1 ###
               #############
               tabPanel("Correlation",
                  sidebarLayout(
                     sidebarPanel (
                        selectInput("myYears1",
                                    label = "School Year",
                                    choices = c("2018-2019" = 20182019,"2020-2021" = 20202021),
                                    selected = 20202021)),
                     mainPanel(plotlyOutput("heat"),plotlyOutput("scatterplot"))
                  )
               ),
               #############
               ### TAB 2 ###
               #############
               tabPanel("Pupil Expenditure Maps",id="act_ppe_map",
                  sidebarLayout(
                     sidebarPanel (
                        selectInput("selectedExpenditure", 
                           label = "Choose an expenditure to display",
                           choices = c("Adminstration" = "adm",
                                       "Salaries" = "salaries",
                                       "Benefits" = "benefits",
                                       "Federal $" = "federal",
                                       "State and Local $" = "state.local",
                                       "3% of Building and Contents" = "x3..building...contents",
                                       "Total Cost Per Pupil" = "per.pupil.cost"), 
                           selected = "per.pupil.cost"),
                        selectInput("myYears",
                           label = "School Year",
                           choices = c("2018-2019" = 20182019,"2020-2021" = 20202021),
                           selected = 20202021)),
                     mainPanel(plotOutput("myMap"))
                     )
                  ),
               #############
               ### TAB 3 ###
               #############
               tabPanel("ACT vs PPE Animation",id="act_ppe_ani",
                  mainPanel(plotlyOutput("scatterplot_ani"))
                     )
                  )
               )
            ),
         

      ####################
      ## ATTENDANCE TAB ##
      ####################
      tabItem(tabName = "attendance", h2("Attendance Tab Content"),
            fluidRow(
               tabBox(title="Attendance",id="attendance_tab",width=10,
                  #############
                  ### TAB 1 ###
                  #############
                  tabPanel("By Year",
                     sidebarLayout(
                        sidebarPanel(
                           selectInput("year_att",
                                       label="Select Year",
                                       choices = c("2015-2016" = "20152016", 
                                                   "2016-2017" = "20162017", 
                                                   "2017-2018" = "20172018", 
                                                   "2018-2019" = "20182019", 
                                                   "2019-2020" = "20192020", 
                                                   "2020-2021" = "20202021"),
                                       selected = "20152016")
                        ),
                        mainPanel(
                           plotOutput("plott_att")
                        )
                     )
                  ),
                  #############
                  ### TAB 2 ###
                  #############
                  tabPanel("By District",
                     sidebarLayout(
                        sidebarPanel(
                           selectInput("district_att",
                              label = "Select District",
                              choices = unique(A31$district),
                              selected = "1")),
                           mainPanel(plotOutput("plott2_att"))
                           )
                        )
                  )
            )
      ),
      ######################
      ## DEMOGRAPHICS TAB ##
      ######################
      tabItem(tabName = "demographics", h2("Relation between ACT Scores and Demographics"),
              fluidRow(
                 tabBox(title="Demographics",id="demographics_tab", width=10,
                        #############
                        ### TAB 1 ###
                        #############
                        tabPanel("Gender",id="gender_tab",
                                 sidebarLayout(
                                    sidebarPanel (
                                       selectInput("var",
                                                   label="Choose gender",
                                                   choices = c("Male","Female"),
                                                   selected="Male"),
                                       selectInput("year",
                                                   label="Choose year",
                                                   choices=c("2016-2017"="2016-2017",
                                                             "2017-2018"="2017-2018",
                                                             "2018-2019"="2018-2019",
                                                             "2020-2021"="2020-2021"),
                                                   selected = "2016-2017")),
                                    
                                    mainPanel(plotOutput("plott"))
                                 )),
                        #############
                        ### TAB 2 ###
                        #############
                        tabPanel("Ethinicity",id="ethnic_tab",
                                 sidebarLayout(
                                    sidebarPanel (
                                       selectInput("ethinic",
                                                   label="Choose Ethinicity",
                                                   choices = c("American Indian or Alaska Native","Asian","Black or African American",
                                                               "Hispanic","Native Hawaiian or Other Pacific Islander",
                                                               "White","Two Or More Races"),
                                                   selected="American Indian or Alaska Native"),
                                       selectInput("year1",
                                                   label="Choose year",
                                                   choices=c("2016-2017"="2016-2017",
                                                             "2017-2018"="2017-2018",
                                                             "2018-2019"="2018-2019",
                                                             "2020-2021"="2020-2021"),
                                                   selected = "2016-2017")),
                                    mainPanel (
                                       
                                       plotOutput("plot2"))
                                 )
                        ),
                        #############
                        ### TAB 3 ###
                        #############     
                        tabPanel("Summary",id="summary_tab",
                                 sidebarLayout(
                                    sidebarPanel(
                                       selectInput("year2",
                                                   label="Choose Year",
                                                   choices=c("2016-2017"="2016-2017",
                                                             "2017-2018"="2017-2018",
                                                             "2018-2019"="2018-2019",
                                                             "2020-2021"="2020-2021"),
                                                   selected="2016-2017"),
                                       selectInput("inputchoice",
                                                   label="Choose gender/ethinicity",
                                                   choices=c("Gender","Ethinicity"),
                                                   selected="Gender")),
                                    mainPanel (
                                       plotOutput("plot3"))
                                 )
                        ),
                        #############
                        ### TAB 4 ###
                        #############
                        tabPanel("Summary By Year",id="summaryyear_tab",
                                 sidebarLayout(
                                    sidebarPanel(
                                       selectInput("inputchoice_year",
                                                   label="Choose gender/ethinicity",
                                                   choices=c("Gender","Ethinicity"),
                                                   selected="Gender")),
                                    mainPanel (
                                       plotOutput("plot4"))
                                 )
                        )
                        
                 )
              )
      )
      ########################
      ### END DEMOGRAPHICS ###
      ########################
   )
)

#########################################
####### start SERVER section here #######
#########################################

shinyServer= function(input, output) {
   
   
   #############
   ### TAB 1 ###   
   # Plotting the Nebraska PPE map
   dataInput0 <- reactive({
      subset(act_all_NARM,
             datayears==input$myYears0 &
                level == "SC")
   })
   
   output$ACTmap <- renderPlot({ 
      combdat <- merge(mdat, dataInput0(), by.x=c('subregion'), 
                       by.y=c('county'), all.x=TRUE)
      odat <- combdat[order(combdat$order),]
      ggplot(odat, aes(x=long, y=lat,group=group)) +
         geom_polygon(aes(fill=average_composite_score), colour = alpha("white", 0.2)) + 
         theme_bw() + scale_fill_continuous(low="blue", high="pink") +
         theme(legend.position = "right",
               axis.text.x=element_blank(), #remove x axis labels
               axis.ticks.x=element_blank(), #remove x axis ticks
               axis.text.y=element_blank(),  #remove y axis labels
               axis.ticks.y=element_blank()  #remove y axis ticks
         ) 
   })
   
   #############
   ### TAB 2 ###
   output$p <- renderPlotly({
      plot_ly(act_county_mean, x = ~datayears, y = ~county_average_score, color = ~county) %>%
         filter(county %in% input$counties) %>%
         group_by(county) %>%
         add_lines()
   })
   
   
   ################
   ## PPE SERVER ##
   ################
   
   #############
   ### TAB 1 ###
   # setting the reactive environment 
   dataInput1 = reactive({act_ppe_NARM %>%
      filter(datayears==input$myYears1) %>%
      select(c(5,6,7,8,9,10,11,14))
   })
   
   output$heat <- renderPlotly({
      plot_ly(source="heat_plot") %>%
         add_heatmap(
            x = names(dataInput1()),
            y = names(dataInput1()),
            z = correlation
         )
   })
   
   output$scatterplot <- renderPlotly({
      # if there is no click data, render nothing!
      clickData <- event_data("plotly_click", source = "heat_plot")
      if (is.null(clickData)) return(NULL)
      
      # Obtain the clicked x/y variables and fit linear model
      vars <- c(clickData[["x"]], clickData[["y"]])
      d <- setNames(dataInput1()[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      
      # scatterplot with fitted line
      plot_ly(d, x = ~x) %>%
         add_markers(y = ~y) %>%
         add_lines(y = ~yhat) %>%
         layout(
            xaxis = list(title = clickData[["x"]]), 
            yaxis = list(title = clickData[["y"]]), 
            showlegend = FALSE
         )
   })
   
   
   
   #############
   ### TAB 2 ###   
   # Plotting the Nebraska PPE map
   dataInput2 <- reactive({
      subset(act_ppe_select,
             datayears==input$myYears & 
                expenditure==input$selectedExpenditure &
                type == "SC")
   })
   
   output$myMap <- renderPlot({ 
      combdat <- merge(mdat, dataInput2(), by.x=c('subregion'), 
                       by.y=c('county'), all.x=TRUE)
      odat <- combdat[order(combdat$order),]
      ggplot(odat, aes(x=long, y=lat,group=group)) +
         geom_polygon(aes(fill=value), colour = alpha("white", 0.2)) + 
         theme_bw() + scale_fill_continuous(low="blue", high="pink") +
         theme(legend.position = "right",
               axis.text.x=element_blank(), #remove x axis labels
               axis.ticks.x=element_blank(), #remove x axis ticks
               axis.text.y=element_blank(),  #remove y axis labels
               axis.ticks.y=element_blank()  #remove y axis ticks
               ) 
   })
   
   #############
   ### TAB 3 ###   
   # Plotting the Animation
   
   
   output$scatterplot_ani <- renderPlotly({ 
         anim = act_ppe_NARM %>%
            ggplot(aes(x= per.pupil.cost, y= average_composite_score, frame= datayears)) + 
            geom_point(alpha= 0.5, aes(color= factor(district), size= salaries)) +
            labs(x= "Per Pupil Expenditure ($)", y= "ACT Score", title= "Scatter Plot of ACT Composite Score and PPE for the 2018-2019 and 2020-2021 Academic School Years") +
            scale_x_log10() +
            scale_y_log10() +
            geom_smooth(method= lm, se= FALSE) + 
            theme(legend.position= "none")

      ggplotly(anim + aes(label= name))
   })
   
   #########################
   ## DEMOGRAPHICS SERVER ##
   ######################### 
   
   #############
   ### TAB 1 ###
   ### Subset gender data based on user input for TAB 1
   tab=reactive({ genderbased%>%
         filter(SUBGROUP_DESCRIPTION %in% input$var) %>% 
         filter(Datayears == input$year) 
   })
   ### render plots for TAB 1 based on plot selected
   output$plott=renderPlot({
      
      ggplot(tab(),aes(x=SUBJECT,y=AVERAGE_SCALE_SCORE,fill=SUBJECT))+
         geom_bar(stat="identity")+labs(y="Average ACT scores")+
         geom_text(aes(label=AVERAGE_SCALE_SCORE),hjust=1, vjust=0)+ggtitle(paste("Barplot for average ACT scores for each subject based on",tab()$SUBGROUP_DESCRIPTION,"for Year",tab()$Datayear))
   })
   
   #############
   ### TAB 2 ###
   ### subset race data based on user input for TAB 2
   tab2=reactive({racebased%>%
         filter(SUBGROUP_DESCRIPTION %in% input$ethinic) %>% 
         filter(Datayears == input$year1) 
   })
   ### render plots for TAB 2 based on plot selected
   output$plot2=renderPlot({
      ggplot(tab2(),aes(x=SUBJECT,y=AVERAGE_SCALE_SCORE,fill=SUBJECT))+
         geom_bar(stat="identity")+labs(y="Average ACT scores")+
         scale_y_continuous(limits = c(0,22))+
         geom_text(aes(label=AVERAGE_SCALE_SCORE),hjust=1, vjust=0)+ggtitle(paste("Barplot for average ACT scores for each subject based on",tab2()$SUBGROUP_DESCRIPTION))
   })
   
   #############
   ### TAB 3 ###
   ### subset race data based on user input for TAB 3
   tab3=reactive({ racebased%>%
         filter(Datayears==input$year2)
   })
   
   
   ### subset gender data based on user input for TAB 3
   tab4=reactive({ genderbased %>%
         filter(Datayears==input$year2)
   })
   
   ### render plots for TAB 3 based on plot selected
   output$plot3=renderPlot({
      if(input$inputchoice=="Ethinicity"){
         o=ggplot(tab3(),aes(x=factor(SUBGROUP_DESCRIPTION),y=AVERAGE_SCALE_SCORE,fill=factor(SUBGROUP_DESCRIPTION)))
         o+geom_boxplot(position=position_dodge())+coord_flip()+
            
            ggtitle(paste("Boxplots for different Ethinic groups for Year ",tab3()$Datayears))+
            labs(x="Average ACT scores",y="Ethinicity",fill = "Ethinicity")+ 
            theme(legend.position="bottom")
         
      }
      else{
         
         p=ggplot(tab4(),aes(y=factor(SUBGROUP_DESCRIPTION),x=AVERAGE_SCALE_SCORE,fill=factor(SUBGROUP_DESCRIPTION)))
         p+geom_boxplot(position=position_dodge())+
            
            labs(x="Average ACT scores",y="Gender",fill = "Gender")+ggtitle(paste("Boxplot based on Gender for Year",tab4()$Datayears))
         
         
      }
   })   
   #############
   ### TAB 4 ###
   ### subset race data based on user input for TAB 4
   output$plot4=renderPlot({
      if(input$inputchoice_year=="Gender"){
         
         o=ggplot(genderbased,aes(x=factor(Datayears),y=AVERAGE_SCALE_SCORE,fill=factor(Datayears)))
         o+geom_boxplot(position=position_dodge())+
            facet_wrap(~SUBGROUP_DESCRIPTION)+
            labs(x="Academic year",fill = "Years")+
            ggtitle("Boxplot depicting Average ACT scores for multiple school years faceted by gender")
         
      }
      else{
         o=ggplot(racebased,aes(x=factor(Datayears),y=AVERAGE_SCALE_SCORE,fill=factor(Datayears)))
         o+geom_boxplot(position=position_dodge())+facet_wrap(~SUBGROUP_DESCRIPTION)+coord_flip()+
            labs(x="Average ACT scores",y="Academic year",fill = "Years")+
            ggtitle("Boxplot depicting Average ACT scores for multiple school years faceted by ethnicity")
      }
      
   })
   
   #######################
   ## ATTENDANCE SERVER ##
   #######################
   
   tab_att=reactive({A31 %>%
         filter(year == input$year_att)
   })
   tab2_att=reactive({A31 %>%
         filter(district == input$district_att)
   })
   
   output$plott_att=renderPlot({
      ggplot(tab_att(),aes(attendance)) +
         geom_histogram(bins = 108) + xlim(0.85, 1) 
   })
   
   output$plott2_att=renderPlot({
      ggplot(tab2_att(), aes(x=year, y=attendance)) +
         geom_point() + geom_line() + ylim(.85, 1) 
   })
}

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui, shinyServer)