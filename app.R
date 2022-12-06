#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
train_df<-read.csv("train.csv")
stores_df<-read.csv("stores.csv")
feat_df<-read.csv("features.csv")
chris<-train_df[train_df$Date %in% c("2010-12-31","2011-12-30","2012-12-28"),]
bowl<-train_df[train_df$Date %in% c("2010-02-12","2011-02-11","2012-02-10"),]
labor<-train_df[train_df$Date %in% c("2010-09-10","2011-09-09","2012-09-07"),]
thank<-train_df[train_df$Date %in% c("2010-11-26","2011-11-25","2012-11-23"),]
chris<-chris%>%group_by(across("Date"))%>%summarise(sales=mean(Weekly_Sales))
bowl<-bowl%>%group_by(across("Date"))%>%summarise(sales=mean(Weekly_Sales))
labor<-labor%>%group_by(across("Date"))%>%summarise(sales=mean(Weekly_Sales))
thank<-thank%>%group_by(across("Date"))%>%summarise(sales=mean(Weekly_Sales))
chris$year<-format(as.Date(chris$Date),format="%Y")
bowl$year<-format(as.Date(bowl$Date),format="%Y")
labor$year<-format(as.Date(labor$Date),format="%Y")
thank$year<-format(as.Date(thank$Date),format="%Y")
n1=merge(labor,bowl,by='year')
n2=merge(chris,thank,by='year')

h=merge(n1,n2,by='year',all.x=TRUE)
colnames(h)<-c("year","datel","salesl","dateb","salesb","datec","salesc","datet","salest")
tt<-train_df%>%group_by(Dept)%>%
  summarise(total_count=n(),sales=mean(Weekly_Sales),.groups = 'drop')
feat_df$Year<-format(as.Date(feat_df$Date),format="%Y")
feat_df<-feat_df%>%rowwise()%>%mutate(markdown = sum(MarkDown1,MarkDown2,MarkDown3,MarkDown4,MarkDown5, na.rm=TRUE))
feat_df<-select(feat_df,-MarkDown1,-MarkDown2,-MarkDown3,-MarkDown4,-MarkDown5)
feat_df$IsHoliday<-ifelse(feat_df$IsHoliday==TRUE,1,0)
train_df$IsHoliday<-ifelse(train_df$IsHoliday==TRUE,1,0)
feat_df<-transform(feat_df,Year=as.numeric(Year))
tshr=train_df[seq(1,nrow(train_df),143),]
fs<-merge(tshr,feat_df,all=FALSE)
fst<-merge(fs,stores_df)
sf<-merge(feat_df,stores_df)
ui <- dashboardPage(skin='purple',
                    dashboardHeader(title="Walmart store sales analysis"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "Intro"),
                        menuItem("Univariate Analysis", tabName = "uni"),
                        menuItem("Multivariate Analysis",tabName="multi"),
                        menuItem("Year wise Analysis",tabName="year"),
                        menuItem("Holiday Analysis",tabName="hol"),
                        menuItem("Store Types Analysis",tabName="type")) ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Intro",
                                fluidRow(h2("Walmart store sales analysis"),align='center'),
                                fluidRow(h4("Sales analysis is an important aspect of running a store as we need to understand the factors that influence
sales if we want to increase profit and customer satisfaction. It provides insights into the past, present, and
future performance and can be used to help forecast trends, identify opportunities for growth, and develop
a strategic action plan for the stores. The aim of this project is to analyze the sales of Walmart stores to
find how different features impact the sales in a Walmart supermarket using exploratory data analysis and
R visualizations.We use a dataset containing sales data for 45 Walmart stores located in different regions."),imageOutput("im"),align='center')
                                
                                ),
                        
                        tabItem(tabName = "uni",h2("Univariate Analysis"),
                                fluidRow( box(background = "black",selectInput("var", label = "Choose a variable to display",
                                                                               choices = c("IsHoliday","Weekly_Sales","Dept","Store","Type","Fuel_Price","Temperature","Size"
                                                                                           ,"markdown","Year","CPI","Unemployment"),selected = "IsHoliday")),uiOutput("bvp"),align='center'),
                                actionButton("show_plot","Plot"),
                                fluidRow(plotOutput("selected_var")),align='center'),
                        tabItem(tabName = "multi", h3("Multivariate analysis"),
                                fluidRow( box(background = "black",selectInput("var1", label = "Choose variable 1",
                                                                               choices = c("IsHoliday","Weekly_Sales","Dept","Store","Type","Fuel_Price","Temperature","Size",
                                                                                                                                                     "markdown","Year","CPI","Unemployment"),selected = "IsHoliday")),uiOutput('mva'),align='center'),
                                actionButton("show_plot1","Plot"), 
                                fluidRow(plotOutput("mvp")),align='center'),
                                
                        tabItem(tabName='year',h3("Year wise analysis"),
                                fluidRow( box(background = "black",selectInput("vary", label = "Choose variable 1",
                                                                               choices = c("IsHoliday","Weekly_Sales","Dept","Store","Type","Fuel_Price","Temperature","Size"
                                                                                                                                             ,"markdown","CPI","Unemployment"),selected = "IsHoliday")),uiOutput("bvpy"),align='center'),
                                actionButton("show_ploty","Plot"), 
                                fluidRow(plotOutput("yw")),align='center'),
                        tabItem(tabName = "hol",h3("Holiday analysis"),
                                tabsetPanel(
                                  tabPanel("IsHoliday",
                                fluidRow( box(background = "black",selectInput("varh", label = "Choose variable 1",
                                                                               choices = c("Weekly_Sales","Dept","Store","Year","Fuel_Price","Temperature","Size","Type","CPI","Unemployment"
                                                                                           ),selected = "Weekly_Sales")),uiOutput("bvph"),align='center'),
                                actionButton("show_ploth","Plot"), 
                                fluidRow(plotOutput("hw")),align='center'),
                                
                                tabPanel("Holidays",
                                    #fluidRow(align='center',box(align='center',background = 'black',checkboxGroupInput("holi",label="Choose holidays",c("Christmas  "="Christmas","Labor day   "="Labor day","Super bowl  "="Super bowl","Thanks giving"="Thanksgiving")),align='center')),
                                    column(1,align='center',checkboxGroupInput("holi",label="Choose Holidays",c("Christmas  "="Christmas","Labor day   "="Labor day","Super bowl  "="Super bowl","Thanks giving"="Thanksgiving"))),
                                    fluidRow(plotOutput("hols")),align='center'         
                                ))),
                        tabItem(tabName='type',h3("Store type analysis"),
                                fluidRow( box(background = "black",selectInput("vart", label = "Choose variable 1",
                                                                               choices = c("IsHoliday","Weekly_Sales","Dept","Year","Fuel_Price","Temperature"
                                                                                           ,"markdown","CPI","Unemployment"),selected = "IsHoliday")),uiOutput("bvpt"),align='center'),
                                actionButton("show_plott","Plot"), 
                                fluidRow(plotOutput("tw")),align='center'))))



server <- function(input,output,session) {
  color<-c("purple","pink","cyan","green","yellow","#0033FF","#CC0033","#FFCC66","#99CC00",'#FF0033','#99FF00','#990033','#CCFF66','#FF66FF','#0033CC','#00CC33',"purple","pink","cyan","green","yellow","#0033FF","#CC0033","#FFCC66","#99CC00",'#FF0033','#99FF00','#990033','#CCFF66','#FF66FF','#0033CC','#00CC33',"purple","pink","cyan","green","yellow","#0033FF","#CC0033","#FFCC66","#99CC00",'#FF0033','#99FF00','#990033','#CCFF66','#FF66FF','#0033CC','#00CC33',"purple","pink","cyan","green","yellow","#0033FF","#CC0033","#FFCC66","#99CC00",'#FF0033','#99FF00','#990033','#CCFF66','#FF66FF','#0033CC','#00CC33',"purple","pink","cyan","green","yellow","#0033FF","#CC0033","#FFCC66","#99CC00",'#FF0033','#99FF00','#990033','#CCFF66','#FF66FF','#0033CC','#00CC33')
  output$im<-renderImage({list(src="walmart.png")})
               output$bvp<-renderUI(if(input$var %in% c("IsHoliday","Type","Year")){
    fluidRow(box(background='black',selectInput("vari",label="Choose a graph to display",choices=c("bar","pie"),selected='bar')))}
    else if(input$var %in% c("Dept","Store")){
      fluidRow(box(background='black',selectInput("vari",label="Bar plot",choices=c("bar"),selected='bar')))
    }
    else{fluidRow(box(background='black',selectInput("vari",label="Histogram",choices=c("Histogram"),selected='Hist')))})
               labl<-function(stri){
  
                 if (stri== 'IsHoliday'){ return('Holiday')}
                 if (stri== 'Dept'){return('Department')}
                 if (stri== 'Type'){return('Store Type')}
                 if (stri== "Fuel_Price"){return('Fuel price (US dollars)')}
                 if (stri== 'Weekly_Sales'){return('Weekly sales (US dollars)')}
                 if (stri=='markdown'){return('markdown (US dollars)')}
                 if (stri== 'Temperature'){return('Temperature (F)')}
                 if (stri== 'CPI'){return('Consumer Price Index')}
                 if (stri== 'Unemployment'){return("Unemployment rate")}
                 if (stri== 'Size'){return("Store size")}
                 if (stri== 'Holiday'){return('IsHoliday')}
                 if (stri== 'Department'){return('Dept')}
                 if (stri == 'Store Type'){return('Type')}
                 if (stri== 'Fuel price (US dollars)'){return('Fuel_Price')}
                 if (stri== 'Weekly sales (US dollars)'){return('Weekly_Sales')}
                 if (stri== 'markdown (US dollars)'){return('markdown')}
                 if (stri== 'emperature (F)'){return("Temperature")}
                 if (stri== 'Consumer Price Index'){return("CPI")}
                 if (stri== "Unemployment rate"){return("Unemployment")}
                 if (stri== "Store Size"){return('Size')}
                 return(stri)
               }
  output$selected_var<-renderPlot({
  gp<-fst%>%group_by(across(input$var))%>%summarise(total_count=n(),.groups = 'drop')
  #gp$`input$var`=as.character(gp$`input$var`)
  if(input$var=='IsHoliday'){gp$IsHoliday=as.character(gp$IsHoliday)}
  if(input$var=='Dept'){gp$Dept=as.character(gp$Dept)}
  if(input$var=="Store"){gp$Store=as.character(gp$Store)}
  if(input$var=="Type"){gp$Type=as.character(gp$Type)}
  if(input$var=="Year"){gp$Year=as.character(gp$Year)}
  if (as.character(input$vari)=="bar"){return(ggplot(gp,aes_string(y='total_count',x=input$var,fill=input$var))+geom_bar(stat='identity')+ggtitle(paste('Bar plot of count of',labl(input$var)))+ xlab(label=labl(input$var))+ylab(label='Total Count'))+geom_text(aes_string(label='total_count'))}
  else if(as.character(input$vari)=="pie"){
    gp <- gp%>% 
      mutate(perc = total_count / sum(total_count)) %>% 
      arrange(perc) %>%
      mutate(labels = scales::percent(perc))
  
   return( ggplot(gp,aes_string(y='total_count',x="''",fill=input$var))+geom_bar(stat='identity')+coord_polar('y')+ggtitle(paste('Pie plot of count of',labl(input$var)))+geom_text(aes(label = labels),
                                                                                                                                                                                    position = position_stack(vjust = 0.5))+scale_fill_manual(values=color))}
  else if(as.character(input$vari)=="Histogram"){return(ggplot(fst,aes_string(x=input$var))+geom_histogram(fill='blue',col='white')+scale_x_log10()+ggtitle(paste('Histogram of count of',input$var)))}
  },width=600)%>%bindEvent(input$show_plot)
  
  
  mv1<-reactive(as.character(input$var1))
  output$mva<-renderUI(if(mv1() %in% c("IsHoliday","markdown") ){
    fluidRow(box(background='black',selectInput("mv2",label="Choose variable 2",choices=c("Weekly_Sales","Fuel_Price","Temperature","CPI","Unemployment","Type","Size","markdown","Year"),selected = "Type")))}
    else if(mv1()=='Dept'){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("Weekly_Sales","Type","Date",'markdown'),selected="IsHoliday")))}
    else if(mv1()=="Store"){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("Weekly_Sales","Size","Type","markdown"),selected="Size")))}
    else if(mv1()=="Weekly_Sales"){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("IsHoliday","Dept","Store","Fuel_Price","Temperature","CPI","Unemployment","Size","Type","markdown",'Year','Date'),selected="IsHoliday")))}
    else if(mv1() %in% c("Fuel_Price","Temperature")){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales",'Year','Date',"Type","markdown"),selected="IsHoliday")))}
    else if( mv1()=="Type"){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c('IsHoliday',"Year",'Store',"Weekly_Sales","markdown",'Date'),selected="Year")))}
    else if(mv1()=="Size" ){
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("Weekly_Sales","markdown",'IsHoliday','Store'),selected="IsHoliday")))}
    else{
      fluidRow(box(background='black',selectInput("mv2",label="Choose second variable",choices=c("Weekly_Sales","Store","Fuel_Price","Temperature","CPI","Unemployment","Size",'Type'),selected="Type")))}
  )
  
  output$mvp<-renderPlot({f<-sf
  mv2=as.character(input$mv2)
  
  if((mv1() %in% c("Weekly_Sales","Dept"))|(mv2 %in% c("Weekly_Sales","Dept"))){f<-fst}
  f$Type<-as.character(f$Type)
  if((mv1()=='markdown')|(mv2=='markdown')){f<-f[which(f$markdown>0),]}
  else if(mv1()=='IsHoliday'  & mv2=='Year'){f<-transform(f,Year<-as.character(Year))
  ggplot(f,aes(y=IsHoliday,x=Year,fill=Year))+geom_bar(stat='identity')+ggtitle("Bar plot between IsHoliday and Year")+scale_fill_continuous(high='purple')}
  else if(mv1()=='IsHoliday'  & mv2=='Type'){
    ggplot(f,aes(x=Type,y=IsHoliday,fill=Type))+geom_bar(stat='identity')+ggtitle("Bar plot between IsHoliday and Store Type")+coord_flip()}
  else if((mv1()=='Year'  & mv2=='Type')|(mv1()=='Type' & mv2=='Year')){
    ggplot(f,aes(y=Year,x=Type,fill=Type))+geom_bar(stat='identity')+ggtitle("Bar plot between Year and Store Type")+coord_flip()}
  
  else if((mv1() %in% c("IsHoliday","Hol type","Type","Year")) & (mv2 %in% c("IsHoliday","Hol type","Type","Year") ))
  {
    ggplot(f,aes_string(x=mv1(),y=mv2,fill=mv1()))+geom_bar(stat='identity')+ggtitle(paste("Bar plot between",labl(mv2),"and",labl(mv1())))}
  else if((mv1() %in% c("IsHoliday","Hol type","Type","Year")) & (mv2 %in% c("Dept","Store") ))
  {ggplot(f,aes_string(x=mv2,y=mv1()))+geom_bar(stat='identity',color='purple')+ggtitle(paste("Bar plot between",labl(mv1()),"and",labl(mv2)))}
  else if((mv1() %in% c("IsHoliday","Hol type","Type","Year")) & (mv2!="Date" ))
  {
    ggplot(f,aes_string(x=mv1(),y=mv2,fill=mv1()))+geom_bar(stat='identity',show.legend = F)+ggtitle(paste("Bar plot between",labl(mv2),"and",labl(mv1())))+scale_fill_continuous(low="purple", high="pink")}
  else if((mv1() %in% c('Weekly_Sales','Temperature','Fuel_Price',"CPI","Unemployment","Size")) & (mv2 %in% c("IsHoliday","Year") ))
  {f$IsHoliday<-as.character(f$IsHoliday)
   f$Year<-as.character(f$Year)
    ggplot(f,aes_string(x=mv2,y=mv1(),fill=mv2))+geom_bar(stat='identity')+ggtitle(paste("Bar plot between",labl(mv2),"and",labl(mv1())))}
  else if((mv1() %in% c("Store","Size")) & (mv2 %in% c("Size","Store") ))
  {ggplot(f,aes_string(x="Store",y="Size"))+geom_bar(stat='identity',color='purple')+ggtitle(paste('Bar plot between',labl(mv2),"count of",labl(mv1())))}
  else if (mv2 %in% c("IsHoliday","Hol type","Type","Year"))
  {
    ggplot(f,aes_string(x=mv1(),y=mv2,fill=mv2))+geom_bar(stat='identity')+ggtitle(paste("Bar plot between",labl(mv2),"and",labl(mv1())))}
  else if(mv2 =="Date"){
    f<-select(f,-Type)
    if(mv1()!='IsHoliday'){f<-f%>%group_by(Date)%>%summarise_all('mean')
    }

    ggplot(f,aes_string(x=mv2,y=mv1()))+geom_line()+geom_point(color='blue')+ggtitle(paste("Scatter plot between",labl(mv1()),"and",labl(mv2)))}
  
  else {ggplot(f,aes_string(x=mv2,y=mv1()))+geom_point(stat='identity',color='purple')+ggtitle(paste('Scatter plot between',labl(mv1()),"and",labl(mv2)))}
  },width=600)%>%bindEvent(input$show_plot1)
  
  bv1<-reactive(as.character(input$vary))
  output$bvpy<-renderUI(if(bv1() %in% c("IsHoliday","markdown") ){
    fluidRow(box(background='black',selectInput("variy",label="Choose variable 2",choices=c("Dept","Weekly_Sales","CPI","Unemployment","Fuel_Price","Temperature","Type"),selected = "Type")))}
    else if(bv1()=='Dept'){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales","Type"),selected="IsHoliday")))}
    else if(bv1()=="Store"){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("Weekly_Sales","markdown"),selected="Weekly_Sales")))}
    else if(bv1()=="Weekly_Sales"){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("IsHoliday","Dept","Store","Fuel_Price","CPI","Unemployment","Temperature","Size","Type","markdown"),selected="IsHoliday")))}
    else if(bv1() %in% c("Fuel_Price","Temperature","CPI","Unemployment")){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales","markdown"),selected="IsHoliday")))}
    else if( bv1()=="Type"){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales","markdown"),selected="IsHoliday")))}
    else if(bv1()=="Size" ){
      fluidRow(box(background='black',selectInput("variy",label="Choose second variable",choices=c("Weekly_Sales","markdown"),selected="IsHoliday")))}
    else{
      selectInput("variy",label="Choose second variable",choices=c("Weekly_Sales","Store","Fuel_Price","Temperature","CPI","Unemployment","Size",'Type'),selected="Type")}
  )
  
  output$yw<-renderPlot({f<-sf
  f<-transform(f,Year=as.character(Year))
  variy=as.character(input$variy)
  if(bv1() %in% c("Dept","Weekly_Sales") | variy %in% c("Dept","Weekly_Sales")){f<-fst
  f<-transform(f,Year=as.character(Year))}
  if(bv1() %in% c("Type",'IsHoliday') & variy %in% c("Fuel_Price","Temperature","CPI","Unemployment","Size","Weekly_Sales","markdown")){
    ggplot(f,aes_string(x=bv1(),y=variy,fill="Year"))+geom_bar(stat='identity',position = 'dodge')+ggtitle(paste("Year-wise Bar plot between",labl(variy),"and",labl(bv1())))}
  else if(bv1() %in% c("IsHoliday","Type") & variy %in% c("Type","IsHoliday")){
    ggplot(f,aes_string(y=bv1(),x=variy,fill="Year"))+geom_bar(stat='identity')+ggtitle(paste("Year-wise Bar plot between",labl(variy),"and",labl(bv1()))) }
  else if(bv1() %in% c("Dept","Type") & variy %in% c("Type","Dept")){
    ggplot(f,aes_string(y="Dept",x="Type",fill="Year"))+geom_bar(stat='identity',position='dodge') }
  else if(bv1() %in% c('Type',"Weekly_Sales") & variy %in% c('Weekly_Sales',"Type")){
    ggplot(f,aes_string(x='Type',y="Weekly_Sales",fill="Year"))+geom_bar(stat="identity",position="dodge")}
  else if(bv1() %in% c('Size',"Fuel_Price","markdown","Temperature","CPI","Unemployment","Weekly_Sales") & variy %in% c('Type',"IsHoliday")){
    ggplot(f,aes_string(x=variy,y=bv1(),fill="Year"))+geom_bar(stat="identity",position="dodge")+ggtitle(paste("Year-wise Bar plot between",labl(bv1()),"and",labl(variy)))}
  else if((bv1()=="IsHoliday" & variy=="Dept")|(bv1()=="Dept" & variy=="IsHoliday")){
    ggplot(f,aes_string(y="IsHoliday",x="Dept",fill="Year"))+geom_bar(stat="identity")+ggtitle("Year-wise Bar plot between IsHoliday and Department")}
  else if((bv1() %in% c("Dept","Store") )){
    ggplot(f,aes_string(x=bv1(),y=variy,color="Year"))+geom_point(stat='identity')}
  else if((variy %in% c("Dept","Store") )|((bv1() %in% c("Weekly_Sales","Fuel_Price","Temperature","CPI","Unemployment","Size","markdown")) |(variy %in% c("Weekly_Sales","Fuel_Price","Temperature","CPI","Unemployment","Size","markdown")))){
    ggplot(f,aes_string(y=bv1(),x=variy,color="Year"))+geom_point(stat='identity')}
  else if((bv1() %in% c("Fuel_Price","Temperature","CPI","Unemployment","Size","markdown")) & (variy %in% c("IsHoliday","Type"))){
    ggplot(f,aes_string(x=variy,y=bv1(),fill="Year"))+geom_bar(stat='identity',position='dodge')+ggtitle(paste("Year-wise Bar plot between",labl(bv1()),"and",labl(variy)))}
  else if(bv1() %in% c("Fuel_Price","Temperature","CPI","Unepmployment","Size","markdown")){
    ggplot(f,aes_string(y=bv1(),x=variy,color="Year"))+geom_point(stat='identity')+ggtitle(paste("Year-wise scatter plot between",labl(bv1()),"and",labl(variy)))}
  
  },width=600)%>%bindEvent(input$show_ploty)
  
  bv2<-reactive(as.character(input$varh))
  output$bvph<-renderUI(if(bv2() == "Dept") {
    fluidRow(box(background='black',selectInput("varih",label="Choose variable 2",choices=c("Weekly_Sales","Type","Size","Year"),selected = "Type")))}
    else if(bv2() =="Store"){
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("Weekly_Sales","markdown"),selected="markdown")))}
    else if(bv2() %in% c("Store","Type","Size")){
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("Weekly_Sales","Fuel_Price","Temperature","CPI",'Unemployment',"markdown","Year"),selected="Year")))}
    else if(bv2()=="Weekly_Sales"){
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("Dept","Store","Size","Type","markdown","Year"),selected="Year")))}
    else if(bv2() %in% c("Fuel_Price","Temperature","CPI","Unepmloyment")){
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("markdown","Year"),selected="Year")))}
    else if(bv2() == "Year"){
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("Dept","Store","Weekly_Sales","Fuel_Price","Temperature","CPI","Unemployment","Type"),selected="Type")))}
    else{
      fluidRow(box(background='black',selectInput("varih",label="Choose second variable",choices=c("Weekly_Sales","Store","Fuel_Price","Temperature","CPI","Unemployment","Size",'Type'),selected="Type")))}
  )
  output$hw<-renderPlot({f<-sf
  f<-transform(f,IsHoliday=as.character(IsHoliday))
  varih=as.character(input$varih)
  if(bv2() %in% c("Dept","Weekly_Sales") | varih %in% c("Dept","Weekly_Sales")){f<-fst
  f<-transform(f,IsHoliday=as.character(IsHoliday))}
  if(bv2() %in% c("Type",'Year') & varih %in% c("Fuel_Price","CPI","Unemployment","Temperature","Size","Weekly_Sales","markdown")){
    ggplot(f,aes_string(x=bv2(),y=varih,fill="IsHoliday"))+geom_bar(stat='identity',position = 'dodge')+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  else if(bv2() %in% c("Year","Type") & varih %in% c("Type","Year")){
    ggplot(f,aes_string(y=bv2(),x=varih,fill="IsHoliday"))+geom_bar(stat='identity')+ggtitle(paste("Holiday wise barplot between",labl(varih),"and",labl(bv2()))) }
  else if(bv2() %in% c("Dept","Type") & varih %in% c("Type","Dept")){
    ggplot(f,aes_string(y="Dept",x="Type",fill="IsHoliday"))+geom_bar(stat='identity',position='dodge') +ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  else if(bv2() %in% c('Type',"Weekly_Sales") & varih %in% c('Weekly_Sales',"Type")){
    ggplot(f,aes_string(x='Type',y="Weekly_Sales",fill="IsHoliday"))+geom_bar(stat="identity",position="dodge")+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  else if(bv2() %in% c('Size',"Fuel_Price","CPI","Unemployment","markdown","Temperature","Weekly_Sales") & varih %in% c('Type',"Year")){
    ggplot(f,aes_string(x=varih,y=bv2(),fill="IsHoliday"))+geom_bar(stat="identity",position="dodge")+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  else if((bv2()=="Year" & varih %in% c("Store"))|(bv2() %in% c("Store") & varih=="Year")){
    ggplot(f,aes_string(y="Year",x="Store",fill="IsHoliday"))+geom_bar(stat="identity")+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  else if((bv2()=="Year" & varih %in% c("Dept"))|(bv2() %in% c("Dept") & varih=="Year")){
    ggplot(f,aes_string(y="Year",x="Dept",fill="IsHoliday"))+geom_bar(stat="identity")+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  
  else if((bv2() %in% c("Dept","Store") )){
    ggplot(f,aes_string(x=bv2(),y=varih,color="IsHoliday"))+geom_point(stat='identity')+ggtitle(paste("Holiday wise scatterplot between",labl(bv2()),"and",labl(varih)))}
  else if((varih %in% c("Dept","Store") )|((bv2() %in% c("Weekly_Sales","Fuel_Price","CPI","Unemployment","Temperature","Size","markdown")) |(varih %in% c("Weekly_Sales","Fuel_Price","CPI","Unemployment","Temperature","Size","markdown")))){
    ggplot(f,aes_string(y=bv2(),x=varih,color="IsHoliday"))+geom_point(stat='identity')+ggtitle(paste("Holiday wise scatterplot between",labl(varih),"and",labl(bv2())))}
  else if((bv2() %in% c("Fuel_Price","CPI","Unemployment","Temperature","Size","markdown")) & (varih %in% c("Year","Type"))){
    ggplot(f,aes_string(x=varih,y=bv2(),fill="IsHoliday"))+geom_bar(stat='identity',position='dodge')+ggtitle(paste("Holiday wise Scatterplot between",labl(bv2()),"and",labl(varih)))}
  else if(bv2() %in% c("Fuel_Price","CPI","Unemployment","Temperature","Size","markdown")){
    ggplot(f,aes_string(y=bv2(),x=varih,color="IsHoliday"))+geom_point(stat='identity')+ggtitle(paste("Holiday wise barplot between",labl(bv2()),"and",labl(varih)))}
  
  },width=600)%>%bindEvent(input$show_ploth)
  
  
  
  
  bv3<-reactive(as.character(input$vart))
  output$bvpt<-renderUI(if(bv3() %in% c("IsHoliday","markdown") ){
    fluidRow(box(background='black',selectInput("varit",label="Choose variable 2",choices=c("Dept","Weekly_Sales","Year"),selected = "Year")))}
    else if(bv3()=='Dept'){
      fluidRow(box(background='black',selectInput("varit",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales"),selected="IsHoliday")))}
    else if(bv3()=="Weekly_Sales"){
      fluidRow(box(background='black',selectInput("varit",label="Choose second variable",choices=c("IsHoliday","Dept","Year","markdown"),selected="IsHoliday")))}
    else if(bv3()=="Year" ){
      fluidRow(box(background='black',selectInput("varit",label="Choose second variable",choices=c("IsHoliday","Weekly_Sales","markdown"),selected="IsHoliday")))}
    else{
      fluidRow(box(background='black',selectInput("varit",label="Choose second variable",choices=c("Weekly_Sales","Year"),selected="Year")))}
  )
  output$tw<-renderPlot({f<-sf
  f<-transform(f,Type=as.character(Type))
  varit=as.character(input$varit)
  if(bv3() %in% c("Dept","Weekly_Sales") | varit %in% c("Dept","Weekly_Sales")){f<-fst
  f<-transform(f,Type=as.character(Type))}
  if(bv3() %in% c("Year",'IsHoliday') & varit %in% c("Fuel_Price","CPI","Unemployment","Temperature","Size","Weekly_Sales","markdown")){
    ggplot(f,aes_string(x=bv3(),y=varit,fill="Type"))+geom_bar(stat='identity',position = 'dodge')+ggtitle(paste("Store type wise bar plot between",labl(varit),"and",labl(bv3())))}
  else if(bv3() %in% c("IsHoliday","Year") & varit %in% c("Year","IsHoliday")){
    ggplot(f,aes_string(y=bv3(),x=varit,fill="Type"))+geom_bar(stat='identity')+ggtitle(paste("Store type wise bar plot between",labl(bv3()),"and",labl(varit))) }
  else if(bv3() %in% c("Dept","Year") & varit %in% c("Year","Dept")){
    ggplot(f,aes_string(y="Dept",x="Year",fill="Type"))+geom_bar(stat='identity',position='dodge') +ggtitle(paste("Store type wise bar plot between",labl(varit),"and",labl(bv3())))}
  else if(bv3() %in% c('Year',"Weekly_Sales") & varit %in% c('Weekly_Sales',"Year")){
    ggplot(f,aes_string(x='Year',y="Weekly_Sales",fill="Type"))+geom_bar(stat="identity",position="dodge")+ggtitle(paste("Store type wise bar plot between",labl(varit),"and",labl(bv3())))}
  else if(bv3() %in% c('Size',"Fuel_Price","CPI","Unemployment","markdown","Temperature","Weekly_Sales") & varit %in% c('Year',"IsHoliday")){
    ggplot(f,aes_string(x=varit ,y=bv3(),fill="Type"))+geom_bar(stat="identity",position="dodge")+ggtitle(paste("Store type wise bar plot between",labl(bv3()),"and",labl(varit)))}
  else if((bv3()=="IsHoliday" & varit=="Dept")|(bv3()=="Dept" & varit=="IsHoliday")){
    ggplot(f,aes_string(y="IsHoliday",x="Dept",fill="Type"))+geom_bar(stat="identity")+ggtitle("Store type wise bar plot between ISHoliday and Department")}
  else if((bv3() %in% c("Dept","Store") )){
    ggplot(f,aes_string(x=bv3(),y=varit,color="Type"))+geom_point(stat='identity')+ggtitle(paste("Store type wise scatter plot between",labl(varit),"and",labl(bv3())))}
  else if((varit %in% c("Dept","Store") )|((bv3() %in% c("Weekly_Sales","Fuel_Price","CPI","Unemploymentt","Temperature","Size","markdown")) |(varit %in% c("Weekly_Sales","Fuel_Price","Temperature","Size","markdown")))){
    ggplot(f,aes_string(y=bv3(),x=varit,color="Type"))+geom_point(stat='identity')+ggtitle(paste("Store type wise scatter plot between",labl(varit),"and",labl(bv3())))}
  else if((bv3() %in% c("Fuel_Price","CPI",'Unemployment',"Temperature","Size","markdown")) & (varit %in% c("IsHoliday","Year"))){
    ggplot(f,aes_string(x=varit,y=bv3(),fill="Type"))+geom_bar(stat='identity',position='dodge')+ggtitle(paste("Store type wise scatter plot between",labl(bv3()),"and",labl(varit)))}
  else if(bv3() %in% c("Fuel_Price","CPI","Unemployment","Temperature","Size","markdown")){
    ggplot(f,aes_string(y=bv3(),x=varit,color="Year"))+geom_point(stat='identity')+ggtitle(paste("Store type wise bar plot between",labl(varit),"and",labl(bv3())))}
  
  },width=600)%>%bindEvent(input$show_plott)
  
  output$hols<-renderPlot({i=input$holi
    ggplot(h,aes(x=year))+{if("Christmas" %in% i)geom_point(aes(y=salesc,color='red'),group=1)}+{if("Christmas" %in% i)geom_line(aes(y=salesc),color='red',group=1)}+
    {if("Labor day" %in% i)geom_point(aes(y=salesl,color='blue'),group=1)}+{if("Labor day" %in% i)geom_line(aes(y=salesl),color='blue',group=1)}+
    {if("Thanksgiving" %in% i)geom_point(aes(y=salest,color='#FF9900'),group=1)}+{if("Thanksgiving" %in% i)geom_line(aes(y=salest),color='#FF9900',group=1)}+
    {if("Super bowl" %in% i)geom_point(aes(y=salesb,color='#990099'),group=1)}+{if("Super bowl" %in% i)geom_line(aes(y=salesb),color='#990099',group=1)}+
      ylab("Sales")+ggtitle("Holidays vs sales")+
     scale_color_manual(name='Holidays',values = c(
         'Christmas' = 'red',
         'Labor day' = 'blue','Thanksgiving'='#FF9900','Super bowl'='#990099')) +
      labs(color = 'Holidays')
  },width=500)
}

  




# Run the application 
shinyApp(ui = ui, server = server)

