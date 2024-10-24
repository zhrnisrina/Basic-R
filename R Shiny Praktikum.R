
setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/SIM")
library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(datasets)


#header
headerItem <- dashboardHeader(
  title = "Latihan Buat Dashboard",
  dropdownMenu(
    type = "tasks",
    taskItem(
      text = "Progress Project SIM",
      value = 10,
      color = "purple"
    ),
    taskItem(
      text = "Progress Tugas ADK",
      value = 5,
      color = "red"
    ),
    taskItem(
      text = "Progress Tugas PKM",
      value = 20,
      color = "blue"
    )
  ),
  dropdownMenu(
    type = "notification",
    notificationItem(
      text = "Get Started with R Shiny",
      icon = shiny::icon("warning"),
      status = "success",
      href = "https://shiny.rstudio.com/"
    )
  ),
  dropdownMenu(
    type = "message",
    messageItem(
      from = "An Yujin",
      message = "Fighting",
      icon = shiny::icon("user"),
      time = "13-06-2021 11:11"
    ),
    messageItem(
      from = "Choi Jisu",
      message = "Fighting",
      icon = shiny::icon("user"),
      time = "13-06-2021 12.45"
    ),
    messageItem(
      from = "Bae Joohyun",
      message = "19",
      icon = shiny::icon("user"),
      time = "13-06-2021 13.00"
    )
  )
)

sidebarItem<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home",tabName = "Home"),
    menuItem("Dataset",tabName = "DataSet"),
    menuItem("Statistika Deskriptif",tabname="StatDesk"),
    menuItem("visualisasi",tabname = "Visualisasi",
             menuSubItem("Variabel Respon",tabName = "Respon"),
             menuSubItem("Variabel Prediktor",tabName = "Prediktor"))
  )
)


bodyItem <- dashboardBody(
  tabItems(
    tabItem(tabName = "DataSet",
            h3("Data yang digunakan"),
            fluidPage(
              sidebarPanel(
                selectInput("data",label = "Data",
                            choices = ls('package:datasets'))),
              mainPanel(tableOutput("data_table"))
            )
    ),
    tabItem(tabName = "StatDesk",
            h3("Summary Statistics"),
            verbatimTextOutput("summary_stat"))
  )
)

server<- function(input,output) {
  output$data_table=renderTable({
    tabel=get(input$dataset, "package:datasets")
    tabel
  })
  
  output$summary_stat=renderPrint({
    ringkasan=get(input$dataset, "package:datasets")
    descr(ringkasan,stats = "common")
  })
}
ui <- dashboardPage(header=headerItem,
                    sidebar=sidebarItem,
                    body = bodyItem)
shinyApp(ui,server)



#-----------------------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(
    title = "Basic dashboard",
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Sales Dept",
                   message = "Sales are steady this month."
                 ),
                 messageItem(
                   from = "New User",
                   message = "How do I register?",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 messageItem(
                   from = "Support",
                   message = "The new server is ready.",
                   icon = icon("life-ring"),
                   time = "2014-12-01"
                 ),
                 type = "notifications",
                 notificationItem(
                   text = "5 new users today",
                   icon("users")
                 ),
                 notificationItem(
                   text = "12 items delivered",
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) { 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })}

shinyApp(ui, server)


#-------------------------------------------------------------------------------------------------


library(ggplot2)
library(car)   # manggil dataset
library(MASS) # manggil dataset
library(reshape2) # mengubah bentuk frame
library(graphics) #mengakomodasi plot 3d
install.packages("gcookbook")
library(gcookbook) # manggil dataset
library(datasets)

data("Salaries")
head(Salaries)
data("biopsy")
data("mtcars")
head(mtcars)
data("geyser")
data("volcano")

# histogram
ggplot(Salaries,aes(x=salary)) + geom_histogram()

# density plot
ggplot(Salaries,aes(x=salary)) + geom_density()

# histogram + density plot
ggplot(Salaries,aes(x=salary,y=..density..)) + geom_histogram(color="grey") +
  geom_density(fill="blue",alpha=0.2) +theme_minimal()

# multiple density plot
## using fill
ggplot(Salaries,aes(x=salary,fill=rank)) + geom_density(alpha=0.5) +theme_minimal()

## facet
ggplot(Salaries,aes(x=salary,fill=rank)) + geom_density() +
  theme_minimal() + theme(legend.position = "none") +
  facet_wrap(~rank) 
ggplot(Salaries) +
  aes(x = salary,fill=rank) +
  geom_density() + theme(legend.position = "none") +
  theme_minimal() +
  facet_wrap(vars(rank),nrow = 3)

# scatter plot
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point()

# add regression line
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point() + 
  geom_smooth()
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point() + 
  geom_smooth(method = lm, formula = y~poly(x,2))


# facet
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point() + 
  geom_smooth() + facet_wrap(~sex)
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point() + 
  geom_smooth(method = lm,formula = y~poly(x,2)) +
  facet_wrap(vars(sex),ncol = 2)
ggplot(Salaries,aes(x=yrs.since.phd,y=salary)) + geom_point() + 
  geom_smooth(method = lm,formula = y~poly(x,2)) +
  facet_wrap(vars(sex),nrow = 2)


# fill
ggplot(Salaries,aes(x=yrs.since.phd,y=salary, fill=sex)) + geom_point() + 
  geom_smooth()
ggplot(Salaries,aes(x=yrs.since.phd,y=salary, fill=sex)) + geom_point() + 
  geom_smooth(method = lm,formula = y~poly(x,2))

# boxplot + jitter plot
ggplot(Salaries,aes(x=rank,y=salary, fill=rank)) + geom_boxplot() + 
  geom_jitter(width = 0.2) + theme_minimal() + theme(legend.position = 'none')

# logistic regression plot
biopsi=biopsy
biopsi$malig=ifelse(biopsi$class=='malignant',1,0)

ggplot(biopsi,aes(x=V1,y=malig))+ geom_jitter(width = 0.3, height = 0.05, alpha=0.3) +
  geom_smooth(method = glm, method.args=list(family='binomial'))

# scatterplot + label
ggplot(mtcars,aes(x=disp,y=mpg,col=as.factor(cyl))) + geom_point() + 
  geom_text(aes(label=rownames(mtcars),x=disp+5), hjust=0, check_overlap = T) +
  expand_limits(xend=650)

# 2d density plot
ggplot(geyser,aes(x=waiting,y=duration)) + geom_point() +
  geom_density_2d()
ggplot(geyser,aes(x=waiting,y=duration)) + geom_point() +
  stat_density_2d(aes(alpha=..density..), geom = 'raster', contour = F)

# 2d contour plot==
volcano3d=melt(volcano)
colnames(volcano3d)=c('x','y','z')
ggplot(volcano3d,aes(x,y))+ geom_contour(aes(z=z))
ggplot(volcano3d,aes(x,y))+ stat_contour(aes(z=z), binwidth = 2, col='grey50')+
  stat_contour(aes(z=z), binwidth = 10, size=1)

# 3d contour
z=2*volcano
x=10*(1:nrow(z))
y=10*(1:ncol(z))
persp(x,y,z, theta=135, phi = 30, col = 'green3', scale = F, ltheta = -120,
      shade = 0.75, border = NA, box = F)

install.packages("esquisse")


#----------------------------------------------------------------------------------------

#25/4/2022 

install.packages("moonBook")
install.packages("Rmisc")
library("ggplot2")
library("gcookbook")
library("moonBook")
library(Rmisc)

#balloon plot
data("countries")
ggplot(subset(countries, Year==2009 & healthexp>2000),
       aes(x=healthexp, y=infmortality, size=GDP))+
  geom_point(fill='cornsilk', shape=21) + guides(size=F)+
  scale_size_area(max_size=15)+
  geom_text(aes(label=Name,
                y =infmortality+sqrt(GDP)/1200), vjust=0, size=3)

#cleveland dot plot
data("tophitters2001")
tophit = tophitters2001[1:25,]
ggplot(data=tophit, aes(x=avg, y=reorder(name,avg), colour=lg))+
  geom_point(size=3)+
  geom_segment(aes(xend=0.31, yend=name))+
  facet_grid(lg ~ ., scales = 'free_y', space='free_y')+
  theme_bw()+
  theme(legend.position='none')+
  ylab('')

#wilkinson dot plot
data("heightweight")
ggplot(heightweight, aes(x=sex, y = heightIn, fill=sex))+
  geom_boxplot(fill="white", width=0.5)+
  geom_dotplot(binaxis='y', stackdir='center', binwidth=0.5)+
  theme(legend.position = 'none')

#barplot labelling
#stack
data("cabbage_exp")
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill= Cultivar))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Weight), position = position_stack(vjust=0.5))

#dodge
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill= Cultivar))+
  geom_bar(stat='identity', position='dodge')+
  geom_text(aes(label=Weight), position = position_dodge(0.9), vjust=0)

#proportional stacked
library(plyr)
res = ddply(cabbage_exp, "Date", mutate, ratio=Weight/sum(Weight)); res
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill= Cultivar))+
  geom_bar(stat='identity', position='fill')+
  #geom_text(aes(label=Weight), position = position_fill(vjust=0.5))
  geom_text(data=res, aes(label=scales::percent(ratio)),
            position = position_fill(vjust=0.5))

#data count
data(acs)
ggplot(acs, aes(x=Dx, fill=smoking))+
  geom_bar() +
  geom_text(aes(label=..count..),stat='count',position = position_stack(vjust=0.5))

#dodge
ggplot(acs, aes(x=Dx, fill=smoking))+
  geom_bar(position='dodge')+
  geom_text(aes(label=..count..),stat='count',position = position_dodge(0.9), vjust=-0.2)

#proportional stacked
res = table(acs$smoking, acs$Dx)
res = apply(res, 2,function(x) x/sum(x))
install.packages("reshape")
library(reshape)
res = reshape::melt(res)
colnames(res) = c('smoking', 'Dx', 'ratio')
ggplot(acs, aes(x=Dx, fill=smoking))+
  geom_bar(position='fill')+
  #geom_text(aes(label=weight), position = position_fill(vjust=0.5))
  #geom_text(aes(label = scales::percent(..count../sum(..count..))), stat='count',
  #          position = position_fill(vjust=0.5))
  geom_text(data=res, aes(label = scales::percent(ratio)), stat='count',
            position = position_fill(vjust=0.5))

# line plot
data("ToothGrowth")
summarised=summarySE(data = ToothGrowth, measurevar = 'len', groupvars = c('supp','dose'))
str(summarised)
ggplot(summarised,aes(x=dose,y=len, col=supp, shape=supp)) + geom_point(size=4) +
  geom_line() + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=0.2)

# barplot + error plot
library(car)
data("Salaries")
df=summarySE(Salaries, measurevar = 'salary', groupvars = c('rank', 'sex'))
ggplot(df,aes(x=rank, y=salary, fill=sex)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin=salary-3*se, ymax=salary+3*se), position = 'dodge')

# boxplot
data("singer")
str(singer)
ggplot(singer, aes(x=voice.part,y=height, col=voice.part)) + 
  geom_boxplot(notch = T) + theme_minimal() +
  guides(col=F) + stat_summary(geom = 'point', fun = mean, shape=23, size=3)

# violin plot
data(acs)
ggplot(acs, aes(Dx,age,fill=Dx)) + geom_violin() +
  geom_boxplot(fill='white', width=0.5) + geom_jitter(width = 0.3) +
  stat_summary(geom = 'point', fun = mean, shape=23, size=5) + 
  guides(fill=F)

df=data.frame(matrix(data = NA, nrow = 120,ncol = 8))
colnames(df)=c('Nama','Pelajaran','S1','S2','S3','S4','S5','S6')
df$Pelajaran=rep(c('a','b','c'),40)
df$S1=rnorm(120,80,9)
df$S2=rnorm(120,80,9)
df$S3=rnorm(120,80,9)
df$S4=rnorm(120,80,9)
df$S5=rnorm(120,80,9)
df$S6=rnorm(120,80,9)

df1=reshape2::melt(df,id.vars=c('Nama','Pelajaran'))
ggplot(df1) +
  aes(x = variable, y = value, fill = Pelajaran) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# area plot
data("uspopage")
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup)) + 
  geom_area() + scale_fill_brewer(palette = 'Blues') +
  guides(fill=guide_legend(reverse = T))

# polar plot
library(ggiraphExtra)
data("rose")
ggplot(rose,aes(x=Month,y=value,fill=group)) + 
  geom_bar(stat = 'identity',col='black') +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  coord_polar()

# scatter plot + regression line + persamaan
data("mtcars")

lm2equation=function(data,x,y){
  reg=lm(mpg~wt,data)
  coeff=reg$coefficients
  coeff=round(coeff,3)
  eqn=paste(reg[["terms"]][[2]],"=", 
            paste(coeff[1]), paste(coeff[2], names(coeff[2])))
}

mylabel=lm2equation(mtcars,mtcars$wt,mtcars$mpg)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point() + 
  geom_text(aes(label=rownames(mtcars),x=wt+0.05),check_overlap = T, 
            hjust=0) +
  geom_smooth(method = 'lm') +
  annotate(geom = 'text', x=4, y=30, label=mylabel, size=7)


#-------------------------------------------------------------------------------------

#Import Library
library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(datasets)

#header
#hati hati typo di tanda kurung, untuk itu codingnya mesti rapi
headerItem<-dashboardHeader(
  title="Dashboard SIM",
  dropdownMenu(
    type = "tasks",
    taskItem(
      text = "Progres Tubes SIM",
      value = 50 #maks100
    )
  ),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "Get Started With R Shiny",
      href = "https://shiny.rstudio.com/"
    )
  ),
  dropdownMenu(
    type = "message",
    messageItem(
      from="Temenmu",
      message = "Ayo ngerjain SIM"
    )
  )
)

sidebarItem<-dashboardSidebar(
  sidebarMenu(
    menuItem("Pendahuluan", tabName = "Pendahuluan",icon = icon("play")),
    menuItem("Statistika Deskriptif", tabName = "statdes"),
    menuItem("Visualisasi", tabName = "Viz",
             menuSubItem("Variabel Respon", tabName = "Respon"),
             menuSubItem("Variabel Prediktor", tabName = "Pred"))
  )
)


bodyItem=dashboardBody(
  tabItems(
    tabItem(tabName = "Pendahuluan",
            h3("Data yang digunakan"),
            fluidPage(
              sidebarPanel(
                selectInput("dataset",label = "Dataset",
                            choices = ls('package:datasets'))),
              mainPanel(tableOutput("data_table"))
            )
    ),
    tabItem(tabName = "statdes",
            h3("Summary Statistics"),
            verbatimTextOutput("summary_stat"))
  )
)

server = function(input, output) {
  output$data_table=renderTable({
    tabel=get(input$dataset, "package:datasets")
    tabel
  })
  
  output$summary_stat=renderPrint({
    ringkasan=get(input$dataset, "package:datasets")
    descr(ringkasan,stats = "common")
  })
}

ui<-dashboardPage(header=headerItem,
                  sidebar = sidebarItem,
                  body = bodyItem,
                  skin="purple")
shinyApp(ui,server)
