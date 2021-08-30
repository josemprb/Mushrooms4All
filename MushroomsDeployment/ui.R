#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Load libraries
library("shiny")
library("shinydashboard")
library("DT")
library("rms")
library("caret")
library("randomForest")
library("dplyr")
library("igraph")
library("ggraph")

# Load data set
setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Mushrooms/")
shrooms <- read.csv("mushrooms.csv")

### Data preparation
# CLASS
shrooms$class <- as.character(shrooms$class)
shrooms$class[shrooms$class == "p"] <- "poisonous"
shrooms$class[shrooms$class == "e"] <- "edible"
shrooms$class <- as.factor(shrooms$class)
relevel(shrooms$class, "poisonous")

# CAP.SHAPE
shrooms$cap.shape <- as.character(shrooms$cap.shape)
shrooms$cap.shape[shrooms$cap.shape == "b"] <- "bell"
shrooms$cap.shape[shrooms$cap.shape == "c"] <- "conical"
shrooms$cap.shape[shrooms$cap.shape == "f"] <- "flat"
shrooms$cap.shape[shrooms$cap.shape == "k"] <- "knobbed"
shrooms$cap.shape[shrooms$cap.shape == "s"] <- "sunken"
shrooms$cap.shape[shrooms$cap.shape == "x"] <- "convex"
shrooms$cap.shape <- as.factor(shrooms$cap.shape)
relevel(shrooms$cap.shape, "bell")

# CAP.COLOR
shrooms$cap.color <- as.character(shrooms$cap.color)
shrooms$cap.color[shrooms$cap.color == "b"] <- "buff"
shrooms$cap.color[shrooms$cap.color == "c"] <- "cinnamon"
shrooms$cap.color[shrooms$cap.color == "e"] <- "red"
shrooms$cap.color[shrooms$cap.color == "g"] <- "gray"
shrooms$cap.color[shrooms$cap.color == "n"] <- "brown"
shrooms$cap.color[shrooms$cap.color == "p"] <- "pink"
shrooms$cap.color[shrooms$cap.color == "r"] <- "green"
shrooms$cap.color[shrooms$cap.color == "u"] <- "purple"
shrooms$cap.color[shrooms$cap.color == "w"] <- "white"
shrooms$cap.color[shrooms$cap.color == "y"] <- "yellow"
shrooms$cap.color <- as.factor(shrooms$cap.color)
relevel(shrooms$cap.color, "buff")

# CAP.SURFACE
shrooms$cap.surface <- as.character(shrooms$cap.surface)
shrooms$cap.surface[shrooms$cap.surface == "f"] <- "fibrous"
shrooms$cap.surface[shrooms$cap.surface == "g"] <- "grooves"
shrooms$cap.surface[shrooms$cap.surface == "s"] <- "smooth"
shrooms$cap.surface[shrooms$cap.surface == "y"] <- "scaly"
shrooms$cap.surface <- as.factor(shrooms$cap.surface)
relevel(shrooms$cap.surface, "fibrous")

# BRUISES
shrooms$bruises <- as.character(shrooms$bruises)
shrooms$bruises[shrooms$bruises == "f"] <- "no"
shrooms$bruises[shrooms$bruises == "t"] <- "bruises"
shrooms$bruises <- as.factor(shrooms$bruises)
relevel(shrooms$bruises, "no")

# ODOR
shrooms$odor <- as.character(shrooms$odor)
shrooms$odor[shrooms$odor == "a"] <- "almond"
shrooms$odor[shrooms$odor == "c"] <- "creosote"
shrooms$odor[shrooms$odor == "f"] <- "foul"
shrooms$odor[shrooms$odor == "l"] <- "anise"
shrooms$odor[shrooms$odor == "m"] <- "musty"
shrooms$odor[shrooms$odor == "n"] <- "none"
shrooms$odor[shrooms$odor == "p"] <- "pungent"
shrooms$odor[shrooms$odor == "s"] <- "spicy"
shrooms$odor[shrooms$odor == "y"] <- "fishy"
shrooms$odor <- as.factor(shrooms$odor)
relevel(shrooms$odor, "almond")

# GILL-ATTACHMENT
shrooms$gill.attachment <- as.character(shrooms$gill.attachment)
shrooms$gill.attachment[shrooms$gill.attachment == "a"] <- "attached"
shrooms$gill.attachment[shrooms$gill.attachment == "d"] <- "descending"
shrooms$gill.attachment[shrooms$gill.attachment == "f"] <- "free"
shrooms$gill.attachment[shrooms$gill.attachment == "n"] <- "notched"
shrooms$gill.attachment <- as.factor(shrooms$gill.attachment)
relevel(shrooms$gill.attachment, "attached")

# GILL-SPACING
shrooms$gill.spacing <- as.character(shrooms$gill.spacing)
shrooms$gill.spacing[shrooms$gill.spacing == "c"] <- "close"
shrooms$gill.spacing[shrooms$gill.spacing == "d"] <- "distant"
shrooms$gill.spacing[shrooms$gill.spacing == "w"] <- "crowded"
shrooms$gill.spacing <- as.factor(shrooms$gill.spacing)
relevel(shrooms$gill.spacing, "close")

# GILL-SIZE
shrooms$gill.size <- as.character(shrooms$gill.size)
shrooms$gill.size[shrooms$gill.size == "b"] <- "broad"
shrooms$gill.size[shrooms$gill.size == "n"] <- "narrow"
shrooms$gill.size <- as.factor(shrooms$gill.size)
relevel(shrooms$gill.size, "broad")

# GILL-COLOR
shrooms$gill.color <- as.character(shrooms$gill.color)
shrooms$gill.color[shrooms$gill.color == "b"] <- "buff"
shrooms$gill.color[shrooms$gill.color == "e"] <- "red"
shrooms$gill.color[shrooms$gill.color == "g"] <- "gray"
shrooms$gill.color[shrooms$gill.color == "h"] <- "chocolate"
shrooms$gill.color[shrooms$gill.color == "k"] <- "black"
shrooms$gill.color[shrooms$gill.color == "n"] <- "brown"
shrooms$gill.color[shrooms$gill.color == "o"] <- "orange"
shrooms$gill.color[shrooms$gill.color == "p"] <- "pink"
shrooms$gill.color[shrooms$gill.color == "r"] <- "green"
shrooms$gill.color[shrooms$gill.color == "u"] <- "purple"
shrooms$gill.color[shrooms$gill.color == "w"] <- "white"
shrooms$gill.color[shrooms$gill.color == "y"] <- "yellow"
shrooms$gill.color <- as.factor(shrooms$gill.color)
relevel(shrooms$gill.color, "buff")

# STALK-SHAPE
shrooms$stalk.shape <- as.character(shrooms$stalk.shape)
shrooms$stalk.shape[shrooms$stalk.shape == "e"] <- "enlarging"
shrooms$stalk.shape[shrooms$stalk.shape == "t"] <- "tapering"
shrooms$stalk.shape <- as.factor(shrooms$stalk.shape)
relevel(shrooms$stalk.shape, "enlarging")

# STALK-ROOT
shrooms$stalk.root <- as.character(shrooms$stalk.root)
shrooms$stalk.root[shrooms$stalk.root == "?"] <- "missing"
shrooms$stalk.root[shrooms$stalk.root == "b"] <- "bulbous"
shrooms$stalk.root[shrooms$stalk.root == "c"] <- "club"
shrooms$stalk.root[shrooms$stalk.root == "e"] <- "equal"
shrooms$stalk.root[shrooms$stalk.root == "r"] <- "rooted"
shrooms$stalk.root[shrooms$stalk.root == "u"] <- "cup"
shrooms$stalk.root[shrooms$stalk.root == "z"] <- "rhizomorphs"
shrooms$stalk.root <- as.factor(shrooms$stalk.root)
relevel(shrooms$stalk.root, "missing")

# STALK-SURFACE-ABOVE-RING
shrooms$stalk.surface.above.ring <- as.character(shrooms$stalk.surface.above.ring)
shrooms$stalk.surface.above.ring[shrooms$stalk.surface.above.ring == "f"] <- "fibrous"
shrooms$stalk.surface.above.ring[shrooms$stalk.surface.above.ring == "k"] <- "silky"
shrooms$stalk.surface.above.ring[shrooms$stalk.surface.above.ring == "s"] <- "smooth"
shrooms$stalk.surface.above.ring[shrooms$stalk.surface.above.ring == "y"] <- "scaly"
shrooms$stalk.surface.above.ring <- as.factor(shrooms$stalk.surface.above.ring)
relevel(shrooms$stalk.surface.above.ring, "fibrous")

# STALK-SURFACE-BELOW-RING
shrooms$stalk.surface.below.ring <- as.character(shrooms$stalk.surface.below.ring)
shrooms$stalk.surface.below.ring[shrooms$stalk.surface.below.ring == "f"] <- "fibrous"
shrooms$stalk.surface.below.ring[shrooms$stalk.surface.below.ring == "k"] <- "silky"
shrooms$stalk.surface.below.ring[shrooms$stalk.surface.below.ring == "s"] <- "smooth"
shrooms$stalk.surface.below.ring[shrooms$stalk.surface.below.ring == "y"] <- "scaly"
shrooms$stalk.surface.below.ring <- as.factor(shrooms$stalk.surface.below.ring)
relevel(shrooms$stalk.surface.below.ring, "fibrous")

# STALK-COLOR-ABOVE-RING
shrooms$stalk.color.above.ring <- as.character(shrooms$stalk.color.above.ring)
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "b"] <- "buff"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "c"] <- "cinnamon"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "e"] <- "red"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "g"] <- "gray"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "n"] <- "brown"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "o"] <- "orange"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "p"] <- "pink"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "w"] <- "white"
shrooms$stalk.color.above.ring[shrooms$stalk.color.above.ring == "y"] <- "yellow"
shrooms$stalk.color.above.ring <- as.factor(shrooms$stalk.color.above.ring)
relevel(shrooms$stalk.color.above.ring, "buff")

# STALK-COLOR-BELOW-RING
shrooms$stalk.color.below.ring <- as.character(shrooms$stalk.color.below.ring)
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "b"] <- "buff"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "c"] <- "cinnamon"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "e"] <- "red"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "g"] <- "gray"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "n"] <- "brown"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "o"] <- "orange"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "p"] <- "pink"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "w"] <- "white"
shrooms$stalk.color.below.ring[shrooms$stalk.color.below.ring == "y"] <- "yellow"
shrooms$stalk.color.below.ring <- as.factor(shrooms$stalk.color.below.ring)
relevel(shrooms$stalk.color.below.ring, "buff")

# VEIL-TYPE
shrooms$veil.type <- as.character(shrooms$veil.type)
shrooms$veil.type[shrooms$veil.type == "p"] <- "partial"
shrooms$veil.type[shrooms$veil.type == "u"] <- "universal"
shrooms$veil.type <- as.factor(shrooms$veil.type)
relevel(shrooms$veil.type, "partial")

# VEIL-COLOR
shrooms$veil.color <- as.character(shrooms$veil.color)
shrooms$veil.color[shrooms$veil.color == "n"] <- "brown"
shrooms$veil.color[shrooms$veil.color == "o"] <- "orange"
shrooms$veil.color[shrooms$veil.color == "w"] <- "white"
shrooms$veil.color[shrooms$veil.color == "y"] <- "yellow"
shrooms$veil.color <- as.factor(shrooms$veil.color)
relevel(shrooms$veil.color, "brown")

# RING-NUMBER
shrooms$ring.number <- as.character(shrooms$ring.number)
shrooms$ring.number[shrooms$ring.number == "b"] <- "other"
shrooms$ring.number[shrooms$ring.number == "n"] <- "none"
shrooms$ring.number[shrooms$ring.number == "o"] <- "one"
shrooms$ring.number[shrooms$ring.number == "t"] <- "two"
shrooms$ring.number <- as.factor(shrooms$ring.number)
relevel(shrooms$ring.number, "none")

# RING-TYPE
shrooms$ring.type <- as.character(shrooms$ring.type)
shrooms$ring.type[shrooms$ring.type == "c"] <- "cobwebby"
shrooms$ring.type[shrooms$ring.type == "e"] <- "evanescent"
shrooms$ring.type[shrooms$ring.type == "f"] <- "flaring"
shrooms$ring.type[shrooms$ring.type == "l"] <- "large"
shrooms$ring.type[shrooms$ring.type == "n"] <- "none"
shrooms$ring.type[shrooms$ring.type == "p"] <- "pendant"
shrooms$ring.type[shrooms$ring.type == "s"] <- "sheathing"
shrooms$ring.type[shrooms$ring.type == "z"] <- "zone"
shrooms$ring.type <- as.factor(shrooms$ring.type)
relevel(shrooms$ring.type, "cobwebby")

# SPORE-PRINT-COLOR
shrooms$spore.print.color <- as.character(shrooms$spore.print.color)
shrooms$spore.print.color[shrooms$spore.print.color == "b"] <- "buff"
shrooms$spore.print.color[shrooms$spore.print.color == "h"] <- "chocolate"
shrooms$spore.print.color[shrooms$spore.print.color == "k"] <- "black"
shrooms$spore.print.color[shrooms$spore.print.color == "n"] <- "brown"
shrooms$spore.print.color[shrooms$spore.print.color == "o"] <- "orange"
shrooms$spore.print.color[shrooms$spore.print.color == "r"] <- "green"
shrooms$spore.print.color[shrooms$spore.print.color == "u"] <- "purple"
shrooms$spore.print.color[shrooms$spore.print.color == "w"] <- "white"
shrooms$spore.print.color[shrooms$spore.print.color == "y"] <- "yellow"
shrooms$spore.print.color <- as.factor(shrooms$spore.print.color)
relevel(shrooms$spore.print.color, "buff")

# POPULATION
shrooms$population <- as.character(shrooms$population)
shrooms$population[shrooms$population == "a"] <- "abundant"
shrooms$population[shrooms$population == "c"] <- "clustered"
shrooms$population[shrooms$population == "n"] <- "numerous"
shrooms$population[shrooms$population == "s"] <- "scattered"
shrooms$population[shrooms$population == "v"] <- "several"
shrooms$population[shrooms$population == "y"] <- "solitary"
shrooms$population <- as.factor(shrooms$population)
relevel(shrooms$population, "abundant")

# HABITAT
shrooms$habitat <- as.character(shrooms$habitat)
shrooms$habitat[shrooms$habitat == "d"] <- "woods"
shrooms$habitat[shrooms$habitat == "g"] <- "grasses"
shrooms$habitat[shrooms$habitat == "l"] <- "leaves"
shrooms$habitat[shrooms$habitat == "m"] <- "meadows"
shrooms$habitat[shrooms$habitat == "p"] <- "paths"
shrooms$habitat[shrooms$habitat == "u"] <- "urban"
shrooms$habitat[shrooms$habitat == "w"] <- "waste"
shrooms$habitat <- as.factor(shrooms$habitat)
relevel(shrooms$habitat, "woods")

# Write it to a file.
write.csv(shrooms, file = "shrooms.csv")


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  dashboardHeader(title = "Mushrooms Deployment"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "1", icon = icon("home", lib = "glyphicon")),
      menuItem("Logistic regression model", tabName = "2", icon = icon("retweet", lib = "glyphicon")),
      menuItem("Random forest model", tabName = "3", icon = icon("tree-conifer", lib = "glyphicon"))
    )
  ),
  # Body content
  dashboardBody(
    tabItems (
      # First tab content
      tabItem(tabName = "1",
              helpText(h2("Home Page")),
              helpText(h4("The application is done to visualize the results of a project done over a data set of mushrooms.
                          It has a different tab to do each model. In this project deployment, we realize two different models.
                          The first one is a logistic regression model, in which the data is divided into training and testing
                          sets, and the the regression is performed within the training set. Afterwards, an evaluation of the 
                          model is done using the test set. The output metrics of this evaluation will be compared with the
                          ones obtained in the other model, to decide which analysis is better. Finally, the model is deployed
                          in the second tab of this Shiny application.")),
              helpText(h4("The second analysis performed is almost the same. Firstly, we apply the model to the data set.
                          For this time, it is a random forest model to train the data before applying a binary
                          classification model. Once the classification is performed, we make the same evaluation as before,
                          but for the second task. The deployment will be visualized in the Shiny application, in its dedicated
                          tab.")),
              helpText(h4("The data set is made of information about different types of mushrooms. They are classified
                          in two main classes, edibles and poisoned ones, which are used as labels. These labels are supposed
                          to be predicted applying different algorithms. The data set is made of 8124 observations organized
                          into 23 different variables, which are all qualitative variables. Below, we can observe a sample of
                          such data set, in which the different variables used for the analysis can be ticked or unticked to
                          show them in the data table.")),
              checkboxGroupInput("vars", h4("Show variables to use:"), names(shrooms), selected = names(shrooms), inline = TRUE),
              DT::dataTableOutput("table")
              ),
      # Second tab content
      tabItem(tabName = "2",
              fluidPage(
                title = "Logistic Regression Deployment",
                fluidRow(
                  column(3,
                         selectInput("var1.lr", 
                                     label = "cap.shape",
                                     choices = levels(shrooms$cap.shape),
                                     selected = levels(shrooms$cap.shape)[1]),
                         
                         selectInput("var2.lr", 
                                     label = "cap.surface",
                                     choices = levels(shrooms$cap.surface),
                                     selected = levels(shrooms$cap.surface)[1]),
                         
                         selectInput("var3.lr", 
                                     label = "cap.color",
                                     choices = levels(shrooms$cap.color),
                                     selected = levels(shrooms$cap.color)[1]),
                         selectInput("var4.lr", 
                                     label = "bruises",
                                     choices = levels(shrooms$bruises),
                                     selected = levels(shrooms$bruises)[1]),
                         
                         selectInput("var5.lr", 
                                     label = "odor",
                                     choices = levels(shrooms$odor),
                                     selected = levels(shrooms$odor)[1]),
                         
                         selectInput("var6.lr", 
                                     label = "gill.attachment",
                                     choices = levels(shrooms$gill.attachment),
                                     selected = levels(shrooms$gill.attachment)[1])
                  ),
                  column(3,
                         
                         selectInput("var7.lr", 
                                     label = "gill.spacing",
                                     choices = levels(shrooms$gill.spacing),
                                     selected = levels(shrooms$gill.spacing)[1]),
                         
                         selectInput("var8.lr", 
                                     label = "gill.size",
                                     choices = levels(shrooms$gill.size),
                                     selected = levels(shrooms$gill.size)[1]),
                         
                         selectInput("var9.lr", 
                                     label = "gill.color",
                                     choices = levels(shrooms$gill.color),
                                     selected = levels(shrooms$gill.color)[1]),
                         
                         selectInput("var10.lr", 
                                     label = "stalk.shape",
                                     choices = levels(shrooms$stalk.shape),
                                     selected = levels(shrooms$stalk.shape)[1]),
                         
                         selectInput("var11.lr", 
                                     label = "stalk.root",
                                     choices = levels(shrooms$stalk.root),
                                     selected = levels(shrooms$stalk.root)[1]),
                         
                         selectInput("var12.lr", 
                                     label = "stalk.surface.above.ring",
                                     choices = levels(shrooms$stalk.surface.above.ring),
                                     selected = levels(shrooms$stalk.surface.above.ring)[1])
                  ),
                  column(3,    
                         
                         selectInput("var13.lr", 
                                     label = "stalk.surface.below.ring",
                                     choices = levels(shrooms$stalk.surface.below.ring),
                                     selected = levels(shrooms$stalk.surface.below.ring)[1]),
                         
                         selectInput("var14.lr", 
                                     label = "stalk.color.above.ring",
                                     choices = levels(shrooms$stalk.color.above.ring),
                                     selected = levels(shrooms$stalk.color.above.ring)[1]),
                         
                         selectInput("var15.lr", 
                                     label = "stalk.color.below.ring",
                                     choices = levels(shrooms$stalk.color.below.ring),
                                     selected = levels(shrooms$stalk.color.below.ring)[1]),
                         
                         selectInput("var16.lr", 
                                     label = "veil.type",
                                     choices = levels(shrooms$veil.type),
                                     selected = levels(shrooms$veil.type)[1]),
                         
                         selectInput("var17.lr", 
                                     label = "veil.color",
                                     choices = levels(shrooms$veil.color),
                                     selected = levels(shrooms$veil.color)[1])
                  ),
                  column(3,      
                         selectInput("var18.lr", 
                                     label = "ring.number",
                                     choices = levels(shrooms$ring.number),
                                     selected = levels(shrooms$ring.number)[1]),
                         
                         selectInput("var19.lr", 
                                     label = "ring.type",
                                     choices = levels(shrooms$ring.type),
                                     selected = levels(shrooms$ring.type)[1]),
                         
                         selectInput("var20.lr", 
                                     label = "spore.print.color",
                                     choices = levels(shrooms$spore.print.color),
                                     selected = levels(shrooms$spore.print.color)[1]),
                         
                         selectInput("var21.lr", 
                                     label = "population",
                                     choices = levels(shrooms$population),
                                     selected = levels(shrooms$population)[1]),
                         
                         selectInput("var22.lr", 
                                     label = "habitat",
                                     choices = levels(shrooms$habitat),
                                     selected = levels(shrooms$habitat)[1])
                  )
                ),
                textOutput("response"),
                
                selectInput("select", label = h3("Select variable to be shown:"), 
                            choices = names(shrooms)[-1], 
                            selected = names(shrooms)[2]),
                plotOutput("plot.lr", width = "100%", height = "400px")
              )   
      ),
      # Third tab content
      tabItem(tabName = "3",
              fluidPage(
                title = "Random Forest Deployment",
                fluidRow(
                  column(3,
                         selectInput("var1.rf", 
                                     label = "cap.shape",
                                     choices = levels(shrooms$cap.shape),
                                     selected = levels(shrooms$cap.shape)[1]),
                         
                         selectInput("var2.rf", 
                                     label = "cap.surface",
                                     choices = levels(shrooms$cap.surface),
                                     selected = levels(shrooms$cap.surface)[1]),
                         
                         selectInput("var3.rf", 
                                     label = "cap.color",
                                     choices = levels(shrooms$cap.color),
                                     selected = levels(shrooms$cap.color)[1]),
                         selectInput("var4.rf", 
                                     label = "bruises",
                                     choices = levels(shrooms$bruises),
                                     selected = levels(shrooms$bruises)[1]),
                         
                         selectInput("var5.rf", 
                                     label = "odor",
                                     choices = levels(shrooms$odor),
                                     selected = levels(shrooms$odor)[1]),
                         
                         selectInput("var6.rf", 
                                     label = "gill.attachment",
                                     choices = levels(shrooms$gill.attachment),
                                     selected = levels(shrooms$gill.attachment)[1])
                  ),
                  column(3,
                         
                         selectInput("var7.rf", 
                                     label = "gill.spacing",
                                     choices = levels(shrooms$gill.spacing),
                                     selected = levels(shrooms$gill.spacing)[1]),
                         
                         selectInput("var8.rf", 
                                     label = "gill.size",
                                     choices = levels(shrooms$gill.size),
                                     selected = levels(shrooms$gill.size)[1]),
                         
                         selectInput("var9.rf", 
                                     label = "gill.color",
                                     choices = levels(shrooms$gill.color),
                                     selected = levels(shrooms$gill.color)[1]),
                         
                         selectInput("var10.rf", 
                                     label = "stalk.shape",
                                     choices = levels(shrooms$stalk.shape),
                                     selected = levels(shrooms$stalk.shape)[1]),
                         
                         selectInput("var11.rf", 
                                     label = "stalk.root",
                                     choices = levels(shrooms$stalk.root),
                                     selected = levels(shrooms$stalk.root)[1]),
                         
                         selectInput("var12.rf", 
                                     label = "stalk.surface.above.ring",
                                     choices = levels(shrooms$stalk.surface.above.ring),
                                     selected = levels(shrooms$stalk.surface.above.ring)[1])
                  ),
                  column(3,    
                         
                         selectInput("var13.rf", 
                                     label = "stalk.surface.below.ring",
                                     choices = levels(shrooms$stalk.surface.below.ring),
                                     selected = levels(shrooms$stalk.surface.below.ring)[1]),
                         
                         selectInput("var14.rf", 
                                     label = "stalk.color.above.ring",
                                     choices = levels(shrooms$stalk.color.above.ring),
                                     selected = levels(shrooms$stalk.color.above.ring)[1]),
                         
                         selectInput("var15.rf", 
                                     label = "stalk.color.below.ring",
                                     choices = levels(shrooms$stalk.color.below.ring),
                                     selected = levels(shrooms$stalk.color.below.ring)[1]),
                         
                         selectInput("var16.rf", 
                                     label = "veil.type",
                                     choices = levels(shrooms$veil.type),
                                     selected = levels(shrooms$veil.type)[1]),
                         
                         selectInput("var17.rf", 
                                     label = "veil.color",
                                     choices = levels(shrooms$veil.color),
                                     selected = levels(shrooms$veil.color)[1])
                  ),
                  column(3,      
                         selectInput("var18.rf", 
                                     label = "ring.number",
                                     choices = levels(shrooms$ring.number),
                                     selected = levels(shrooms$ring.number)[1]),
                         
                         selectInput("var19.rf", 
                                     label = "ring.type",
                                     choices = levels(shrooms$ring.type),
                                     selected = levels(shrooms$ring.type)[1]),
                         
                         selectInput("var20.rf", 
                                     label = "spore.print.color",
                                     choices = levels(shrooms$spore.print.color),
                                     selected = levels(shrooms$spore.print.color)[1]),
                         
                         selectInput("var21.rf", 
                                     label = "population",
                                     choices = levels(shrooms$population),
                                     selected = levels(shrooms$population)[1]),
                         
                         selectInput("var22.rf", 
                                     label = "habitat",
                                     choices = levels(shrooms$habitat),
                                     selected = levels(shrooms$habitat)[1])
                      
                  )
                ),
                
                textOutput("response1"),
                
                sliderInput("k", label = h3("Select a tree to show:"), 
                            min = 1, max = 500, value = 1),
                plotOutput("tree", width = "100%", height = "800px")
              )    
      )
              )
              )
              ))