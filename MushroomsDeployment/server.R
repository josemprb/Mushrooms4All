#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Load libraries
library("shiny")
library("DT")

# Load prepared data set, written after changing the names of the variables.
setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/CognitiveSystems/Exams/Mushrooms/")
shrooms <- read.csv("shrooms.csv")

### Logistic Regression Modelling and Evaluation
# Divide in training and test sets.
set.seed(123)
train_ind <- sample.int(n = nrow(shrooms), size = floor(0.15*nrow(shrooms)), replace = F)
trainset <- shrooms[train_ind, ]
testset <- shrooms[-train_ind, ]


# Modelling using logistic regression and the training test.
model.lr<-glm(class~cap.shape+cap.surface+cap.color+bruises+odor+gill.attachment+gill.spacing+gill.size+
                gill.color+stalk.shape+stalk.root+stalk.surface.above.ring+stalk.surface.below.ring+
                stalk.color.above.ring+stalk.color.below.ring+veil.type+veil.color+ring.number+
                ring.type+spore.print.color+population+habitat,data = trainset,family = binomial(link = "logit"))
summary(model.lr)

# Prediction using the test set.
predictedval <- predict(model.lr, newdata = testset, type='response')
fitted.results.cat <- ifelse(predictedval > 0.5,"poisonous","edible")
fitted.results.cat <- as.factor(fitted.results.cat)

# Evaluation. Compute the confusion matrix and calculate the metrics.
cm <- confusionMatrix(data=fitted.results.cat, 
                      reference=testset$class)
Accuracy.lr<-round(cm$overall[1],2)
cm$table
deathrate.lr <- cm$table[1,2]/sum(cm$table[,1])
precision.lr <- cm$table[1,1]/sum(cm$table[,1])
recall.lr <- cm$table[1,1]/sum(cm$table[1,])
specificity.lr <- cm$table[2,2]/sum(cm$table[2,])

### Random Forest Modelling and Evaluation
# Divide in train and test set.
set.seed(123)
train_ind <- sample.int(n = nrow(shrooms), size = floor(0.15*nrow(shrooms)), replace = F)
trainset1 <- shrooms[train_ind, ]
testset1 <- shrooms[-train_ind, ]
trainset1 <- trainset1[-1]
testset1 <-testset1[-1]

# Modelling with training set.
model.rf <- randomForest(class ~ ., data=trainset1, importance=TRUE)
# Look at variable importance.
round(importance(model.rf), 2)
# Predict the model using the test set.
pred <- predict(model.rf, newdata = testset1)

# Evaluation: compute the confusion matrix and calculate every metric from it.
pred.cm <- table(pred, testset1$class)
pred.cm
accuracy.rf <- (pred.cm[1,1]+pred.cm[2,2])/sum(pred.cm)
deathrate.rf <- pred.cm[1,2]/sum(pred.cm[1,])
precision.rf <- pred.cm[1,1]/sum(pred.cm[,1])
recall.rf <- pred.cm[1,1]/sum(pred.cm[1,])
specificity.rf <- pred.cm[2,2]/sum(pred.cm[2,])

### Define server logic
shinyServer(function(input, output) {
  
  # Tab 1
  output$table <- DT::renderDataTable({
    DT::datatable(shrooms[input$vars], options = 
                    list(scrollX = TRUE, pageLength = 50, lengthMenu = c(10, 50, 100, 150, 200, 250, 300, 400, 500)))
  })
  
  # Tab 2
  x <- reactive ({
    if (input$select == "cap.shape") {
      x<- shrooms$cap.shape
    } else if (input$select == "cap.surface") {
      x<- shrooms$cap.surface
    } else if (input$select == "cap.color") {
      x<- shrooms$cap.color
    } else if (input$select == "bruises") {
      x<- shrooms$bruises
    } else if (input$select == "odor") {
      x<- shrooms$odor
    } else if (input$select == "gill.attachment") {
      x<- shrooms$gill.attachment
    } else if (input$select == "gill.spacing") {
      x<- shrooms$gill.spacing
    } else if (input$select == "gill.size") {
      x<- shrooms$gill.size
    } else if (input$select == "gill.color") {
      x<- shrooms$gill.color
    } else if (input$select == "stalk.shape") {
      x<- shrooms$stalk.shape
    } else if (input$select == "stalk.root") {
      x<- shrooms$stalk.root
    } else if (input$select == "stalk.surface.above.ring") {
      x<- shrooms$stalk.surface.above.ring
    } else if (input$select == "stalk.surface.below.ring") {
      x<- shrooms$stalk.surface.below.ring
    } else if (input$select == "stalk.color.above.ring") {
      x<- shrooms$stalk.color.above.ring
    } else if (input$select == "stalk.color.below.ring") {
      x<- shrooms$stalk.color.below.ring
    } else if (input$select == "veil.type") {
      x<- shrooms$veil.type
    } else if (input$select == "veil.color") {
      x<- shrooms$veil.color
    } else if (input$select == "ring.number") {
      x<- shrooms$ring.number
    } else if (input$select == "ring.type") {
      x<- shrooms$ring.type
    } else if (input$select == "spore.print.color") {
      x<- shrooms$spore.print.color
    } else if (input$select == "population") {
      x<- shrooms$population
    } else if (input$select == "habitat") {
      x<- shrooms$habitat
    }
  })
  
  output$plot.lr <- renderPlot({
    plot(class~x(), data=shrooms, main = "Relationship between the selected variable and the predicted class",
         col=class, xlab = input$select)
  })
  
  
  output$response <- renderText({
    #Pred <-predict(model.lr,cap.shape=input$var1.lr,cap.surface=input$var2.lr,cap.color=input$var3.lr,bruises=input$var4.lr,odor=input$var5.lr,gill.attachment=input$var6.lr,
    #                                  gill.spacing=input$var7.lr,gill.size=input$var8.lr,gill.color=input$var9.lr,stalk.shape=input$var10.lr,stalk.root=input$var11.lr,stalk.surface.above.ring=input$var12.lr,
    #                                  stalk.surface.below.ring=input$var13.lr,stalk.color.above.ring=input$var14.lr,stalk.color.below.ring=input$var15.lr,veil.type=input$var16.lr,veil.color=input$var17.lr,ring.number=input$var18.lr,
    #                                  ring.type=input$var19.lr,spore.print.color=input$var20.lr,population=input$var21.lr,habitat=input$var22.lr)
    cazz<-as.data.frame(cbind(input$var1.lr,input$var2.lr,input$var3.lr,input$var4.lr,input$var5.lr,input$var6.lr,input$var7.lr,input$var8.lr,input$var9.lr,input$var10.lr,input$var11.lr,input$var12.lr,input$var13.lr,input$var14.lr,input$var15.lr,input$var16.lr,input$var17.lr,input$var18.lr,input$var19.lr,input$var20.lr,input$var21.lr,input$var22.lr))
    colnames(cazz)<-colnames(shrooms)[-c(1,2)]
    Pred<-predict(model.lr,newdata=cazz)
    answer<- ifelse(Pred > 0.5,"poisonous","edible")
    
    
    if(answer=="poisonous"){paste("The chosen mushroom is  ",answer, "DO NOT EAT IT")}
    else {paste("The chosen mushroom is  ",answer)}
  })
  
  # Tab 3
  
  
  
  output$tree <- renderPlot ({
    # get tree by index
    tree <- randomForest::getTree(model.rf, 
                                  k = input$k, 
                                  labelVar = TRUE) %>%
      tibble::rownames_to_column() %>%
      # make leaf split points to NA, so the 0s won't get plotted
      mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
    
    # prepare data frame for graph
    graph_frame <- data.frame(from = rep(tree$rowname, 2),
                              to = c(tree$`left daughter`, tree$`right daughter`))
    
    # convert to graph and delete the last node that we don't want to plot
    graph <- graph_from_data_frame(graph_frame) %>%
      delete_vertices("0")
    
    # set node labels
    V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
    V(graph)$leaf_label <- as.character(tree$prediction)
    V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
    
    # plot
    plot <- ggraph(graph, 'dendrogram') + 
      theme_bw() +
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
      geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
      geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                      repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "white"),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 18))
    
    print(plot)
    
    
  })
  
  output$response1 <- renderText({
    cazz1<-as.data.frame(cbind(input$class.rf,input$var1.rf,input$var2.rf,input$var3.rf,input$var4.rf,input$var5.rf,input$var6.rf,
                               input$var7.rf,input$var8.rf,input$var9.rf,input$var10.rf,input$var11.rf,input$var12.rf,
                               input$var13.rf,input$var14.rf,input$var15.rf,input$var16.rf,input$var17.rf,input$var18.rf,
                               input$var19.rf,input$var20.rf,input$var21.rf,input$var22.rf))
    colnames(cazz1)<-colnames(shrooms)[-c(1,2)]
    cazz1[,1] <- factor(cazz1[,1], levels = levels(trainset1$cap.shape))
    cazz1[,2] <- factor(cazz1[,2], levels = levels(trainset1$cap.surface))
    cazz1[,3] <- factor(cazz1[,3], levels = levels(trainset1$cap.color))
    cazz1[,4] <- factor(cazz1[,4], levels = levels(trainset1$bruises))
    cazz1[,5] <- factor(cazz1[,5], levels = levels(trainset1$odor))
    cazz1[,6] <- factor(cazz1[,6], levels = levels(trainset1$gill.attachment))
    cazz1[,7] <- factor(cazz1[,7], levels = levels(trainset1$gill.spacing))
    cazz1[,8] <- factor(cazz1[,8], levels = levels(trainset1$gill.size))
    cazz1[,9] <- factor(cazz1[,9], levels = levels(trainset1$gill.color))
    cazz1[,10] <- factor(cazz1[,10], levels = levels(trainset1$stalk.shape))
    cazz1[,11] <- factor(cazz1[,11], levels = levels(trainset1$stalk.root))
    cazz1[,12] <- factor(cazz1[,12], levels = levels(trainset1$stalk.surface.above.ring))
    cazz1[,13] <- factor(cazz1[,13], levels = levels(trainset1$stalk.surface.below.ring))
    cazz1[,14] <- factor(cazz1[,14], levels = levels(trainset1$stalk.color.above.ring))
    cazz1[,15] <- factor(cazz1[,15], levels = levels(trainset1$stalk.color.below.ring))
    cazz1[,16] <- factor(cazz1[,16], levels = levels(trainset1$veil.type))
    cazz1[,17] <- factor(cazz1[,17], levels = levels(trainset1$veil.color))
    cazz1[,18] <- factor(cazz1[,18], levels = levels(trainset1$ring.number))
    cazz1[,19] <- factor(cazz1[,19], levels = levels(trainset1$ring.type))
    cazz1[,20] <- factor(cazz1[,20], levels = levels(trainset1$spore.print.color))
    cazz1[,21] <- factor(cazz1[,21], levels = levels(trainset1$population))
    cazz1[,22] <- factor(cazz1[,22], levels = levels(trainset1$habitat))
    Pred1<-predict(model.rf,newdata=cazz1)
    
    if(Pred1=="poisonous"){paste("The chosen mushroom is  ",Pred1, "DO NOT EAT IT")}
    else {paste("The chosen mushroom is  ",Pred1)}
  })
})