#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("class")
#install.packages("caTools")
#install.packages("caret")
#install.packages("shiny")
library("ggplot2")
library(reshape)
library(class)
library(caTools)
library(caret)
library(shiny)

# Data Exploration

fifa = read.csv('CopyOffifa_2023.csv')

fifa

# check dimensions and for missing data
#dim(fifa)
#which(is.na(fifa))
#head(fifa)

# remove extra index column
fifa = subset(fifa, select = -c(X))

# get subset of just ratings of the players
ratings = subset(fifa, select = c(rating, PAC, SHO, PAS, DRI, DEF, PHY))

# get subset of player ratings based on position group
defenders = subset(fifa, (position == "LWB" | position == "LB" | position == "CB" | position == "RB" | position == "RWB"))
midefielders = subset(fifa, (position == "CDM" | position == "LM" | position == "CM" | position == "RM" | position == "CAM"))
forwards = subset(fifa, (position == "LW" | position == "CF" | position == "RW" | position == "ST"))

# function for plotting heat map of correlation of ratings
plot_cor_ratings = function(data, title) {
  colnames(data) = c("OVR", "PAC", "SHO", "PAS", "DRI", "DEF", "PHY")
  rownames(data) = c("OVR", "PAC", "SHO", "PAS", "DRI", "DEF", "PHY")
  df = melt(data)
  colnames(df) = c("x", "y", "value")
  df$value = round(df$value, digits = 2)
  ggplot(df, aes(x,y)) + geom_tile(aes(fill = value)) + geom_text(aes(label = value)) + ggtitle(title) + scale_fill_gradient2(low = "navy", mid="white", high = "red4") + theme(axis.title.x = element_blank(),
                                                                                                                                                                                axis.title.y = element_blank())
}

# correlation matrices of player ratings by position group
all = cor(ratings)
def = cor(subset(defenders, select = c(rating, PAC, SHO, PAS, DRI, DEF, PHY)))
mid = cor(subset(midefielders, select = c(rating, PAC, SHO, PAS, DRI, DEF, PHY)))
forw = cor(subset(forwards, select = c(rating, PAC, SHO, PAS, DRI, DEF, PHY)))

# KNN Algorithm

# create data frame of variables for the model
pos = subset(fifa, select = -c(name, rating, card_type, nation, league, team))

# encode positions to numeric labels for model
fact = factor(pos$position)
pos$position = unclass(fact)

# set random seed
set.seed(23)

# split into train and test data
split = sample.split(pos$position, SplitRatio = 0.7)
train = subset(pos, split == "TRUE")
test = subset(pos, split == "FALSE")

# normalize
pp = preProcess(subset(train, select = -c(position)), method=c("range"))
train = predict(pp, train)
test = predict(pp, test)

# chose k as sqrt(n), n=10,000
# note: train and test must NOT include position, cl is position
clf_knn = knn(train=train[,-1], test=test[,-1], cl=train$position, k=99)

# create confusion matrix
predictions = clf_knn
truth = test$position
cm = confusionMatrix(table(truth, predictions))

# plot confusion matrix
cmt = cm$table
colnames(cmt) = levels(fact)
rownames(cmt) = levels(fact)
hm = melt(cmt)
colnames(hm) = c("Actual", "Predicted", "value")

# get precision, recall, calculate f1 score
accuracy = cm$overall[1]
recall = cm$byClass[,1]
precision = cm$byClass[,3]
f1 = (2 * precision * recall) / (precision + recall)

# plot precision, recall, and f1 (IN APP USE CHECK BOXES TO SELECT WHICH TO PLOT ON TOP OF EACH OTHER)
metrics = data.frame(precision, recall, f1)
metrics["position"] = levels(fact)
metrics = melt(metrics)

# KNN Algorithm Using Position Groups

# RB, LB, RWB, LWB
outside_backs = subset(fifa, (position == "RB" | position == "LB" | position == "RWB" | position == "LWB"), select = -c(name, rating, card_type, nation, league, team))

# CB
center_backs = subset(fifa, (position == "CB"), select = -c(name, rating, card_type, nation, league, team))

# CDM, CM, CAM
center_mids = subset(fifa, (position == "CDM" | position == "CM" | position == "CAM"), select = -c(name, rating, card_type, nation, league, team))

# RM, LM, RW, LW
wide_players = subset(fifa, (position == "RM" | position == "LM" | position == "RW" | position == "LW"), select = -c(name, rating, card_type, nation, league, team))

# ST, CF
strikers = subset(fifa, (position == "ST" | position == "CF"), select = -c(name, rating, card_type, nation, league, team))

# combine positions into position groups
outside_backs$position = "OB"
center_mids$position = "CM"
wide_players$position = "WR"
strikers$position = "ST"

# recombine each group into one data frame
groups = rbind(outside_backs, center_backs)
groups = rbind(groups, center_mids)
groups = rbind(groups, wide_players)
groups = rbind(groups, strikers)

# encode positions to numeric labels for model
gf = factor(groups$position)
groups$position = unclass(gf)

# set random seed
set.seed(23)

# split into train and test data
split = sample.split(groups$position, SplitRatio = 0.7)
train_groups = subset(groups, split == "TRUE")
test_groups = subset(groups, split == "FALSE")

# normalize
ppg = preProcess(subset(train_groups, select = -c(position)), method=c("range"))
train_groups = predict(ppg, train_groups)
test_groups = predict(ppg, test_groups)

# chose k as sqrt(n), n=10,000
# note: train and test must NOT include position, cl is position
clf_knn_groups = knn(train=train_groups[,-1], test=test_groups[,-1], cl=train_groups$position, k=99)

# create confusion matrix
predictions = clf_knn_groups
truth = test_groups$position
cmg = confusionMatrix(table(truth, predictions))

# plot confusion matrix
cmgt = cmg$table
colnames(cmgt) = levels(gf)
rownames(cmgt) = levels(gf)
hmg = melt(cmgt)
colnames(hmg) = c("Actual", "Predicted", "value")

# get precision, recall, calculate f1 score
accuracy = cmg$overall[1]
recall = cmg$byClass[,1]
precision = cmg$byClass[,3]
f1 = (2 * precision * recall) / (precision + recall)

# plot precision, recall, and f1 (IN APP USE CHECK BOXES TO SELECT WHICH TO PLOT ON TOP OF EACH OTHER)
metrics_groups = data.frame(precision, recall, f1)
metrics_groups["position"] = levels(gf)
metrics_groups = melt(metrics_groups)

# Shiny App

# Define UI ----
ui <- fluidPage(
  titlePanel("FIFA 2023 App"),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        h1("About the Data"),
        
        p("I selected a data set containing FIFA 2023 cards from the FIFA 2023 video game. The FIFA video game franchise are soccer games that use real players, clubs, and national teams. These players have cards that have an overall rating and display their in game statistics, including things like pace, passing ability, shooting ability, dribbling ability, etc. The game is updated according to real world player performance and transfer market activity. Players can have more than one card, as they often get upgrades after playing well and or receiving some award. The data can be found on kaggle at this link:"), 
        
        a("https://www.kaggle.com/datasets/crxxom/fifa2023-all-cards"),
        
        h2("Motivation"),
        
        p("The motivation behind choosing this data set is simply because I am a soccer fan and enjoy playing FIFA games. I thought it would be interesting to try and predict which position a player is listed as based on their in game stats. I will create and train a machine learning model to classify card position according to the six player statistics: pace, shooting, passing, dribbling, defending, and physicality."),
        
        h1("Explore the Data")
      )
    )
  ),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h4("View the distribution of positions of cards between a selected range of overall ratings."),
        h5("The 14 positions are Striker, Center Forward, Center Attacking Midfield, Center Midfield, Center Defensive Midfield, Center Back, Left and Right Wing, Left and Right Midfield, Left and Right Back, Left and Right Wingback"),
        p("It is clear that the most prevelant positions are center back, striker, and center midfield. This follows what I would expect because they are the most common, basic positions that play down the center of the field, and generally there are multiple of each of these positions on a team. What is interesting to note is that as the sample of players tends towards higher ratings, strikers surpass centerbacks as the most prevalent position. The same relationship exists with wingers and outside backs. This indicates that attacking players are more highly rated than defending players."),
        sliderInput("slider", label = h4("Move sliders to adjust range"), min = 67, 
                    max = 98, value = c(67, 98))
      ),
      mainPanel(
        plotOutput("position_distribution")
      )
    )
  ),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h4("Choose a player statistic to view its distribution across all cards."),
        p("Most of the statistics apear to have a relatively normal distribution. The notable exception is the defensive stat, which has two peaks. I believe this is because the defensive stat is heavily impacted by position. The majority of defensive players have high rated defensive stats, while the majority of attacking players have low defensive stats. This dynamic is different from the other stats like passing where both attacking and defensive players can have a broader range of values."),
        selectInput("stat", h4("Choose a player statistic from the dropdown."), 
                    choices = list("Overall" = 1, "Pace" = 2, "Passing" = 3, "Shooting" = 4, 
                                   "Dribbling" = 5, "Defending" = 6, "Physicality" = 7), selected = 1)
      ),
      mainPanel(
        plotOutput("ratings_distribution")
      )
    )
  ),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h4("View the correlation matrix of player statistics among each position group."),
        p("It is interesting to see which statistics are more highly correleated with each other. Especially among position groups you can see differences such as among defenders defense and physicality are much more correlated than among attackers."),
        radioButtons("radio", h4("Select Position Group"), 
                     choices = list("All Players" = 1, "Defenders" = 2, "Midfielders" = 3, "Forwards" = 4), selected = 1)
      ),
      mainPanel(
        plotOutput("ratings_correlation")
      )
    )
  ),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h4("Search players by name to view their cards and information about club team and league, nationality, and card type."),
        textInput("player", label = h4("Enter Player Name"), value = "")
      ),
      mainPanel(
        dataTableOutput("player_profile")
      )
    )
  ),
  
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        h4("Enter player statistics to predict the position of that card. Try searching a player above and entering one of their cards stats to see if it predicts their position correctly!"),
        numericInput("pac", label = h3("Pace"), value = 80),
        numericInput("sho", label = h3("Shooting"), value = 80),
        numericInput("pas", label = h3("Passing"), value = 80),
        numericInput("dri", label = h3("Dribbling"), value = 80),
        numericInput("def", label = h3("Defending"), value = 80),
        numericInput("phy", label = h3("Physicality"), value = 80)
      ),
      
      mainPanel(
        h1("K-Nearest Neighbors Algorithm"),
        p("The task I want to accomplish is classifying player position based on their six in-game statistics. Because this is a multiclass classification problem I chose the K-Nearest Neighbors (KNN) algorithm which is suitable for this type of task. The algorithm works by choosing the k nearest data points according to some distance measure. The most common is Euclidean distance d = sqrt((x2-x1)^2 + (y2-y1)^2), which is familiar to most from the Pythagorean Theorem. But it can be extended to any number of dimensions, not just two. After calculating the k nearest points, the class labels of those points are used to vote on which class to predict. The majority vote decides the predicted class (ties can be chose randomly). The choice of k is often critical to the success of the model, and usually the optimal k is found using cross-validation. However for this model we simply chose k to be a commonly optimal value of the square root of the number of data points. For this data set that would be 100, but to avoid ties I used k=99. KNN is a good choice for this task because first of all it is a supervised learning algorithm. Supervised means it uses class labels to train on and learn from, which in this task is the position of each card, which we have. Second, KNN does well when classes of data are grouped together according to their fields. I expect this will be the case since defensive players like center backs will likely have high defensive and physical ratings, while attacking players like strikers will likely have high shooting and dribbling ratings, and vice-versa. A third reason KNN suits this task is becasue the player statistics are integer interval data. This means the euclidean distance between them is well defined."),
        p("The KNN model results are shown in the figures below. The model had an accuracy of 56%. The reason this is low is because this is imbalanced multiclass classification. There are much more strikers and center backs than there are right and left wingbacks, so learning wingbacks is inherently difficult because there are not many examples in the data. Also the more classes there are the more challenging it is to correctly label predict them. Even more, in this problem many of the classes are very similar to each other. The attributes of a right back and a left back logically should be similar because they are effectively the same position, just different sides of the field. You can see that the model makes a lot of these types of mistakes in the cnfusion matrix. The model will likely do much better if we combine the positions into position groups like center midfield, outside back, wide player, etc. This is explored below. Accuracy is not typically the metric used to judge multiclass classification however becasue of the reasons above. Instead metrics like precision, recall, and F1 score are usually used. You can see in the graph that the model does really well at identifying center backs. I think that should be expected because as discovered in the data exploration they are the most common position and have tend to have a combination of high defensive and physical rating that distinguishes them from other positions."),
        textOutput("position"),
        fluidRow(
          column(6, plotOutput("confusion")),
          column(6, plotOutput("metrics"))
        )
      )
    )
  ),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        numericInput("gpac", label = h3("Pace"), value = 80),
        numericInput("gsho", label = h3("Shooting"), value = 80),
        numericInput("gpas", label = h3("Passing"), value = 80),
        numericInput("gdri", label = h3("Dribbling"), value = 80),
        numericInput("gdef", label = h3("Defending"), value = 80),
        numericInput("gphy", label = h3("Physicality"), value = 80)
      ),
      
      mainPanel(
        h1("KNN Revisited With Position Groups"),
        p("Now the positions have been organized by their group. RBs, LBs, RWBs, and LWBs are now Outside Backs (OB). Center Backs remain the same. Center Midfielders now include CDMs and CAMs. LMs, RMs, LWs, and RWs are now Wingers (WG). Strikers now include CFs. The accuracy of this model is 77%, which is a big imporvement on the previous model. This is expected largely because now there are only 5 classes instead of 14. But you can see from the confusion matrix and bar graph of the precision, recall, and F1 score that the model is now much better at predicting each class. In a practical sense combining positions into position groups is a more accurate way of thinking about the differences in skills between them. As I said before positions with left and right sides are really the same position, in that they demand the same thing, but they are just on different sides of the field."),
        textOutput("position_groups"),
        column(6, plotOutput("confusion_groups")),
        column(6, plotOutput("metrics_groups"))
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$position_distribution <- renderPlot({
    sbst = subset(fifa, (rating >= input$slider[1] & rating <= input$slider[2]), select = c(position))
    tbl = table(sbst)
    tbl = tbl[order(tbl)]
    barplot(tbl, horiz=TRUE, las=1, main="Position Distribution")
  })
  
  output$ratings_distribution <- renderPlot({
    if (input$stat == 1) {
      hist(ratings$rating, xlab="Overall", main="Overall Rating Distribution")
    }
    else if (input$stat == 2) {
      hist(ratings$PAC, xlab="Pace", main="Pace Distribution")
    }
    else if (input$stat == 3) {
      hist(ratings$PAS, xlab="Passing", main="Passing Distribution")
    }
    else if (input$stat == 4) {
      hist(ratings$SHO, xlab="Shooting", main="Shooting Distribution")
    }
    else if (input$stat == 5) {
      hist(ratings$DRI, xlab="Dribbling", main="Dribbling Distribution")
    }
    else if (input$stat == 6) {
      hist(ratings$DEF, xlab="Defending", main="Defending Distribution")
    }
    else if (input$stat == 7) {
      hist(ratings$PHY, xlab="Physicality", main="Physical Distribution")
    }
  })
  
  output$ratings_correlation <- renderPlot({
    if (input$radio == 1) {
      plot_cor_ratings(all, "All Players Statistical Correlations")
    }
    else if (input$radio == 2) {
      plot_cor_ratings(def, "Defenders Statistical Correlations")
    }
    else if (input$radio == 3) {
      plot_cor_ratings(mid, "Midfielders Statistical Correlations")
    }
    else if (input$radio == 4) {
      plot_cor_ratings(forw, "Forwards Statistical Correlations")
    }
  })
  
  output$player_profile <- renderDataTable({
    profile = fifa[grepl(input$player, fifa$name, ignore.case = TRUE),]
    profile
  })
  
  output$position <- renderText({
    stats = c(input$pac, input$sho, input$pas, input$dri, input$def, input$phy)
    stats = matrix(stats, 1, 6)
    stats = as.data.frame(stats)
    colnames(stats) = colnames(train[-1])
    
    # normalize
    stats = predict(pp, stats)
    prediction = knn(train=train[,-1], test=stats, cl=train$position, k=99)
    
    levels(fact)[prediction]
  })
  
  output$confusion <- renderPlot({
    ggplot(hm, aes(Predicted,Actual)) + geom_tile(aes(fill = value)) + geom_text(aes(label = value)) + ggtitle("Confusion Matrix") + scale_fill_gradient2(low = "white", high = "darkgreen")
  })
  
  output$metrics <- renderPlot({
    ggplot(metrics, aes(fill=variable, y=value, x=position)) + 
      geom_bar(position="identity", stat="identity", alpha = 0.4) + ggtitle("Precision, Recall, and F1 Scores")
  })
  
  output$position_groups <- renderText({
    gstats = c(input$gpac, input$gsho, input$gpas, input$gdri, input$gdef, input$gphy)
    gstats = matrix(gstats, 1, 6)
    gstats = as.data.frame(gstats)
    colnames(gstats) = colnames(train_groups[-1])
    
    # normalize
    gstats = predict(ppg, gstats)
    prediction = knn(train=train_groups[,-1], test=gstats, cl=train_groups$position, k=99)
    
    levels(gf)[prediction]
  })
  
  output$confusion_groups <- renderPlot({
    ggplot(hmg, aes(Predicted,Actual)) + geom_tile(aes(fill = value)) + geom_text(aes(label = value)) + ggtitle("Confusion Matrix") + scale_fill_gradient2(low = "white", high = "darkgreen")
  })
  
  output$metrics_groups <- renderPlot({
    ggplot(metrics_groups, aes(fill=variable, y=value, x=position)) + 
      geom_bar(position="identity", stat="identity", alpha = 0.4) + ggtitle("Precision, Recall, and F1 Scores")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

