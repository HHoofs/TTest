require(MASS)
require(psych)
# library(Hmisc)
# require(reshape)
# require(ggplot2)
library(grid)
# library(gridExtra)
# require(ggthemes)
library(ggplot2)
# require(rgl)
# require(stringr)

gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

color <- function(x){
  y <- gsub2(pattern=c(1,2), replacement=c("#F8766D", "#00BFC4"), x=x)
  y
}

color2 <- function(x){
  y <- gsub2(pattern=c(1,2), replacement=c("#F8766D", "#00BFC4"), x=x)
  y
}

shinyServer(function(input, output) {

t.value <- reactive({
  if(input$ttest == TRUE){
    t.val <- (input$one_X-input$one_u)/(input$s/sqrt(input$one_N/1))
  }

})

t.crit <- reactive({
  if(input$ttest == TRUE){
    if(input$method == "=") t.crit <- qt(p=input$alpha/2, input$one_N-1)
    if(input$method == "<") t.crit <- qt(p=input$alpha, input$one_N-1)
    if(input$method == "<") t.crit <- qt(p=input$alpha, input$one_N-1, lower.tail=FALSE)
  }
})

densit <- reactive({
   rangeX <- c(-20,20)
#    if(rangeX[1] < input$lim[1]) rangeX[1] <- input$lim[1]
#    if(rangeX[2] > input$lim[2]) rangeX[2] <- input$lim[2]
   
   xx <- seq(rangeX[1],rangeX[2],length.out=1000)
   data.frame(x=xx, y=dt(x=xx, df=(input$one_N/1)))
 })
 

polygP <- reactive({
   polygon <- densit()
   
   if(input$method == "=") testK <- polygon$x > t.crit() & polygon$x < -(t.crit())
   if(input$method == "<") testK <- polygon$x > t.crit()  
   if(input$method == ">") testK <- polygon$x < t.crit() 
   
   polygon <- polygon[testK,]
   data.frame(x=c(polygon$x, rev(polygon$x)), 
              y=c(polygon$y, rep(0, times=length(polygon$x)))
              )
 })

polygN <- reactive({
  
  polygon <- densit()
  
  if(input$method == "=") testK <- polygon$x < t.crit() & polygon$x > -(t.crit())
  if(input$method == "<") testK <- polygon$x < t.crit()  
  if(input$method == ">") testK <- polygon$x > t.crit() 
  
  polygon <- polygon[testK,]
  data.frame(x=c(polygon$x, rev(polygon$x)), 
             y=c(polygon$y, rep(0, times=length(polygon$x)))
  )
  
})

# polyg <- reactive({
#   polygonP <- densit()
#   lab <- pnorm(input$refferP,mean(datas()$x),sd(datas()$x))
#   lab[2] <- 1-lab
#   if(polygonP$x[which.max(polygonP$y)] > input$refferP) {
#     testK <- polygonP$x > input$refferP } else {
#       testK <- polygonP$x < input$refferP
#     }
#   polygonP <- polygonP[testK,]
#   polygonP <- 
#   data.frame(x=c(polygonP$x, rev(polygonP$x)), 
#              y=c(polygonP$y, rep(0, times=length(polygonP$x))),
#              P=as.character(round(max(lab),3)))
# 
#   polygonN <- densit()
#   if(polygonN$x[which.max(polygonN$y)] > input$refferP) {
#     testK <- polygonN$x < input$refferP } else {
#       testK <- polygonN$x > input$refferP
#     }
#   polygonN <- polygonN[testK,]
#   polygonN <- 
#   data.frame(x=c(polygonN$x, rev(polygonN$x)), 
#              y=c(polygonN$y, rep(0, times=length(polygonN$x))),
#              P=as.character(round(min(lab),3)))
#   
#   poly <- rbind(polygonP,polygonN)
#   
#   if(!input$difdens){
#     pool <- densit()
#     poly <- 
#       data.frame(x=c(pool$x, rev(pool$x)), 
#                  y=c(pool$y, rep(0, times=length(pool$x))),
#                  P="")
#   }
#   
#   poly
#   
# })

output$plot <- renderPlot({
  plot <-
    ggplot(data=densit(), aes(x=x,y=y)) +
    geom_line(lwd=1.5) 
  
  print(plot)
})

  output$summar <- renderPrint({
    describe(polygon())
  })

})
