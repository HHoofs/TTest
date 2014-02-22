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
  if(input$ttest){
    t.val <- (input$one_X-input$one_u)/(input$one_sd/sqrt(input$one_N/1))
  }
    t.val
})

t.crit <- reactive({
  if(input$ttest){
    if(input$sided == "=") value <- qt(p=input$alpha/2, input$one_N-1)
    if(input$sided == "<") value <- qt(p=input$alpha, input$one_N-1)
    if(input$sided == ">") value <- qt(p=input$alpha, input$one_N-1, lower.tail=FALSE)
  }
  value
  })

densit <- reactive({
   rangeX <- c(-input$lim,input$lim)
   xx <- seq(rangeX[1],rangeX[2],length.out=1000)
   data.frame(x=xx, y=dt(x=xx, df=(input$one_N-1)))
 })
 
pval <- reactive({
  if(input$sided == "=") pvalue <- pt(-abs(t.value()), input$one_N)*2
  if(input$sided == "<") pvalue <- pt(t.value(), input$one_N) 
  if(input$sided == ">") pvalue <- pt(t.value(), input$one_N,lower.tail =FALSE) 
  pvalue
})

hypo <- reactive({
  if(pval() < input$alpha) "HA" else "H0"
})

polygH0 <- reactive({
   polygon <- densit()
   
   if(input$sided == "=") {
     testK <- polygon$x > -abs(t.crit()) & polygon$x < abs(t.crit())
   }
   if(input$sided == "<") testK <- polygon$x > -abs(t.crit())  
   if(input$sided == ">") testK <- polygon$x < abs(t.crit())  
   
   polygon <- polygon[testK,]
   data.frame(x=c(polygon$x, rev(polygon$x)), 
              y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
              )
 })

polygL <- reactive({
  
  polygon <- densit()
  
  if(input$sided == "=") testK <- polygon$x < -abs(t.crit()) 
  if(input$sided == "<") testK <- polygon$x < -abs(t.crit())  
  if(input$sided == ">") testK <- 0
  
  polygon <- polygon[testK,]
  data.frame(x=c(polygon$x, rev(polygon$x)), 
             y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
  )
  
})

polygR <- reactive({

  polygon <- densit()
  
  if(input$sided == "=") testK <- polygon$x > abs(t.crit()) 
  if(input$sided == ">") testK <- polygon$x > abs(t.crit())  
  if(input$sided == "<") testK <- 0
  
  polygon <- polygon[testK,]
  data.frame(x=c(polygon$x, rev(polygon$x)), 
             y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
  )
  
})

polygProp <- reactive({
  polygon <- densit()
  
  if(input$sided == "=") testK <- polygon$x < -abs(t.value())
  if(input$sided == "<") testK <- polygon$x < t.value()  
  if(input$sided == ">") testK <- polygon$x > t.value()
  
  polygon <- polygon[testK,]
  data.frame(x=c(polygon$x, rev(polygon$x)), 
             y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
  )
})

polygSpss <- reactive({
  polygon <- densit()
  
  testK <- polygon$x > abs(t.value())
  
  polygon <- polygon[testK,]
  data.frame(x=c(polygon$x, rev(polygon$x)), 
             y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
  )
})

output$plot <- renderPlot({  
  # Plot
  t.cr <- abs(t.crit())
  
  p <-
    ggplot(densit(),aes(x=x,y=y)) +
    geom_line(lwd=1.5)
  
  if(input$sided == "="){
    p <-
      p +
      geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.4, fill="darkgreen") +
      geom_polygon(aes(x=x,y=y),data=polygL(),alpha=.4, fill="darkred") +
      geom_polygon(aes(x=x,y=y),data=polygR(),alpha=.4, fill="darkred") +
      geom_segment(x=-t.cr,xend=-t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) +
      geom_segment(x=t.cr,xend=t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) +
      annotate("text",label=paste("T[krit]==",round(t.cr,2)), x = t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) +
      annotate("text",label=paste("T[krit]==",round(-t.cr,2)), x = -t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) 
    
    
  }

  if(input$sided == "<"){
    p <-
      p +
      geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.4, fill="darkgreen") +
      geom_polygon(aes(x=x,y=y),data=polygL(),alpha=.4, fill="darkred") +
      geom_segment(x=-t.cr,xend=-t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5)
  }


  if(input$sided == ">"){
    p <-
      p +
      geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.4, fill="darkgreen") +
      geom_polygon(aes(x=x,y=y),data=polygR(),alpha=.4, fill="darkred") +
      geom_segment(x=t.cr,xend=t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5)
  }
  
# Adjust t value placement if it cross the border!
if(t.value() < 0) par_hjust <- 0 else par_hjust <- 1

par_x   <- t.value() 
par_lab <- paste("T[waarde]==",round(t.value(),2))  

if(t.value() <  min(densit()$x)) {
  par_x   <- min(densit()$x) 
  par_lab <- paste("{phantom(0)%<-%T[waarde]}==",round(t.value(),2))}

if(t.value() > max(densit()$x)) {
  par_x   <- max(densit()$x) 
  par_lab <- paste("{phantom(0)%->%T[waarde]}==",round(t.value(),2))}





# input the adjusted v-value placement
p <- 
  p + 
  geom_vline(xintercept=par_x, lwd=1.5,lty=2) +
  annotate("text",label=par_lab, x =  par_x, y = max(densit()$y)/2, hjust = par_hjust, size=8,parse=TRUE)

  

p <- 
  p +   
  theme_bw(20) +
  ggtitle("t-verdeling") +
  theme(axis.text.y = element_blank(),axis.title.y=element_blank()) +
  scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + scale_y_continuous(expand=c(0,0))  
   
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt) 
})

output$plotH <- renderPlot({
  # start new page
  plot.new() 
  
  # setup layout
  gl <- grid.layout(nrow=1, ncol=2)
  # grid.show.layout(gl)
  
  # setup viewports
  vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
  vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
  
  # init layout
  pushViewport(viewport(layout=gl))
  # access the first position
  pushViewport(vp.1)
  
  # start new base graphics in first viewport
  par(new=TRUE, fig=gridFIG())
  

  # done with the first viewport
  popViewport()
  
  # move to the next viewport
  pushViewport(vp.2)
  
  #text
  if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
  
  
})

output$plot2 <- renderPlot({  
  # Plot
  t.va <- t.value()
  
  #   Adjust t value placement if it cross the border!
  par_x <- NULL
  par_lab <- NULL
  par_hjust <- NULL
  
  par_x[1]   <- t.value() 
  par_lab[1] <- paste("T[waarde]==",round(t.value(),2))
  par_lab[2] <- paste("T[waarde]==",round(-t.value(),2))  
  
  if(t.value() <  min(densit()$x)) {
    par_x[1]   <- min(densit()$x) 
    par_lab[1] <- paste("{phantom(0)%<-%T[waarde]}==",round(-abs(t.value()),2))
    if(input$sided == "=") par_lab[2] <- paste("{phantom(0)%->%T[waarde]}==",round(abs(t.value()),2))
  }
  
  if(t.value() >  max(densit()$x)) {
    par_x[1]   <- max(densit()$x) 
    par_lab[1] <- paste("{phantom(0)%->%T[waarde]}==",round(abs(t.value()),2))
    if(input$sided == "=") par_lab[2] <- paste("{phantom(0)%<-%T[waarde]}==",round(-abs(t.value()),2))
  }
  
  par_x[2] <- -par_x[1]
  
  if(par_x[1] < 0) par_hjust[1] <- 0 else  par_hjust[1] <- 1
  if(par_x[2] < 0) par_hjust[2] <- 0 else  par_hjust[2] <- 1
  

  
  p <-
    ggplot(densit(),aes(x=x,y=y)) +
    geom_line(lwd=1.5)
  
  if(input$sided == "="){
    p <-
      p +
      geom_polygon(aes(x=x,y=y),data=polygProp(), alpha=.4, fill="darkblue") +
      geom_polygon(aes(x=x,y=y),data=data.frame(x=-polygProp()$x,y=polygProp()$y), alpha=.4, fill="darkblue") +
      geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) +
      geom_segment(x=par_x[2], xend=par_x[2], y=-Inf,yend=densit()[which.min(abs(par_x[2]-densit()$x)),"y"], lwd=1.5) +
      annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[1]) +
      annotate("text",label=par_lab[2], x = par_x[2], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[2]) 
  } else {
    p <-
      p +
      geom_polygon(aes(x=x,y=y),data=polygProp(), alpha=.4, fill="darkblue") +
      geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) +
      annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[1]) 
  }
   
  p <- 
    p +   
    theme_bw(20) +
    ggtitle("t-verdeling") +
    theme(axis.text.y = element_blank(),axis.title.y=element_blank()) +
    scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p =",round(pval(),3)),vjust=1.5,size=8,hjust=0)
    
  
  if(input$sided == "="){
    p <- p +   annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig."),vjust=3,size=8,hjust=0)

  }
  

  if(!input$sided == "="){
    if(pval() > .05) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = 1-sig./2"),vjust=3,size=8,hjust=0)
    if(pval() < .05) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig./2"),vjust=3,size=8,hjust=0)
    
  }
#   round(pt(-abs(t.value()),input$one_N-1)*2,3))
#   
#   annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste("p =",round(pval(),3)),vjust=1.5,size=8,hjust=-.25)
  
  
  
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt) 
})

output$plot3 <- renderPlot({  
  t.va <- t.value()
  
  #   Adjust t value placement if it cross the border!
  par_x <- NULL
  par_lab <- NULL
  par_hjust <- NULL
  
  par_x[1]   <- t.value() 
  par_lab[1] <- paste("T[waarde]==",round(t.value(),2))
  par_lab[2] <- paste("T[waarde]==",round(-t.value(),2))  
  
  if(t.value() <  min(densit()$x)) {
    par_x[1]   <- min(densit()$x) 
    par_lab[1] <- paste("{phantom(0)%<-%T[waarde]}==",round(-abs(t.value()),2))
    par_lab[2] <- paste("{phantom(0)%->%T[waarde]}==",round(abs(t.value()),2))
  }
  
  if(t.value() >  max(densit()$x)) {
    par_x[1]   <- max(densit()$x) 
    par_lab[1] <- paste("{phantom(0)%->%T[waarde]}==",round(abs(t.value()),2))
    par_lab[2] <- paste("{phantom(0)%<-%T[waarde]}==",round(-abs(t.value()),2))
  }
  
  par_x[2] <- -par_x[1]
  
  if(par_x[1] < 0) par_hjust[1] <- 0 else  par_hjust[1] <- 1
  if(par_x[2] < 0) par_hjust[2] <- 0 else  par_hjust[2] <- 1
  
  
  
  p <-
    ggplot(densit(),aes(x=x,y=y)) +
    geom_line(lwd=1.5) +
    geom_polygon(aes(x=x,y=y),data=polygSpss(), alpha=.4, fill="darkorange") +
    geom_polygon(aes(x=x,y=y),data=data.frame(x=-polygSpss()$x,y=polygSpss()$y), alpha=.4, fill="darkorange") +
    geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) +
    geom_segment(x=par_x[2], xend=par_x[2], y=-Inf,yend=densit()[which.min(abs(par_x[2]-densit()$x)),"y"], lwd=1.5) +
    annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=4,parse=TRUE, hjust= par_hjust[1]) +
    annotate("text",label=par_lab[2], x = par_x[2], y = -Inf, vjust = 1.5,size=4,parse=TRUE, hjust= par_hjust[2]) +
    annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste("Sig.=",round(pt(-abs(t.value()),input$one_N-1)*2,3)),vjust=1.5,size=4,hjust=-.25) 
    
  
  p <- 
    p +   
    theme_bw(10) +
    ggtitle("SPSS") +
    theme(axis.text.y = element_blank(),axis.title.y=element_blank()) +
    scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + scale_y_continuous(expand=c(0,0))  
  
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  
  
  # start new page
  plot.new() 
  
  # setup layout
  gl <- grid.layout(nrow=1, ncol=2)
  # grid.show.layout(gl)
  
  # setup viewports
  vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
  vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
  
  # init layout
  pushViewport(viewport(layout=gl))
  # access the first position
  pushViewport(vp.1)
  
  # start new base graphics in first viewport
  par(new=TRUE, fig=gridFIG())
  
  grid.draw(gt) 
  
  # done with the first viewport
  popViewport()
  
  # move to the next viewport
  pushViewport(vp.2)
  
  #text
  grid.text(bquote({t == frac( .(input$one_X) - .(input$one_u),frac(.(input$one_sd),sqrt(.(input$one_N) - 1)) )} == 2), gp=gpar(fontsize=40))
  
#   if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
  
})


output$summar <- renderPrint({
  paste(input$one_X,input$one_u,input$s,input$one_N)
})
  output$summar <- renderPrint({
    paste(input$one_X,input$one_u,input$s,input$one_N)
  })

})
