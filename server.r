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
      if(input$sided == "=") value <- qt(p=as.numeric(input$alpha)/2, input$one_N-1)
      if(input$sided == "<") value <- qt(p=as.numeric(input$alpha), input$one_N-1)
      if(input$sided == ">") value <- qt(p=as.numeric(input$alpha), input$one_N-1, lower.tail=FALSE)
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
        geom_segment(x=-t.cr,xend=-t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) +
        annotate("text",label=paste("T[krit]==",round(-t.cr,2)), x = -t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE)     
    }
    
    
    if(input$sided == ">"){
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.4, fill="darkgreen") +
        geom_polygon(aes(x=x,y=y),data=polygR(),alpha=.4, fill="darkred") +
        geom_segment(x=t.cr,xend=t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) +
        annotate("text",label=paste("T[krit]==",round(t.cr,2)), x = t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE)    
    }
    
    # Adjust t value placement if it cross the border!
    if(t.value() < 0) par_hjust <- 0 else par_hjust <- 1
    
    par_x   <- t.value() 
    par_lab <- paste("T[w]==",round(t.value(),2))  
    
    if(t.value() <  min(densit()$x)) {
      par_x   <- min(densit()$x) 
      par_lab <- paste("{phantom(0)%<-%T[w]}==",round(t.value(),2))}
    
    if(t.value() > max(densit()$x)) {
      par_x   <- max(densit()$x) 
      par_lab <- paste("{phantom(0)%->%T[w]}==",round(t.value(),2))}
    
    
    
    
    
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
    grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
    if(input$answ2){
      if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
    }
  })
  
  output$plot2 <- renderPlot({  
    # Plot
    t.va <- t.value()
    
    #   Adjust t value placement if it cross the border!
    par_x <- NULL
    par_lab <- NULL
    par_hjust <- NULL
    
    par_x[1]   <- t.value() 
    par_lab[1] <- paste("t[w]==",round(t.value(),2))
    par_lab[2] <- paste("t[w]==",round(-t.value(),2))  
    
    if(t.value() <  min(densit()$x)) {
      par_x[1]   <- min(densit()$x) 
      par_lab[1] <- paste("{phantom(0)%<-%T[w]}==",round(-abs(t.value()),2))
      if(input$sided == "=") par_lab[2] <- paste("{phantom(0)%->%T[w]}==",round(abs(t.value()),2))
    }
    
    if(t.value() >  max(densit()$x)) {
      par_x[1]   <- max(densit()$x) 
      par_lab[1] <- paste("{phantom(0)%->%t[w]}==",round(abs(t.value()),2))
      if(input$sided == "=") par_lab[2] <- paste("{phantom(0)%<-%T[w]}==",round(-abs(t.value()),2))
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
      scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + scale_y_continuous(expand=c(0,0),limits=c(-(max(densit()$y)/20),max(densit()$y))) +
      annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p =",round(pval(),3)),vjust=1.5,size=8,hjust=0)
    
    
    if(input$sided == "="){
      p <- p +   annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig."),vjust=3,size=8,hjust=0)
      
    }
    
    
    if(input$sided == "<"){
      if(t.value() > 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = 1-sig./2"),vjust=3,size=8,hjust=0)
      if(t.value() <= 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig./2"),vjust=3,size=8,hjust=0)
    }
    
    if(input$sided == ">"){
      if(t.value() <= 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = 1-sig./2"),vjust=3,size=8,hjust=0)
      if(t.value() > 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig./2"),vjust=3,size=8,hjust=0)
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
    par_lab[1] <- paste("T[w]==",round(t.value(),2))
    par_lab[2] <- paste("T[w]==",round(-t.value(),2))  
    
    if(t.value() <  min(densit()$x)) {
      par_x[1]   <- min(densit()$x) 
      par_lab[1] <- paste("{phantom(0)%<-%T[w]}==",round(-abs(t.value()),2))
      par_lab[2] <- paste("{phantom(0)%->%T[w]}==",round(abs(t.value()),2))
    }
    
    if(t.value() >  max(densit()$x)) {
      par_x[1]   <- max(densit()$x) 
      par_lab[1] <- paste("{phantom(0)%->%T[w]}==",round(abs(t.value()),2))
      par_lab[2] <- paste("{phantom(0)%<-%T[w]}==",round(-abs(t.value()),2))
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
    grid.draw(gt) 
    popViewport()
    
    pushViewport(vp.2)
    grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
    if(input$answ2){
      if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
    }
  })
  
  output$CI <- renderPlot({
    Xval <- input$one_X
    Tval <- abs(qt(as.numeric(input$alpha),input$one_N-1)*(input$one_sd/sqrt(input$one_N-1)))
    se <- data.frame(y="",x=Xval, xlow=Xval - Tval, xhigh=Xval +Tval)
    p <- ggplot(se, aes(x=y, y=x, ymin = xlow, ymax=xhigh)) +  
      geom_pointrange(lwd=1.5) +
      geom_errorbar(width = 0.5, lwd = 1.5)  + 
      geom_hline(yintercept=input$one_u) + 
      annotate("text",y=se$xlow,  x=.7,label=round(se$xlow,2)) +
      annotate("text",y=se$xhigh, x=.7,label=round(se$xhigh,2)) +
      annotate("text",y=se$x, x=.95,label=round(se$x,2)) +
      scale_y_continuous(breaks=input$one_u, labels=list(bquote(mu == .(input$one_u))),name="") + scale_x_discrete(name="")
    
    p <- p + 
      ggtitle("Betrouwbaarheids Interval") +
      theme_bw(20)  +
      coord_flip()
    
    print(p)
  })
  
  output$CI_info <- renderPlot({
    gl <- grid.layout(nrow=1, ncol=2)
    vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
    vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
    
    # init layout
    pushViewport(viewport(layout=gl))
    # access the first position
    pushViewport(vp.1)    
    grid.text(bquote(group("(",list(
      bar(X)-t[list(alpha/2,N-1)]*frac(s,sqrt(N-1)),
      bar(X)+t[list(alpha/2,N-1)]*frac(s,sqrt(N-1))
    ),")")),
    gp=gpar(fontsize=20),vjust=-2)
    
    grid.text(bquote(group("(",list(
      .(input$one_X)-.(round(abs(t.crit()),2))*frac(.(input$one_sd),sqrt(.(input$one_N)-1)),
      .(input$one_X)+.(round(abs(t.crit()),2))*frac(.(input$one_sd),sqrt(.(input$one_N)-1)) 
    ),")")),
    gp=gpar(fontsize=20))
    
    grid.text(bquote(group("(",list(
      .(round(input$one_X-abs(t.crit())*(input$one_sd/sqrt(input$one_N-1)),2) ),
      .(round(input$one_X+abs(t.crit())*(input$one_sd/sqrt(input$one_N-1)),2) )
    ),")")),
    gp=gpar(fontsize=20),vjust=4)
    popViewport()
    
    pushViewport(vp.2)
    grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
    if(input$answ2){
      if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
    }
  })
  
  output$hypopaar <- renderPlot({
    plot.new() 
    
    # setup layout
    gl <- grid.layout(nrow=6, ncol=2)
    # grid.show.layout(gl)
    
    # setup viewports
    vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1)
    vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
    vp.3 <- viewport(layout.pos.col=1, layout.pos.row=2) 
    vp.4 <- viewport(layout.pos.col=1, layout.pos.row=3) 
    vp.5 <- viewport(layout.pos.col=1, layout.pos.row=4) 
    vp.6 <- viewport(layout.pos.col=1, layout.pos.row=5) 
    vp.7 <- viewport(layout.pos.col=1, layout.pos.row=6) 
    vp.8 <- viewport(layout.pos.col=2, layout.pos.row=3:6) 
    
    
    
    
    # init layout
    pushViewport(viewport(layout=gl))
    # access the first position
    # done with the first viewport
    if(input$sided == "="){
      pushViewport(vp.1)  
      grid.text(bquote('H'[0]: mu==.(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
      pushViewport(vp.2)      
      grid.text(bquote('H'[1]: mu != .(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
    }
    if(input$sided == "<"){
      pushViewport(vp.1)  
      grid.text(bquote('H'[0]: mu>=.(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
      pushViewport(vp.2)     
      grid.text(bquote('H'[1]: mu < .(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
    }
    if(input$sided == ">"){
      pushViewport(vp.1)  
      grid.text(bquote('H'[0]: mu<=.(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
      pushViewport(vp.2)    
      grid.text(bquote('H'[1]: mu > .(input$one_u)), gp=gpar(fontsize=40), hjust=1)
      popViewport()
    }
    
    pushViewport(vp.3)    
    grid.text(bquote(alpha == .(as.numeric(input$alpha))), gp=gpar(fontsize=40), hjust=1)
    popViewport()
    pushViewport(vp.4)    
    grid.text(bquote(mu == .(as.numeric(input$one_u))), gp=gpar(fontsize=40), hjust=1)
    popViewport()
    pushViewport(vp.5)    
    grid.text(bquote(bar(X) == .(as.numeric(input$one_X))), gp=gpar(fontsize=40), hjust=1)
    popViewport()
    pushViewport(vp.6)    
    grid.text(bquote(s == .(as.numeric(input$one_sd))), gp=gpar(fontsize=40), hjust=1)
    popViewport()
    pushViewport(vp.7)    
    grid.text(bquote(N == .(as.numeric(input$one_N))), gp=gpar(fontsize=40), hjust=1)
    popViewport()
    pushViewport(vp.8)    
    grid.text(bquote({t[w] == frac( .(input$one_X) - .(input$one_u),.(input$one_sd)/sqrt(.(input$one_N) - 1) )} == .(round(t.value()),2)), 
              gp=gpar(fontsize=40))
    popViewport()
    
  })
  
  
  output$summar <- renderPrint({
    paste(input$one_X,input$one_u,input$s,input$one_N)
  })
  
  
})
