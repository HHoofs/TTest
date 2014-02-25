# The MIT License (MIT)
# 
# Copyright (c) 2014 Huub
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the 'Software' ), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Librarys ----------------------------------------------------------------
library(grid)
library(ggplot2)
require(xtable)
require(stringr)

# Functions ---------------------------------------------------------------
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

# df tabel ----------------------------------------------------------------
# Alpha's
levels <- c(.005,.01,.025,.05,.1,.25,.5,.75,.9,.95,.975,.99,.995)
# df
df     <- c(1:30,40,50,60,80,100,10000)
# Tabel defineren
ttable <- matrix(nrow=length(df),ncol=length(levels))

# Tabel invoeren
for(i in 1:length(levels)){
  ttable[,i] <- qt(levels[i],df)
  if(levels[i]==.5) ttable[,i] <- 0
}

# Data frame maken
ttable <- data.frame(ttable)
# correcte namen geven aan rijen en kolommen
rownames(ttable) <- df
rownames(ttable)[length(df)] <- "z"
colnames(ttable) <- levels

# shiny deel --------------------------------------------------------------
shinyServer(function(input, output) {
  
  # t waarde uitrekenen voor gegeven input --------------------------------------------------------------
  t.value <- reactive({
    # Een sample ttoets
    if(input$test == "onettest"){
      t.val <- (input$one_X-input$one_u)/(input$one_sd/sqrt(input$one_N))
    }
    # Twee sample ttoets
    if(input$test == "twottest"){
      t.val <- (input$two_X1 - input$two_X2)/sqrt( (input$two_sd1^2)/input$two_N1 + (input$two_sd2^2)/input$two_N2)
    }
    t.val
  })
  
  # kritieke t waarde uitrekenen voor gegeven input --------------------------------------------------------------
  t.crit <- reactive({
    # Een sample ttoets
    if(input$test == "onettest"){
      if(input$sided == "=") value <- qt(p=as.numeric(input$alpha)/2, input$one_N-1) # a/2 omdat tweezijdig
      if(input$sided == "<") value <- qt(p=as.numeric(input$alpha), input$one_N-1)
      if(input$sided == ">") value <- qt(p=as.numeric(input$alpha), input$one_N-1, lower.tail=FALSE) #lower tail omdat het van rechts komt
    }
    # Twee sample ttoets
    if(input$test == "twottest"){
      if(input$sided == "=") value <- qt(p=as.numeric(input$alpha)/2, input$two_N1 + input$two_N2 -2) # a/2 omdat tweezijdig
      if(input$sided == "<") value <- qt(p=as.numeric(input$alpha), input$two_N1 + input$two_N2 -2)
      if(input$sided == ">") value <- qt(p=as.numeric(input$alpha), input$two_N1 + input$two_N2 -2, lower.tail=FALSE) #lower tail omdat het van rechts komt
    }
    value
  })
  
  # df waarde voor de tabel (omdat hier niet alle waardes instaan) --------------------------------------------------------------
  table.df <- reactive({
    # Een sample ttoets
    if(input$test == "onettest"){
      if(input$one_N - 1 >= 200) value <- "z" #omdat dit richting de z verdeling gaat
      if(input$one_N - 1 < 200) value <- 100
      if(input$one_N - 1 < 100) value <- 80
      if(input$one_N - 1 < 80)  value <- 60
      if(input$one_N - 1 < 60) value <-  50
      if(input$one_N - 1 < 50) value <-  40
      if(input$one_N - 1 < 40) value <-  30
      if(input$one_N - 1 < 30) value <- input$one_N - 1
    }
    # Twee sample ttoets
    if(input$test == "twottest"){
      if(input$two_N1 + input$two_N2 -2 >= 200) value <- "z" #omdat dit richting de z verdeling gaat
      if(input$two_N1 + input$two_N2 -2 < 200) value <- 100
      if(input$two_N1 + input$two_N2 -2 < 100) value <- 80
      if(input$two_N1 + input$two_N2 -2 < 80)  value <- 60
      if(input$two_N1 + input$two_N2 -2 < 60) value <-  50
      if(input$two_N1 + input$two_N2 -2 < 50) value <-  40
      if(input$two_N1 + input$two_N2 -2 < 40) value <-  30
      if(input$two_N1 + input$two_N2 -2 < 30) value <- input$two_N1 + input$two_N2 -2 #-2 vanwege twee groepen
    }
    value
  })
  
  # exacte df waarmee gerekend wordt in alle formules --------------------------------------------------------------
  df <- reactive({
    # Een sample ttoets
    if(input$test == "onettest") valdf <- input$one_N-1
    # Twee sample ttoets
    if(input$test == "twottest") valdf <-  input$two_N1 + input$two_N2 -2
    valdf
  })
  
  # Creeeren basis density frame -------------------------------------------------------------- 
  densit <- reactive({
    # input afhankelijk van opties (default=5)
    rangeX <- c(-input$lim,input$lim)
    tvalues <- seq(rangeX[1],rangeX[2],length.out=1000)
    # range invoeren om zodoende bij iedere waarde bijbehorende density te krijgen 
    data.frame(x=tvalues, y=dt(x=tvalues, df=df()))
  })
  
  # P-waarde berekenen -------------------------------------------------------------- 
  pval <- reactive({
    if(input$sided == "=") pvalue <- pt(-abs(t.value()), df())*2 # Tweezijdig dus daarom waarschijnlijkheid keer twee
    if(input$sided == "<") pvalue <- pt(t.value(), df()) 
    if(input$sided == ">") pvalue <- pt(t.value(), df(),lower.tail =FALSE) # gaat naar rechts toe
    pvalue
  })
  
  # Hypothese test -------------------------------------------------------------- 
  hypo <- reactive({
    if(pval() < input$alpha) "HA" else "H0" # Als p < a dan Ha anders H0
  })
  
  # Kritieke gebied Polygon dat in het midden hoort -------------------------------------------------------------
  polygH0 <- reactive({
    polygon <- densit()
    # Bepalen t-waardes die onder H0 vallen
    if(input$sided == "=") testK <- polygon$x > -abs(t.crit()) & polygon$x < abs(t.crit()) # Tweezijdig (< rechtse waarde, > linkse waarde)
    if(input$sided == "<") testK <- polygon$x > -abs(t.crit())                             # Linkszijdig (> linkse waarde)
    if(input$sided == ">") testK <- polygon$x < abs(t.crit())                              # Rechtsszijdig (< rechtse waarde)
    # Subset van density frame voor t waardes binnen H0
    polygon <- polygon[testK,]
    # Polygon maken, met een beetje verhoging -(max(densit()$y)/20) is eigenlijk 0
    data.frame(x=c(polygon$x, rev(polygon$x)), 
               y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
    )
  })
  
  # Kritieke gebied Polygon dat links hoort onder Ha -------------------------------------------------------------
  polygL <- reactive({
    polygon <- densit()
    # Bepalen t-waardes die onder Ha vallen
    if(input$sided == "=") testK <- polygon$x < -abs(t.crit()) # Tweezijdig  (< linkse waarde) 
    if(input$sided == "<") testK <- polygon$x < -abs(t.crit()) # Linkszijdig (< linkse waarde) 
    if(input$sided == ">") testK <- 0                          # bestaat niet bij Rechtsszijdig
    # Subset van density frame voor t waardes binnen h0
    polygon <- polygon[testK,]
    # Polygon maken, met een beetje verhoging -(max(densit()$y)/20) is eigenlijk 0
    data.frame(x=c(polygon$x, rev(polygon$x)), 
               y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
    )  
  })
  
  # Kritieke gebied Polygon dat links hoort onder Ha -------------------------------------------------------------
  polygR <- reactive({
    polygon <- densit()
    # Bepalen t-waardes die onder Ha vallen    
    if(input$sided == "=") testK <- polygon$x > abs(t.crit()) # Tweezijdig (> rechtse waarde) 
    if(input$sided == "<") testK <- 0                         # bestaat niet bij Linkszijdig
    if(input$sided == ">") testK <- polygon$x > abs(t.crit()) # Rechtsszijdig (> rechtse waarde)
    # Subset van density frame voor t waardes binnen h0    
    polygon <- polygon[testK,]
    # Polygon maken, met een beetje verhoging -(max(densit()$y)/20) is eigenlijk 0
    data.frame(x=c(polygon$x, rev(polygon$x)), 
               y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
    )
  })
  
  # P waarde Polygon dat hoort bij echte test (Dus niet SPSS troep) -------------------------------------------------------------
  polygProp <- reactive({
    polygon <- densit()
    # Bepalen t-waardes die exteremer zijn dan gevonden t-waarde (onder H0) 
    if(input$sided == "=") testK <- polygon$x < -abs(t.value()) # ander helft wordt omgedraaid in de functie van de plot
    if(input$sided == "<") testK <- polygon$x < t.value()       # Alleen linkse kant
    if(input$sided == ">") testK <- polygon$x > t.value()       # Alleen rechtse kant
    # Subset van density frame voor extreme(re) t waardes    
    polygon <- polygon[testK,]
    # Polygon maken, met een beetje verhoging -(max(densit()$y)/20) is eigenlijk 0
    data.frame(x=c(polygon$x, rev(polygon$x)), 
               y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
    )
  })

  # P waarde Polygon dat hoort bij SPSS (Dus altijd tweezijdig) -------------------------------------------------------------
  polygSpss <- reactive({
    polygon <- densit()
    # Bepalen t-waardes die extremere zijn dan gevonden t-waarde (tweezijdig, onder H0)
    testK <- polygon$x > abs(t.value())
    # Subset van density frame voor extreme(re) t waardes     
    polygon <- polygon[testK,]
    # Polygon maken, met een beetje verhoging -(max(densit()$y)/20) is eigenlijk 0
    data.frame(x=c(polygon$x, rev(polygon$x)), 
               y=c(polygon$y, rep(-(max(densit()$y)/20), times=length(polygon$x)))
    )
  })
  
  # Kritieke waarde plot Plot ------------------------------------------------------------- 
  output$kritiek_plot <- renderPlot({  
    # Voor het gemak kritieke t waarde opnieuw inlezen (En altijd absoluut maken)
    t.cr <- abs(t.crit())
    # Basis Plot (density frame en lijntjes trekken)
    p <-
      ggplot(densit(),aes(x=x,y=y)) +
      geom_line(lwd=1.5)
    
    # Tweezijdig
    if(input$sided == "="){
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.8, fill="#E84E10") + # H0 frame
        geom_polygon(aes(x=x,y=y),data=polygL(),alpha=.8, fill="#001C3D") +  # Ha frame
        geom_polygon(aes(x=x,y=y),data=polygR(),alpha=.8, fill="#001C3D") +  # Ha frame
        geom_segment(x=-t.cr,xend=-t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) + # afbakening H0 links
        geom_segment(x=t.cr,xend=t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) +   # afbakening H0 rechts
        annotate("text",label=paste("t[krit]==",round(t.cr,2)), x = t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) + # kritieke waarde weergeven
        annotate("text",label=paste("t[krit]==",round(-t.cr,2)), x = -t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) # kritieke waarde weergeven   
    }
    
    # Linkszijdig
    if(input$sided == "<"){
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.8, fill="#E84E10") + # H0 frame
        geom_polygon(aes(x=x,y=y),data=polygL(),alpha=.8, fill="#001C3D") +  # Ha frame
        geom_segment(x=-t.cr,xend=-t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) + # afbakening H0 links
        annotate("text",label=paste("t[krit]==",round(-t.cr,2)), x = -t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) # kritieke waarde weergeven        
    }
    
    # Rechtszijdig
    if(input$sided == ">"){
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygH0(),alpha=.8, fill="#E84E10") + # H0 frame
        geom_polygon(aes(x=x,y=y),data=polygR(),alpha=.8, fill="#001C3D") +  # Ha frame
        geom_segment(x=t.cr,xend=t.cr,y=-Inf,yend=densit()[which.min(abs(t.cr-densit()$x)),"y"],lwd=1.5) + # afbakening H0 rechts
        annotate("text",label=paste("t[krit]==",round(t.cr,2)), x = t.cr , y = -Inf, vjust = 1.5,size=8,parse=TRUE) # kritieke waarde weergeven            
    }
    
    # Adjust t value placement if it cross the border!
    if(t.value() < 0) par_hjust <- 0 else par_hjust <- 1
    par_x   <- t.value() 
    par_lab <- paste("~t[w]==",round(t.value(),2))  
    # Als het links overschrijd
    if(t.value() <  min(densit()$x)) {
      par_x   <- min(densit()$x) # Kleinst mogelijke x 
      par_lab <- paste("{~t[w] %<-% phantom(0)}==",round(t.value(),2))} # Label plaatsing Kleinst mogelijke x (met werkelijk waarde)
    # Als het rechts overschrijd   
    if(t.value() > max(densit()$x)) {
      par_x   <- max(densit()$x) # Grootst mogelijke x 
      par_lab <- paste("{t[w] %->% phantom(0)}==",round(t.value(),2))} # Label plaatsing Grootst mogelijke x (met werkelijk waarde)
    
    # gevonden t-waarde plaatsen, al dan niet gecorigeerd
    p <- 
      p + 
      geom_vline(xintercept=par_x, lwd=1.5,lty="longdash") +
      annotate("text",label=par_lab, x =  par_x, y = max(densit()$y)/2, hjust = par_hjust, size=8,parse=TRUE) # label midden in de plot
    
    # laatste opmaak
    p <- 
      p +   
      theme_bw(20) + # Vergroten en zwart wit thema
      geom_line(lwd=1.5) + # Nogmaals lijnen trekken
      ggtitle("t-verdeling") + # Titel
      theme(axis.text.y = element_blank(),axis.title.y=element_blank()) + # leeg thema
      scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + # Kale x-as
      scale_y_continuous(expand=c(0,0)) # kale y-as
    
    # Aparte build omdat anders niet buiten de plot valt te plotten http://stackoverflow.com/questions/12196529/how-to-name-sections-on-x-axis-that-are-seperated-by-vertical-lines-in-an-r-plot
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt) 
  })
  
  # Kriteke waarde toevoeging (conclusie e.d.) ------------------------------------------------------------- 
  output$kritiek_conc <- renderPlot({
    # Altijd conclusie voordrukken
    grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
    # Als antwoord wordt gevraagd geven!
    if(input$conc){
      if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
    }
  })
  
  # P waarde plot Plot -------------------------------------------------------------  
  output$pwaarde_plot <- renderPlot({  
    # Voor het gemak gevonde t waarde opnieuw inlezen
    t.va <- t.value()
    
    # Adjust t value placement if it cross the border!
    # Legen objecten om ze hierna te vullen
    par_x <- NULL
    par_lab <- NULL
    par_hjust <- NULL
    
    # Objecten vullen
    par_x[1]   <- t.value() # Normaal gesproken geen overschrijving
    par_lab[1] <- paste("t[w]==",round(t.value(),2)) # Normaal gesproken geen overschrijving
    par_lab[2] <- paste("t[w]==",round(-t.value(),2)) # Normaal gesproken geen overschrijving
    
    # Als het links overschrijd
    if(t.value() <  min(densit()$x)) {
      par_x[1]   <- min(densit()$x) # Kleinst mogelijke x 
      par_lab[1] <- paste("{t[w]%<-%phantom(0)}==",round(-abs(t.value()),2)) # Label plaatsing Kleinst mogelijke x (met werkelijk waarde)
      if(input$sided == "=") par_lab[2] <- paste("{t[w]%->%phantom(0)}==",round(abs(t.value()),2)) # Bij tweezijdig ook aan andere kant label
    }
    # Als het rechts overschrijd  
    if(t.value() >  max(densit()$x)) {
      par_x[1]   <- max(densit()$x) # Grootst mogelijke x  
      par_lab[1] <- paste("{t[w]%->%phantom(0)}==",round(abs(t.value()),2)) # Label plaatsing Groots mogelijke x (met werkelijk waarde)
      if(input$sided == "=") par_lab[2] <- paste("{t[w]%<-%phantom(0)}==",round(-abs(t.value()),2)) # Bij tweezijdig ook aan andere kant label
    }
    # T waarde spiegelen
    par_x[2] <- -par_x[1]
    # Aanpassen welke kant t-waarde van de lijn staat 
    if(par_x[1] < 0) par_hjust[1] <- 0 else  par_hjust[1] <- 1 # Naar links rechtsuitlijnen
    if(par_x[2] < 0) par_hjust[2] <- 0 else  par_hjust[2] <- 1 # Naar rechts linksuitlijnen

    # Basis Plot (density frame en lijntjes trekken)
    p <-
      ggplot(densit(),aes(x=x,y=y)) +
      geom_line(lwd=1.5)
    
    # Tweezijdig
    if(input$sided == "="){
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygProp(), alpha=.8, fill="#001C3D") + # Gebied waarschijnlijkheid
        geom_polygon(aes(x=x,y=y),
                     data=data.frame(x=-polygProp()$x,y=polygProp()$y), # spiegeling gebied waarschijnlijkheid (want tweezijdig)
                     alpha=.8, fill="#001C3D"
                     ) + 
        geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) + # afbakening gebied 
        geom_segment(x=par_x[2], xend=par_x[2], y=-Inf,yend=densit()[which.min(abs(par_x[2]-densit()$x)),"y"], lwd=1.5) +  # afbakening gebied 
        annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[1]) + # gevonden waarde weergeven
        annotate("text",label=par_lab[2], x = par_x[2], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[2])   # gevonden waarde weergeven
    } else {
      p <-
        p +
        geom_polygon(aes(x=x,y=y),data=polygProp(), alpha=.8, fill="#001C3D") +  # Gebied waarschijnlijkheid
        geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) + # afbakening gebied 
        annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=8,parse=TRUE, hjust= par_hjust[1]) # gevonden waarde weergeven
    }
    
    # laatste opmaak
    p <- 
      p +   
      theme_bw(20) + # Vergroten en zwart wit thema 
      geom_line(lwd=1.5) + # Nogmaals lijnen trekken
      ggtitle("t-verdeling") + # Titel
      theme(axis.text.y = element_blank(),axis.title.y=element_blank()) + # leeg thema
      scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + # Kale x-as 
      scale_y_continuous(expand=c(0,0),limits=c(-(max(densit()$y)/20),max(densit()$y))) + # kale y-as
      annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p =",round(pval(),3)),vjust=1.5,size=8,hjust=0) # exacte p-waarde  
    
    # Berekening p waarde via sig (spss)
    # Tweezijdig
    if(input$sided == "="){
      p <- p +   annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig."),vjust=3,size=8,hjust=0) # is gewoon sig.
    }
    # Linkszijdig
    if(input$sided == "<"){
      if(t.value() > 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = 1-sig./2"),vjust=3,size=8,hjust=0) # omdat het de verkeerde kant is, gekke berekening
      if(t.value() <= 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig./2"),vjust=3,size=8,hjust=0) # gebiedje spss gedeeld door twee
    }
    if(input$sided == ">"){
      if(t.value() <= 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = 1-sig./2"),vjust=3,size=8,hjust=0) # omdat het de verkeerde kant is, gekke berekening
      if(t.value() > 0) p <- p + annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste(" p = sig./2"),vjust=3,size=8,hjust=0) # gebiedje spss gedeeld door twee
    }
    
    # Aparte build omdat anders niet buiten de plot valt te plotten 
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt) 
  })
  
  # P waarde toevoeging (SPSS & conclusie e.d.) ------------------------------------------------------------- 
  output$pwaarde_conc <- renderPlot({
    # Alles exact gelijk aan hierboven alleen dan altijd tweezijdig 
    t.va <- t.value()
    par_x <- NULL
    par_lab <- NULL
    par_hjust <- NULL
    
    par_x[1]   <- t.value() 
    par_lab[1] <- paste("t[w]==",round(t.value(),2))
    par_lab[2] <- paste("t[w]==",round(-t.value(),2))  
    
    if(t.value() <  min(densit()$x)) {
      par_x[1]   <- min(densit()$x) 
      par_lab[1] <- paste("{t[w]%<-%phantom(0)}==",round(-abs(t.value()),2))
      par_lab[2] <- paste("{t[w]%->%phantom(0)}==",round(abs(t.value()),2))
    }
    
    if(t.value() >  max(densit()$x)) {
      par_x[1]   <- max(densit()$x) 
      par_lab[1] <- paste("{t[w]%->%phantom(0)}==",round(abs(t.value()),2))
      par_lab[2] <- paste("{t[w]%<-%phantom(0)}==",round(-abs(t.value()),2))
    }
    
    par_x[2] <- -par_x[1]
    
    if(par_x[1] < 0) par_hjust[1] <- 0 else  par_hjust[1] <- 1
    if(par_x[2] < 0) par_hjust[2] <- 0 else  par_hjust[2] <- 1
    
    p <-
      ggplot(densit(),aes(x=x,y=y)) +
      geom_line(lwd=1.5) +
      geom_polygon(aes(x=x,y=y),data=polygSpss(), alpha=.8, fill="#001C3D") +
      geom_polygon(aes(x=x,y=y),data=data.frame(x=-polygSpss()$x,y=polygSpss()$y), alpha=.8, fill="#001C3D") +
      geom_segment(x=par_x[1], xend=par_x[1], y=-Inf,yend=densit()[which.min(abs(par_x[1]-densit()$x)), "y"], lwd=1.5) +
      geom_segment(x=par_x[2], xend=par_x[2], y=-Inf,yend=densit()[which.min(abs(par_x[2]-densit()$x)),"y"], lwd=1.5) +
      annotate("text",label=par_lab[1], x = par_x[1], y = -Inf, vjust = 1.5,size=4,parse=TRUE, hjust= par_hjust[1]) +
      annotate("text",label=par_lab[2], x = par_x[2], y = -Inf, vjust = 1.5,size=4,parse=TRUE, hjust= par_hjust[2]) +
      annotate("text",x=min(densit()$x),y=max(densit()$y),label=paste("Sig.=",round(pt(-abs(t.value()),df())*2,3)),vjust=1.5,size=4,hjust=-.25) 
    
    p <- 
      p +   
      theme_bw(10) + geom_line(lwd=1.5) +
      ggtitle("SPSS") +
      theme(axis.text.y = element_blank(),axis.title.y=element_blank()) +
      scale_x_continuous(breaks=NULL,name="",expand=c(0,0)) + scale_y_continuous(expand=c(0,0))  
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    plot.new() 

    # Samenvoegen plot SPSS en conclusie
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
    
    # Tweede viewport
    pushViewport(vp.2)
    grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
    if(input$conc){
      if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
    }
  })
  
  # BI waarde plot Plot -------------------------------------------------------------  
  output$CI_plot <- renderPlot({
    
    # BI voor een steekproef
    if(input$test == "onettest"){
      Xval <- input$one_X # X waarde
      Tval <- abs(t.crit()*(input$one_sd/sqrt(input$one_N))) # SE berekenen
      BI <- data.frame(y="",x=Xval, xlow=Xval - Tval, xhigh=Xval +Tval) # data frame
      p <- ggplot(BI, aes(x=y, y=x, ymin = xlow, ymax=xhigh)) +  
        geom_pointrange(lwd=1.5) + # dikke ranges
        geom_errorbar(width = 0.5, lwd = 1.5)  + # errorbars even dik
        geom_hline(yintercept=input$one_u) + # intercept
        annotate("text",y=BI$xlow,  x=.7,label=round(BI$xlow,2)) +  # Low BI
        annotate("text",y=BI$xhigh, x=.7,label=round(BI$xhigh,2)) + # High BI
        annotate("text",y=BI$x, x=.95,label=round(BI$x,2)) +        # Point est.
        scale_y_continuous(breaks=input$one_u, labels=list(bquote(mu == .(input$one_u))),name="") + # Scale X kaal
        scale_x_discrete(name="") + # Scale y kaal
        ggtitle("Betrouwbaarheids Interval") + # Titel
        theme_bw(20) + # Thema
        coord_flip() # Omdraaien   
      print(p) # Printen
    }
 
    # BI voor t weeteekproeven   
    if(input$test == "twottest"){
      # Berkeening hoog en laag BI
      lowX <- input$two_X1 - input$two_X2 - (abs(t.crit()) * sqrt(((input$two_sd1^2)/input$two_N1) + ((input$two_sd1^2)/input$two_N1)))
      highX<- input$two_X1 - input$two_X2 + (abs(t.crit()) * sqrt(((input$two_sd1^2)/input$two_N1) + ((input$two_sd1^2)/input$two_N1)))
      # Point estimate
      Xval <- input$two_X1 - input$two_X2
      # Data frame
      BI <- data.frame(y="",x=Xval, xlow=lowX, xhigh=highX)
      p <- ggplot(BI, aes(x=y, y=x, ymin = xlow, ymax=xhigh)) + # Gelijk aan hierboven
        geom_pointrange(lwd=1.5) +
        geom_errorbar(width = 0.5, lwd = 1.5)  + 
        geom_hline(yintercept=0) + 
        annotate("text",y=BI$xlow,  x=.7,label=round(BI$xlow,2)) +
        annotate("text",y=BI$xhigh, x=.7,label=round(BI$xhigh,2)) +
        annotate("text",y=BI$x, x=.95,label=round(BI$x,2)) +
        scale_y_continuous(breaks=0, labels=list(bquote(t == 0)),name="") + 
        scale_x_discrete(name="")
        ggtitle("Betrouwbaarheids Interval") +
        theme_bw(20)  +
        coord_flip()  
      print(p)
    }
    
    # "WARNING" als men eenzijdig wilt toetsen (wat dus niet kan)
    if(input$sided != "="){
      plot.new()
      grid.text("Niet mogelijk bij eenzijdig toetsen", gp=gpar(fontsize=40))
    }
  })

  # BI waarde toevoeging (BI berekenen & conclusie e.d.) ------------------------------------------------------------- 
  output$CI_conc <- renderPlot({
    # Eenzijdig
    if(input$test == "onettest"){
      # Definiering viewports
      gl <- grid.layout(nrow=1, ncol=2)
      vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
      vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
      # init layout
      pushViewport(viewport(layout=gl))
      # access the first position
      pushViewport(vp.1)
      # Eerst formule
      grid.text(bquote(group("(",list(
        bar(X)-t[list(alpha/2,N-1)]*frac(s,sqrt(N)),
        bar(X)+t[list(alpha/2,N-1)]*frac(s,sqrt(N))
      ),")")),
      gp=gpar(fontsize=20),vjust=-2)
      # Gedeeltelijke invoer .(x) wordt uitgerekend
      grid.text(bquote(group("(",list(
        .(input$one_X)-.(round(abs(t.crit()),2))*frac(.(input$one_sd),sqrt(.(input$one_N))),
        .(input$one_X)+.(round(abs(t.crit()),2))*frac(.(input$one_sd),sqrt(.(input$one_N))) 
      ),")")),
      gp=gpar(fontsize=20))
      # Uiteindelijke BI
      grid.text(bquote(group("(",list(
        .(round(input$one_X-abs(t.crit())*(input$one_sd/sqrt(input$one_N)),2) ),
        .(round(input$one_X+abs(t.crit())*(input$one_sd/sqrt(input$one_N)),2) )
      ),")")),
      gp=gpar(fontsize=20),vjust=4)
      # Einde Viewport 1
      popViewport()
      
      # Start Viewport 2
      pushViewport(vp.2)
      # Altijd conclusie
      grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
      # Als antwoord gevraagd wordt geven!
      if(input$conc){
        if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
      }
    }
    
    # Idee precies gelijk aan hierboven maar nu voor twee steekproeven
    if(input$test == "twottest"){
      gl <- grid.layout(nrow=1, ncol=2)
      vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1) 
      vp.2 <- viewport(layout.pos.col=2, layout.pos.row=1) 
      pushViewport(viewport(layout=gl))
      pushViewport(vp.1)    
      grid.text(bquote(bar(X)[1]-bar(X)[2]%+-%t[list(alpha/2,df)]*sqrt(frac(s[1]^2,N[1])+frac(s[2]^2,N[2]))),
                gp=gpar(fontsize=20),vjust=-1.5)
      grid.text(bquote(.(input$two_X1 - input$two_X2)%+-%.(round(abs(t.crit()),2))%*% sqrt(.(round((input$two_sd1^2)/input$two_N1,3))+.(round((input$two_sd2^2)/input$two_N2,3)))),
                gp=gpar(fontsize=20)) 
      grid.text(bquote(group("(",list(
        .(round(input$two_X1 - input$two_X2 - (abs(t.crit())* sqrt(((input$two_sd1^2)/input$two_N1) +((input$two_sd1^2)/input$two_N1))),2)),
        .(round(input$two_X1 - input$two_X2 + (abs(t.crit())* sqrt(((input$two_sd1^2)/input$two_N1) +((input$two_sd1^2)/input$two_N1))),2))
      ),")")),
      gp=gpar(fontsize=20),vjust=4)
      popViewport()
      pushViewport(vp.2)
      grid.text("Conclusie:",gp=gpar(fontsize=40),vjust=-2)
      if(input$conc){
        if(hypo() == "HA") grid.text(expression(H[A]),gp=gpar(fontsize=80)) else grid.text(expression(H[0]),gp=gpar(fontsize=80))
      }
    }
    
    # Als niet mogelijk leeg
    if(input$sided != "="){
      plot.new()
    }
  })
  
  # Actuele waardes, hypotheses, alpha, en t-waardes -------------------------------------------------------------  
  output$waardes <- renderPlot({
    # Een steekproef 
    if(input$test == "onettest"){
      plot.new() 
      # setup layout
      gl <- grid.layout(nrow=6, ncol=2)
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
      # plotten hypotheses naast elkaar afhankelijk van richting
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
      # Rest van waardes plotten
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
      grid.text(bquote({t[w] == frac( .(input$one_X) - .(input$one_u),.(input$one_sd)/sqrt(.(input$one_N)) )} == .(round(t.value(),2))), 
                gp=gpar(fontsize=40))
      popViewport()
    }
    
    # Twee steekproeven precies gelijk aan hierboven
    if(input$test == "twottest"){
      plot.new() 
      gl <- grid.layout(nrow=6, ncol=3)
      vp.1 <- viewport(layout.pos.col=1, layout.pos.row=1)
      vp.2 <- viewport(layout.pos.col=3, layout.pos.row=1)
      vp.3 <- viewport(layout.pos.col=1, layout.pos.row=2)
      vp.4 <- viewport(layout.pos.col=1, layout.pos.row=3) 
      vp.5 <- viewport(layout.pos.col=1, layout.pos.row=4) 
      vp.6 <- viewport(layout.pos.col=1, layout.pos.row=5) 
      vp.7 <- viewport(layout.pos.col=2, layout.pos.row=3) 
      vp.8 <- viewport(layout.pos.col=2, layout.pos.row=4) 
      vp.9 <- viewport(layout.pos.col=2, layout.pos.row=5) 
      vp.10 <- viewport(layout.pos.col=1:2, layout.pos.row=6) 
      pushViewport(viewport(layout=gl))
      if(input$sided == "="){
        pushViewport(vp.1)  
        grid.text(bquote('H'[0]: mu[1]==mu[2]), gp=gpar(fontsize=40), hjust=0)
        popViewport()
        pushViewport(vp.2)      
        grid.text(bquote('H'[1]:  mu[1] !=  mu[2]), gp=gpar(fontsize=40), hjust=1)
        popViewport()
      }
      if(input$sided == "<"){
        pushViewport(vp.1)  
        grid.text(bquote('H'[0]:  mu[1]>= mu[2]), gp=gpar(fontsize=40), hjust=0)
        popViewport()
        pushViewport(vp.2)     
        grid.text(bquote('H'[1]:  mu[1] <  mu[2]), gp=gpar(fontsize=40), hjust=1)
        popViewport()
      }
      if(input$sided == ">"){
        pushViewport(vp.1)  
        grid.text(bquote('H'[0]:  mu[1]<= mu[2]), gp=gpar(fontsize=40), hjust=0)
        popViewport()
        pushViewport(vp.2)    
        grid.text(bquote('H'[1]:  mu[1] >  mu[2]), gp=gpar(fontsize=40), hjust=1)
        popViewport()
      }
      # alpha
      pushViewport(vp.3)    
      grid.text(bquote(alpha == .(as.numeric(input$alpha))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      # Waardes groep 1
      pushViewport(vp.4)    
      grid.text(bquote(bar(X)[1] == .(as.numeric(input$two_X1))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      pushViewport(vp.5)    
      grid.text(bquote(s[1] == .(as.numeric(input$two_sd1))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      pushViewport(vp.6)    
      grid.text(bquote(N[1] == .(as.numeric(input$two_N1))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      pushViewport(vp.7)
      # Waardes groep 2
      grid.text(bquote(bar(X)[2] == .(as.numeric(input$two_X2))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      pushViewport(vp.8)    
      grid.text(bquote(s[2] == .(as.numeric(input$two_sd2))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      pushViewport(vp.9)    
      grid.text(bquote(N[2] == .(as.numeric(input$two_N2))), gp=gpar(fontsize=40), hjust=0)
      popViewport()
      # t waarde
      pushViewport(vp.10)    
      grid.text(bquote({t[w] == frac( .(input$two_X1) - .(input$two_X2),sqrt(frac(.(input$two_sd1)^2,.(input$two_N1))+frac(.(input$two_sd2)^2,.(input$two_N2))))} == .(round(t.value(),2))), 
                gp=gpar(fontsize=40))
      popViewport()
    }
  })
  
  # T tabel als in imbos met selectie van rijen + kolommen -------------------------------------------------------------  
  output$ttable <- renderText({
    # Afhankelijk van welke richting en alpha selectie kolommen (stappen tussen kolommen worden gegeven)
    if(as.numeric(input$alpha)==.01){
      values <- c(1,11) 
      if(input$sided == "<") values <- c(2,11)
      if(input$sided == ">") values <- c(12,1)
    }
    
    if(as.numeric(input$alpha)==.05){
      values <- c(3,7,2) 
      if(input$sided == "<") values <- c(4,9)
      if(input$sided == ">") values <- c(10,3)
    }
    
    if(as.numeric(input$alpha)==.1){
      values <- c(4,5,3) 
      if(input$sided == "<") values <- c(5,8)
      if(input$sided == ">") values <- c(9,4)
    }
    
    # Als er twee aaneengesloten "lege" kolomspans zijn
    if(length(values) == 2) {
      schema_kleur <-
        paste('<colgroup>  <col span="',values[1],
              '" ><col span="1" style="background-color:white">
            <col span="',values[2],'" >
    <col span="1" style="background-color:white">
    </colgroup>')
    }

    # Als er drie aaneengesloten "lege" kolomspans zijn
    if(length(values) == 3) {
      schema_kleur <-
        paste('<colgroup>  <col span="',values[1],
              '" ><col span="1" style="background-color:white">
            <col span="',values[2],'" >
            <col span="1" style="background-color:white"><col span="',values[3],'" >
            </colgroup>')
    }
    
    ttableHTML <- ttable  # opnieuw inlezen voor gemak
    ttableHTML <- round(ttableHTML,3) # Round anders gaat het fout als je er tekst van maakt
    # Selectie kolom + rij van eerste gevonden waarde (wordt uiteindelijk dikgedrukt)
    ttableHTML[rownames(ttable) == table.df(),values[1]] <- paste("@@", ttableHTML[rownames(ttable) == table.df(),values[1]])
    # Selectie kolom + rij van twee gevonden waarde bij tweezijdig
    if(input$sided == "=") {
      ttableHTML[rownames(ttable) == table.df(),values[1]+values[2]+1] <- 
        paste("@@", ttableHTML[rownames(ttable) == table.df(),values[1]+values[2]+1])
    }
    # Capture output zodat we kunnen frobelen
    ttableHTML <- capture.output(print(xtable(ttableHTML),"HTML"))
    # Kleuren schema toevoegen
    ttableHTML[3] <- paste(ttableHTML[3],schema_kleur)
    # Rij selecteren die gehiglight moeten worden
    sel_row <- which(str_detect(string=ttableHTML, paste('<TD align=\"right\">',table.df(),'</TD>')))
    # Html code voor highlight
    ttableHTML[sel_row] <- gsub("<TR>","<tr bgcolor='white'>",ttableHTML[sel_row])
    # Toevoegen dikgedrukt
    ttableHTML <- gsub("@@","<b>",ttableHTML)
    # Toevoegen tekst linkboven
    ttableHTML <- gsub("<TR> <TH> </TH> <TH>","<TR> <TH> df\\\\p </TH> <TH>",ttableHTML)
    # uitvoer printen
    ttableHTML
  })

  # Exacte uitkomst van Imbos tabel (Waarmee gerekend wordt) -------------------------------------------------------------  
  output$critt <- renderPlot({
    # Achtergrond
    grid.rect(gp=gpar(fill="#00A2DB",lwd=0,col="white"))
    # Kritieke t geven (zoals exact wordt gebruikt)
    grid.text(bquote(t[krit] %~~% .(round(t.crit(),2))), gp=gpar(fontsize=40), hjust=1.5)
    # bij tweezijdig ook andere kritieke t geven
    if(input$sided=="=")  grid.text(bquote(t[krit] %~~% .(round(-t.crit(),2))), gp=gpar(fontsize=40), hjust=0)
  })
  
  # Version info -------------------------------------------------------------     
  output$version <- renderPrint({
    # Versie en sessie info
    sessionInfo()
  })

  # Author info -------------------------------------------------------------     
  output$author <- renderPrint({
    "Author: Huub Hoofs"
    #42
  })
  
  # Lincence info -------------------------------------------------------------    
  output$licence <- renderPrint({
    cat("
The MIT License (MIT)

Copyright (c) 2014 Huub

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the 'Software' ), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
")
  })
  
})
