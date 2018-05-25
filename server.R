library(shiny)
library(ggplot2)

#Download data from ESA URL
dat.url <- "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
dat  <- data.frame(read.delim(dat.url,header=TRUE, sep="\t",na.strings="-999.00"))
dat$metmass <- dat$X18.1_BasalMetRate_mLO2hr / dat$X5.1_AdultBodyMass_g
dat$species <- paste(dat$MSW05_Genus,dat$MSW05_Species,sep=" ")
order.names <- unique(dat[,1])

#Key for labels
lab.key <- list()
lab.key[[56]] <- "Metabolic Rate (mL O2/ g / hr)"
lab.key[[7]] <- "Body Mass (g)"
lab.key[[16]] <- "Gestational Duration (days)"
lab.key[[18]] <- "Home range (km2)"
lab.key[[21]] <- "Litter size (n)"
lab.key[[23]] <- "Maximum longevity (months)"
lab.key[[24]] <- "Neonate mass (g)"
lab.key[[26]] <- "Population density (n/km2)"
lab.key[[29]] <- "Social group size (n)"
lab.key[[28]] <- "Sexual maturity age (days)"
lab.key[[33]] <- "Weaning age (days)"
lab.key[[52]] <- "Mean annual precipitation (mm)"
lab.key[[53]] <- "Mean annual temperature (0.1 C)"

#Key for table
tab.key <- list()
tab.key[[56]] <- "Met Rate"
tab.key[[7]] <- "Mass"
tab.key[[16]] <- "Gest Dur"
tab.key[[18]] <- "Range"
tab.key[[21]] <- "Litt size"
tab.key[[23]] <- "Max long"
tab.key[[24]] <- "Neo mass"
tab.key[[26]] <- "Pop Dens"
tab.key[[29]] <- "Group size"
tab.key[[28]] <- "Mat Age"
tab.key[[33]] <- "Wean Age"
tab.key[[52]] <- "MAP"
tab.key[[53]] <- "MAT"

shinyServer(
  function(input, output){
    
    output$myplot <- renderPlot({
      if(input$order=="All"){
        
        colx = dat[,as.numeric(input$xvar)]
        coly = dat[,as.numeric(input$yvar)]
        Order = dat[,1]
        Species = dat$species
        dat.new = data.frame(colx,coly,Order,Species)
        dat.new = dat.new[complete.cases(dat.new),]
        
        if(input$fitdata=="Linear"){
          g = ggplot(dat.new, aes(x=colx, y=coly, col=Order)) + geom_point() +
            xlab(lab.key[[as.numeric(input$xvar)]]) + 
            ylab(lab.key[[as.numeric(input$yvar)]]) +
            scale_x_log10() + scale_y_log10() + 
            theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                  legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold"))+
            geom_smooth(aes(group=1),method="lm",color="black",se=TRUE) 
        } else if (input$fitdata=="None"){
          g = ggplot(dat.new, aes(x=colx, y=coly, col=Order)) + geom_point() +
            xlab(lab.key[[as.numeric(input$xvar)]]) + ylab(lab.key[[as.numeric(input$yvar)]]) +
            scale_x_log10() + scale_y_log10() + 
            theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                  legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold")) 
        }
      } else {
        order.rows = which(dat[,1]==input$order)
        colx = dat[order.rows,as.numeric(input$xvar)]
        coly = dat[order.rows,as.numeric(input$yvar)]
        Family = dat[order.rows,2]
        Species = dat$species[order.rows]
        dat.new = data.frame(colx,coly,Family,Species)
        dat.new = dat.new[complete.cases(dat.new),]
          if(input$fitdata=="Linear"){
            g = ggplot(dat.new, aes(x=colx, y=coly, col=Family)) + geom_point()+
              xlab(lab.key[[as.numeric(input$xvar)]]) + ylab(lab.key[[as.numeric(input$yvar)]])+
              scale_x_log10() + scale_y_log10()+ 
              theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                    legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold"))+
              geom_smooth(aes(group=1),method="lm",color="black",se=TRUE) 
          } else if (input$fitdata=="None"){
            g = ggplot(dat.new, aes(x=colx, y=coly, col=Family)) + geom_point()+
              xlab(lab.key[[as.numeric(input$xvar)]]) + ylab(lab.key[[as.numeric(input$yvar)]])+
              scale_x_log10() + scale_y_log10()+ 
              theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                    legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold"))
          }
        }

      g
    })
    
    # Output linear model equation below graph
    output$equation <- renderUI({
      if(input$fitdata=="Linear"){
        if(input$order=="All"){
          colx = dat[,as.numeric(input$xvar)]
          coly = dat[,as.numeric(input$yvar)]
          dat.new = data.frame(colx,coly)
          colnames(dat.new) <- c(tab.key[[as.numeric(input$xvar)]],
                                 tab.key[[as.numeric(input$yvar)]])
          dat.new = dat.new[complete.cases(dat.new),]
          model <- lm(log(coly) ~ log(colx))
          slope <- signif(as.numeric(coef(model)[2]),3)
          int <- signif(as.numeric(coef(model)[1]), 3)
          r_sq <- signif(as.numeric(summary(model)$adj.r.squared),2)
          str1 <- paste("ln(",tab.key[[as.numeric(input$yvar)]], 
                         ") = ",slope,"ln(", 
                         tab.key[[as.numeric(input$xvar)]], ") + ",int)
          str2 <- paste(" where R2 = ", r_sq, " with ", nrow(dat.new), 
                         " unique data points.")

          HTML(paste(str1, str2, sep = '<br/>'))
        } else {
          order.rows = which(dat[,1]==input$order)
          colx = dat[order.rows,as.numeric(input$xvar)]
          coly = dat[order.rows,as.numeric(input$yvar)]
          dat.new = data.frame(colx,coly)
          dat.new = dat.new[complete.cases(dat.new),]
          model <- lm(log(coly) ~ log(colx))
          slope <- signif(as.numeric(coef(model)[2]),3)
          int <- signif(as.numeric(coef(model)[1]), 3)
          r_sq <- signif(as.numeric(summary(model)$adj.r.squared),2)
          str1 <- paste("ln(",tab.key[[as.numeric(input$yvar)]], 
                        ") = ",slope,"ln(", 
                        tab.key[[as.numeric(input$xvar)]], ") + ",int)
          str2 <- paste(" where R2 = ", r_sq, " with ", nrow(dat.new), 
                        " unique data points.")
          
          HTML(paste(str1, str2, sep = '<br/>'))
        }
      } else {
        paste0("Select 'Linear model' in the pull-down menu to the left to fit a linear model to the log-transformed data.")
      }
    })
    
    # Output selected points info
    output$info <- renderPrint({
      if(input$order=="All"){
        colx = dat[,as.numeric(input$xvar)]
        coly = dat[,as.numeric(input$yvar)]
        Order = dat[,1]
        Species = dat$species
        dat.new <- data.frame(colx,coly,Order,Species)
        dat.new <- dat.new[complete.cases(dat.new),]
        
      } else {
        order.rows = which(dat[,1]==input$order)
        colx = dat[order.rows,as.numeric(input$xvar)]
        coly = dat[order.rows,as.numeric(input$yvar)]
        Family = dat[order.rows,2]
        Species = dat$species[order.rows]
        dat.new = data.frame(colx,coly,Family,Species)
        dat.new = dat.new[complete.cases(dat.new),]
      }
      brushedPoints(dat.new, input$plot_brush)
    })
  }
)

