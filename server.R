library(shiny)
library(ggplot2)

#Download data from ESA URL
dat_url <- "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
dat  <- data.frame(read.delim(dat_url,header=TRUE, sep="\t",na.strings="-999.00"))
dat$metmass <- dat$X18.1_BasalMetRate_mLO2hr / dat$X5.1_AdultBodyMass_g
dat$species <- paste(dat$MSW05_Genus,dat$MSW05_Species,sep=" ")
order.names <- unique(dat[,1])

#Key for labels
lab_key <- list()
lab_key[[56]] <- "Metabolic Rate (mL O2/ g / hr)"
lab_key[[7]] <- "Body Mass (g)"
lab_key[[16]] <- "Gestational Duration (days)"
lab_key[[18]] <- "Home range (km2)"
lab_key[[21]] <- "Litter size (n)"
lab_key[[23]] <- "Maximum longevity (months)"
lab_key[[24]] <- "Neonate mass (g)"
lab_key[[26]] <- "Population density (n/km2)"
lab_key[[29]] <- "Social group size (n)"
lab_key[[28]] <- "Sexual maturity age (days)"
lab_key[[33]] <- "Weaning age (days)"
lab_key[[52]] <- "Mean annual precipitation (mm)"
lab_key[[53]] <- "Mean annual temperature (0.1 C)"

#Key for table
tab_key <- list()
tab_key[[56]] <- "Met Rate"
tab_key[[7]] <- "Mass"
tab_key[[16]] <- "Gest Dur"
tab_key[[18]] <- "Range"
tab_key[[21]] <- "Litt size"
tab_key[[23]] <- "Max long"
tab_key[[24]] <- "Neo mass"
tab_key[[26]] <- "Pop Dens"
tab_key[[29]] <- "Group size"
tab_key[[28]] <- "Mat Age"
tab_key[[33]] <- "Wean Age"
tab_key[[52]] <- "MAP"
tab_key[[53]] <- "MAT"

shinyServer(
  function(input, output){
    
    output$myplot <- renderPlot({
      if(input$order=="All"){
        
        colx = dat[,as.numeric(input$xvar)]
        coly = dat[,as.numeric(input$yvar)]
        Order = dat[,1]
        Species = dat$species
        dat_new = data.frame(colx,coly,Order,Species)
        dat_new = dat_new[complete.cases(dat_new),]
        
        if(input$fitdata=="Linear"){
          g = ggplot(dat_new, aes(x=colx, y=coly, col=Order)) + geom_point() +
            xlab(lab_key[[as.numeric(input$xvar)]]) + 
            ylab(lab_key[[as.numeric(input$yvar)]]) +
            scale_x_log10() + scale_y_log10() + 
            theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                  legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold"))+
            geom_smooth(aes(group=1),method="lm",color="black",se=TRUE) 
        } else if (input$fitdata=="None"){
          g = ggplot(dat_new, aes(x=colx, y=coly, col=Order)) + geom_point() +
            xlab(lab_key[[as.numeric(input$xvar)]]) + ylab(lab_key[[as.numeric(input$yvar)]]) +
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
        dat_new = data.frame(colx,coly,Family,Species)
        dat_new = dat_new[complete.cases(dat_new),]
          if(input$fitdata=="Linear"){
            g = ggplot(dat_new, aes(x=colx, y=coly, col=Family)) + geom_point()+
              xlab(lab_key[[as.numeric(input$xvar)]]) + ylab(lab_key[[as.numeric(input$yvar)]])+
              scale_x_log10() + scale_y_log10()+ 
              theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                    legend.text=element_text(size=14),legend.title=element_text(size=14,face="bold"))+
              geom_smooth(aes(group=1),method="lm",color="black",se=TRUE) 
          } else if (input$fitdata=="None"){
            g = ggplot(dat_new, aes(x=colx, y=coly, col=Family)) + geom_point()+
              xlab(lab_key[[as.numeric(input$xvar)]]) + ylab(lab_key[[as.numeric(input$yvar)]])+
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
          dat_new = data.frame(colx,coly)
          colnames(dat_new) <- c(tab_key[[as.numeric(input$xvar)]],
                                 tab_key[[as.numeric(input$yvar)]])
          dat_new = dat_new[complete.cases(dat_new),]
          model <- lm(log(coly) ~ log(colx))
          slope <- signif(as.numeric(coef(model)[2]),3)
          int <- signif(as.numeric(coef(model)[1]), 3)
          r_sq <- signif(as.numeric(summary(model)$adj.r.squared),2)
          str1 <- paste("ln(",tab_key[[as.numeric(input$yvar)]], 
                         ") = ",slope,"ln(", 
                         tab_key[[as.numeric(input$xvar)]], ") + ",int)
          str2 <- paste(" where R2 = ", r_sq, " with ", nrow(dat_new), 
                         " unique data points.")

          HTML(paste(str1, str2, sep = '<br/>'))
        } else {
          order.rows = which(dat[,1]==input$order)
          colx = dat[order.rows,as.numeric(input$xvar)]
          coly = dat[order.rows,as.numeric(input$yvar)]
          dat_new = data.frame(colx,coly)
          dat_new = dat_new[complete.cases(dat_new),]
          model <- lm(log(coly) ~ log(colx))
          slope <- signif(as.numeric(coef(model)[2]),3)
          int <- signif(as.numeric(coef(model)[1]), 3)
          r_sq <- signif(as.numeric(summary(model)$adj.r.squared),2)
          str1 <- paste("ln(",tab_key[[as.numeric(input$yvar)]], 
                        ") = ",slope,"ln(", 
                        tab_key[[as.numeric(input$xvar)]], ") + ",int)
          str2 <- paste(" where R2 = ", r_sq, " with ", nrow(dat_new), 
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
        dat_new <- data.frame(colx,coly,Order,Species)
        dat_new <- dat_new[complete.cases(dat_new),]
        
      } else {
        order.rows = which(dat[,1]==input$order)
        colx = dat[order.rows,as.numeric(input$xvar)]
        coly = dat[order.rows,as.numeric(input$yvar)]
        Family = dat[order.rows,2]
        Species = dat$species[order.rows]
        dat_new = data.frame(colx,coly,Family,Species)
        dat_new = dat_new[complete.cases(dat_new),]
      }
      brushedPoints(dat_new, input$plot_brush)
    })
  }
)

