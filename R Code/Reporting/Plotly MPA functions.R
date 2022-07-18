#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# MPA size decision support model for SA case study by Nils Krueck, Jan 2022          @
# Script to plot results from model v1.00 as an interactive figure (Ross Dwyer), 17 Mar 2022  #                                         @
#                                                                                     @
# Version:                                                                            @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(plotly)

#returndat <- readRDS(paste0(resultsdir,modelversion,'_',scenario.name,'_returndat.RDS'))
#SpeciesNames <- str_replace_all(species.names.ordered,'_',' ') # Formatted species names

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Calculate MPA size to achieve certain protection levels ##### ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot mean protection #### ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@

plotPropTim <- function(sxaxis=100,sd=FALSE){
  
  if(sd==FALSE){
  fig <- plot_ly(x = returndat[[1]]$statstable$mpa_size/1000, 
                 y =round(returndat[[1]]$statstable$mean_prot,2),
                 name = str_replace_all(names(returndat)[1],'_',' '),
                 type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~round(returndat[[2]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[2],'_',' ')) 
  fig <- fig %>% add_trace(y = ~round(returndat[[3]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[3],'_',' '))
  fig <- fig %>% add_trace(y = ~round(returndat[[4]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[4],'_',' ')) 
  fig <- fig %>% add_trace(y = ~round(returndat[[5]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[5],'_',' '))
  fig <- fig %>% add_trace(y = ~round(returndat[[6]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[6],'_',' '))
  fig <- fig %>% add_trace(y = ~round(returndat[[7]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[7],'_',' '),
                           line = list(dash = 'dot')) 
  fig <- fig %>% add_trace(y = ~round(returndat[[8]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[8],'_',' '),
                           line = list(dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~round(returndat[[9]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[9],'_',' '),
                           line = list(dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~round(returndat[[10]]$statstable$mean_prot,2), 
                           name = str_replace_all(names(returndat)[10],'_',' '),
                           line = list(dash = 'dash')) 
  }
  if(sd==TRUE){
    fig <- plot_ly(x = returndat[[1]]$statstable$mpa_size/1000, 
                   y =round(returndat[[1]]$statstable$mean_prot,2),
                   name = str_replace_all(names(returndat)[1],'_',' '),
                   type = 'scatter', mode = 'lines+markers',
                   error_y =list(array=round(returndat[[1]]$statstable$sd_prot,2)))
    fig <- fig %>% add_trace(y = ~round(returndat[[2]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[2],'_',' '),
                             error_y =list(array=round(returndat[[2]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[3]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[3],'_',' '),
                             error_y =list(array=round(returndat[[3]]$statstable$sd_prot,2)))  
    fig <- fig %>% add_trace(y = ~round(returndat[[4]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[4],'_',' '),
                             error_y =list(array=round(returndat[[4]]$statstable$sd_prot,2)))  
    fig <- fig %>% add_trace(y = ~round(returndat[[5]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[5],'_',' '),
                             error_y =list(array=round(returndat[[5]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[6]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[6],'_',' '),
                             error_y =list(array=round(returndat[[6]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[7]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[7],'_',' '),
                             line = list(dash = 'dot'),
                             error_y =list(array=round(returndat[[7]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[8]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[8],'_',' '),
                             line = list(dash = 'dash'),
                             error_y =list(array=round(returndat[[8]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[9]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[9],'_',' '),
                             line = list(dash = 'dash'),
                             error_y =list(array=round(returndat[[9]]$statstable$sd_prot,2))) 
    fig <- fig %>% add_trace(y = ~round(returndat[[10]]$statstable$mean_prot,2), 
                             name = str_replace_all(names(returndat)[10],'_',' '),
                             line = list(dash = 'dash'),
                             error_y =list(array=round(returndat[[10]]$statstable$sd_prot,2))) 
  }
  # Add formatting of plot xy axis and hover text with full MPA dim range
  # or limit the x axis to 10 km
  fig <- fig %>% 
    layout( yaxis = list(title = 'Proportion of time spent in MPA'), 
            xaxis = list(title = 'No-take MPA size (km)',
                         range = list(0, sxaxis+1))) %>%
    layout(hovermode = "x unified")
  return(fig)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot lifetime fishing mortality and fishing mortality offsets #### ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## For age of maturity
plotLifMort <- function(F1="0.05",sd=FALSE){
  i = which(names(returndat[[1]]$mean_Fmortality)==F1)
  
  if(sd==FALSE){
  fig2 <- plot_ly(x = returndat[[1]]$mean_Fmortality$mpa_size/1000, 
                  y = unlist(round(returndat[[1]]$mean_Fmortality[i]* 100,2)),# Round to 2 decimal places for easy viewing
                  name = str_replace_all(names(returndat)[1],'_',' '),
                  type = 'scatter', mode = 'lines+markers')
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[2]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[2],'_',' '))
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[3]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[3],'_',' ')) 
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[4]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[4],'_',' '))
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[5]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[5],'_',' '))
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[6]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[6],'_',' '))
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[7]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[7],'_',' '),
                             line = list(dash = 'dot'))  
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[8]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[8],'_',' '),
                             line = list(dash = 'dash')) 
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[9]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[9],'_',' '),
                             line = list(dash = 'dash'))  
  fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[10]]$mean_Fmortality[i]* 100,2)), 
                             name = str_replace_all(names(returndat)[10],'_',' '),
                             line = list(dash = 'dash')) 
  }
  if(sd==TRUE){
    fig2 <- plot_ly(x = returndat[[1]]$mean_Fmortality$mpa_size/1000, 
                    y = unlist(round(returndat[[1]]$mean_Fmortality[i]* 100,2)),# Round to 2 decimal places for easy viewing
                    name = str_replace_all(names(returndat)[1],'_',' '),
                    type = 'scatter', mode = 'lines+markers',
                    error_y =list(array=unlist(round(returndat[[1]]$sd_Fmortality[i]* 100,2))))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[2]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat)[2],'_',' '),
                               error_y =list(array=unlist(round(returndat[[2]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[3]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat)[3],'_',' '),
                               error_y =list(array=unlist(round(returndat[[3]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[4]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat)[4],'_',' '),
                               error_y =list(array=unlist(round(returndat[[4]]$sd_Fmortality[i]* 100,2))))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[5]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat)[5],'_',' '),
                               error_y =list(array=unlist(round(returndat[[5]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[6]]$mean_Fmortality[i]* 100,6)), 
                               name = str_replace_all(names(returndat)[6],'_',' '),
                               error_y =list(array=unlist(round(returndat[[6]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[7]]$mean_Fmortality[i]* 100,7)), 
                               name = str_replace_all(names(returndat)[7],'_',' '),
                               line = list(dash = 'dot'), 
                               error_y =list(array=unlist(round(returndat[[7]]$sd_Fmortality[i]* 100,2))))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[8]]$mean_Fmortality[i]* 100,8)), 
                               name = str_replace_all(names(returndat)[8],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat[[8]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[9]]$mean_Fmortality[i]* 100,9)), 
                               name = str_replace_all(names(returndat)[9],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat[[9]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat[[10]]$mean_Fmortality[i]* 100,10)), 
                               name = str_replace_all(names(returndat)[10],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat[[10]]$sd_Fmortality[i]* 100,2))))  
  }
  # Limits the x axis to 100 km
  fig2 <- fig2 %>% 
    layout( yaxis = list(title = 'Lifetime fishing mortality (%) to age-at-maturity',
                         range = list(0, 100)), 
            xaxis = list(title = 'No-take MPA size (km)',
                         range = list(0, 100)),
            title= list(text = paste0('Age at ',age,': ',as.numeric(F1) * 100, '% annual fishing mortality'))) %>%
    layout(hovermode = "x unified")
  return(fig2)
}

## For longevity
plotLifMort_L <- function(F1="0.05",sd=FALSE){ 
  i = which(names(returndat2[[1]]$mean_Fmortality)==F1)
  
  if(sd==FALSE){
    fig2 <- plot_ly(x = returndat2[[1]]$mean_Fmortality$mpa_size/1000, 
                    y = unlist(round(returndat2[[1]]$mean_Fmortality[i]* 100,2)),# Round to 2 decimal places for easy viewing
                    name = str_replace_all(names(returndat2)[1],'_',' '),
                    type = 'scatter', mode = 'lines+markers')
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[2]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[2],'_',' '))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[3]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[3],'_',' ')) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[4]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[4],'_',' '))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[5]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[5],'_',' '))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[6]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[6],'_',' '))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[7]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[7],'_',' '),
                               line = list(dash = 'dot'))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[8]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[8],'_',' '),
                               line = list(dash = 'dash')) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[9]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[9],'_',' '),
                               line = list(dash = 'dash'))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[10]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[10],'_',' '),
                               line = list(dash = 'dash')) 
  }
  if(sd==TRUE){
    fig2 <- plot_ly(x = returndat2[[1]]$mean_Fmortality$mpa_size/1000, 
                    y = unlist(round(returndat2[[1]]$mean_Fmortality[i]* 100,2)),# Round to 2 decimal places for easy viewing
                    name = str_replace_all(names(returndat2)[1],'_',' '),
                    type = 'scatter', mode = 'lines+markers',
                    error_y =list(array=unlist(round(returndat2[[1]]$sd_Fmortality[i]* 100,2))))
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[2]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[2],'_',' '),
                               error_y =list(array=unlist(round(returndat2[[2]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[3]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[3],'_',' '),
                               error_y =list(array=unlist(round(returndat2[[3]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[4]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[4],'_',' '),
                               error_y =list(array=unlist(round(returndat2[[4]]$sd_Fmortality[i]* 100,2))))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[5]]$mean_Fmortality[i]* 100,2)), 
                               name = str_replace_all(names(returndat2)[5],'_',' '),
                               error_y =list(array=unlist(round(returndat2[[5]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[6]]$mean_Fmortality[i]* 100,6)), 
                               name = str_replace_all(names(returndat2)[6],'_',' '),
                               error_y =list(array=unlist(round(returndat2[[6]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[7]]$mean_Fmortality[i]* 100,7)), 
                               name = str_replace_all(names(returndat2)[7],'_',' '),
                               line = list(dash = 'dot'), 
                               error_y =list(array=unlist(round(returndat2[[7]]$sd_Fmortality[i]* 100,2))))  
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[8]]$mean_Fmortality[i]* 100,8)), 
                               name = str_replace_all(names(returndat2)[8],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat2[[8]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[9]]$mean_Fmortality[i]* 100,9)), 
                               name = str_replace_all(names(returndat2)[9],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat2[[9]]$sd_Fmortality[i]* 100,2)))) 
    fig2 <- fig2 %>% add_trace(y = ~unlist(round(returndat2[[10]]$mean_Fmortality[i]* 100,10)), 
                               name = str_replace_all(names(returndat2)[10],'_',' '),
                               line = list(dash = 'dash'), 
                               error_y =list(array=unlist(round(returndat2[[10]]$sd_Fmortality[i]* 100,2))))  
  }
  # Limits the x axis to 100 km
  fig2 <- fig2 %>% 
    layout( yaxis = list(title = 'Lifetime fishing mortality (%) to longevity',
                         range = list(0, 100)), 
            xaxis = list(title = 'No-take MPA size (km)',
                         range = list(0, 100)),
            title= list(text = paste0('Age at ',age2,': ',as.numeric(F1) * 100, '% annual fishing mortality'))) %>%
    layout(hovermode = "x unified")
  return(fig2)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# fishing mortality offsets @ ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@

plotMortOff <- function(F1="0.05"){
  i = which(names(returndat[[1]]$mean_Foffset)==F1)
  
  fig3 <- plot_ly(x = returndat[[1]]$mean_Foffset$mpa_size/1000, 
                  y = unlist(round(returndat[[1]]$mean_Foffset[i],3)),# Round to 2 decimal places for easy viewing
                  name = str_replace_all(names(returndat)[1],'_',' '),
                  type = 'scatter', mode = 'lines+markers')
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[2]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[2],'_',' ')) 
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[3]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[3],'_',' '))
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[4]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[4],'_',' ')) 
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[5]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[5],'_',' ')) 
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[6]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[6],'_',' ')) 
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[7]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[7],'_',' '),
                             line = list(dash = 'dot'))
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[8]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[8],'_',' '),
                             line = list(dash = 'dash')) 
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[9]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[9],'_',' '),
                             line = list(dash = 'dash'))
  fig3 <- fig3 %>% add_trace(y = ~unlist(round(returndat[[10]]$mean_Foffset[i],3)), 
                             name = str_replace_all(names(returndat)[10],'_',' '),
                             line = list(dash = 'dash')) 
  
  # Limits the x axis to 100 km
  fig3 <- fig3 %>% 
    layout( yaxis = list(title = 'Annual fishing mortality',
                         range = list(0, as.numeric(F1))), 
            xaxis = list(title = 'No-take MPA size (km)',
                         range = list(0, 100)),
            title= list(text = paste0(as.numeric(F1) * 100, '% Annual fishing mortality'))) %>%
    layout(hovermode = "x unified")    
  return(fig3)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@
# Probability of survival @ ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@

plotProbSurv <- function(F1="0.05"){
  
  i = which(names(returndat[[1]]$mean_survival_probability)==F1)
  
  fig4 <- plot_ly(x = returndat[[1]]$mean_survival_probability$mpa_size/1000, 
                  y = unlist(round(returndat[[1]]$mean_survival_probability[i],2)),# Round to 2 decimal places for easy viewing
                  name = str_replace_all(names(returndat)[1],'_',' '), 
                  type = 'scatter', mode = 'lines+markers')
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[2]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[2],'_',' ')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[3]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[3],'_',' ')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[4]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[4],'_',' ')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[5]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[5],'_',' ')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[6]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[6],'_',' ')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[7]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[7],'_',' '),
                             line = list(dash = 'dot')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[8]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[8],'_',' '),
                             line = list(dash = 'dash')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[9]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[9],'_',' '),
                             line = list(dash = 'dash')) 
  fig4 <- fig4 %>% add_trace(y = ~unlist(round(returndat[[10]]$mean_survival_probability[i],2)), 
                             name = str_replace_all(names(returndat)[10],'_',' '), 
                             line = list(dash = 'dash')) 
  
  # Limits the x axis to 100 km
  fig4 <- fig4 %>% 
    layout( yaxis = list(title = 'Annual probability of survival',
                         range = list(0, 1)), 
            xaxis = list(title = 'No-take MPA size (km)',
                         range = list(0, 100)),
            title= list(text = paste0(as.numeric(F1) * 100, '% Annual fishing mortality'))) %>%
    layout(hovermode = "x unified")    
  return(fig4)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot number of protected individuals #### ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

plotNoInd <- function(F1="0.05"){
  
  i = which(names(returndat[[1]]$mean_protected_individuals)==F1)
  
  fig5 <- plot_ly(x = returndat[[1]]$mean_protected_individuals$mpa_size/1000, 
                  y =unlist(round(returndat[[1]]$mean_protected_individuals[i],0)),
                  name = str_replace_all(names(returndat)[1],'_',' '), 
                  type = 'scatter', mode = 'lines+markers')
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[2]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[2],'_',' ')) 
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[3]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[3],'_',' '))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[4]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[4],'_',' ')) 
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[5]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[5],'_',' '))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[6]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[6],'_',' '))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[7]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[7],'_',' '), 
                             line = list(dash = 'dot'))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[8]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[8],'_',' '), 
                             line = list(dash = 'dash'))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[9]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[9],'_',' '), 
                             line = list(dash = 'dash'))
  fig5 <- fig5 %>% add_trace(y = ~unlist(round(returndat[[10]]$mean_protected_individuals[i],0)), 
                             name = str_replace_all(names(returndat)[10],'_',' '), 
                             line = list(dash = 'dash')) 
  
  fig5 = fig5 %>%  # Add formatiing of plot xy axis and hover text with full MPA dim range
    layout( title = paste0(as.numeric(F1)*100,"% annual fishing mortality"),
            yaxis = list(title = 'Number of protected individuals in MPA'), 
            xaxis = list(title = 'No-take MPA size (km)')) %>%
    layout(hovermode = "x unified")
  return(fig5)
}

#########


