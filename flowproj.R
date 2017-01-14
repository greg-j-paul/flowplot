


#edges = data.frame(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),
#                   N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),
#                   Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),
#                   stringsAsFactors = F)

#edges = edges[sample(c(TRUE, FALSE), nrow(edges), replace = TRUE, prob = c(0.8, 0.2)),]
#head(edges)



####################################################################
#' A function to plot flows in R.
#' This is a slight improvement over the RiverPlot package with slightly better display settings
#' @param edges a matrix of format:  N1 , N2 , Value
#' @keywords RiverPlot
#' @export
#' @examples
#' edges = data.frame(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),
#' N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),
#' Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),
#' stringsAsFactors = F)
#' plotflows(edges)



# take a matrix of format:
# N1, N2 , Value
# N1 & N2 represent the transition 
# i.e. if you go from state A to state B N1 = A1 and N2 = B2
# the number after the state name is used to decide where to put the state in the flow diagram
# assign x and y based on the no of x and y as well as values
# plot in ggplot
# x and y assignment done with splines
# minimal size value for each stage is 1 for plotting reasons(which may be inaccurate)
# spacing parameter affects state spacing in final graph
# spacing <- 2
# colorset <- "Set1"
# ONLY SUPPORTS 2 DIGITS LAYERS i.e. x values


#user supplies edges matrix
#code begins below
library(stringi)
library(RColorBrewer)
library(ggplot2) 
library(dplyr)
library(FField)


plotflows <- function( edges ){
  
  nodes = data.frame(ID = unique(c(edges$N1, edges$N2)), stringsAsFactors = FALSE)
  #x and y represent final position labels
  #do 2 rounds with 1 and 2 length
  #the number represents where on the x axis the state is plotted as
  d1 <- stri_extract_last_regex(nodes$ID, "\\d{1}")
  d1[is.na(d1)] <- 0
  d2 <- stri_extract_last_regex(nodes$ID, "\\d{2}")
  d2[is.na(d2)] <- 0
  nodes$x = pmax(d1,d2)
  
  
  
  #gets the length of the x value and assumes that what is remaining is the state string
  positionlengths  <- stri_length(nodes$x)
  idlengths <- stri_length(nodes$ID)
  statenames <- substring(nodes$ID, 1, idlengths - positionlengths )
  statenames <- as.factor(statenames)
  nodes$y <- as.numeric(statenames) * spacing
  nodes$statenames <- statenames
  rownames(nodes) = nodes$ID
  
  palette = brewer.pal(length(levels(statenames)), colorset)
  
  #fields <- names(df_normal) 
  
  #### repeating the extration and factor for edges
  # used later on for color for transfers
  # colors should correspond to the color of origin
  ed1 <- stri_extract_last_regex(edges$N1, "\\d{1}")
  ed1[is.na(ed1)] <- 0
  ed2 <- stri_extract_last_regex(edges$N1, "\\d{2}")
  ed2[is.na(ed2)] <- 0
  edx = pmax(ed1,ed2)
  edpositionlengths  <- stri_length(edx)
  idN1 <- stri_length(edges$N1)
  n1statenames <- substring(edges$N1, 1, idN1 - edpositionlengths )
  edges$n1statenames <- as.factor(n1statenames)
  
  N1sizes <- summarise (group_by(edges, N1), sum(Value))
  N1sizes <- as.data.frame(N1sizes)
  N2sizes <- summarise (group_by(edges, N2), sum(Value))
  N2sizes <- as.data.frame (N2sizes)
  Nsizes <- merge(N1sizes , N2sizes, by.x = "N1" , by.y = "N2")
  Nsizes$tot <-  Nsizes$`sum(Value).x`+ Nsizes$`sum(Value).y`
  # adding in missing values as the starting level will be excluded
  N1matches <-  N1sizes$N1 %in% Nsizes$N1
  N1append <- N1sizes[!N1matches,]
  Nsizes <- Nsizes[-3:-2]
  colnames(N1append) <- colnames(Nsizes) 
  Nsizes <- rbind(Nsizes, N1append)
  #order according to N1
  Nsizes <- Nsizes [order(Nsizes$N1),]
  #get matching nodes and then take the total values
  nodematch <- nodes$ID %in% Nsizes$N1
  #min size is 1 for plotting
  nodesize <- rep(1, nrow(nodes) )
  nodesize[nodematch]   <- Nsizes$tot[nodematch]
  nodes$nodesize <- nodesize
  
  
  p <- ggplot(data = nodes, aes(colour = statenames))
  p <- p + geom_point( aes(x = x , y = y , size = nodesize) )
  for (i in 1:nrow(edges)) { 
    
    labelx  <-  ( (as.numeric (nodes[edges$N2[i],]$x) +   as.numeric (nodes[edges$N1[i],]$x) )/2 )  
    labely  <-  ( (as.numeric (nodes[edges$N2[i],]$y) +   as.numeric (nodes[edges$N1[i],]$y) )/2 )     
    labeltext <-   format(round(edges$Value[i], 2), nsmall = 2)
    
    if ( nodes[edges$N1[i],]$y !=  nodes[edges$N2[i],]$y)
    {
      
      
      
      loop_input = paste0("geom_curve(aes(x= "
                          ,nodes[edges$N1[i],]$x
                          ," ,y= ",nodes[edges$N1[i],]$y
                          ," ,xend = "
                          ,  nodes[edges$N2[i],]$x
                          ," ,yend = "
                          , nodes[edges$N2[i],]$y
                          ,"), ncp = 1000"
                          ,", color = '"
                          , palette [as.numeric(edges$n1statenames[i])]
                          ,"'"
                          ,", alpha = 0.1"
                          ,", size ="
                          ,edges$Value[i]
                          ,")"
      )
    }
    
    else{
      loop_input = paste0("geom_segment(aes(x= "
                          ,nodes[edges$N1[i],]$x
                          ," ,y = ",nodes[edges$N1[i],]$y
                          ," , xend ="
                          ,  nodes[edges$N2[i],]$x
                          ," , yend = "
                          , nodes[edges$N2[i],]$y
                          ," ),"
                          ,", color = '"
                          , palette [as.numeric(edges$n1statenames[i])]
                          ,"'"
                          ,", alpha = 0.1"
                          ,", size ="
                          ,edges$Value[i]
                          ,")"
      )
      
      
      
    }
    
    
    #loop_input <- paste0(loop_input, textlabel)
    
    p <- p + eval(parse(text=loop_input))  
  }
  
  
  #missing something obvious but have to move label to this section as combining curves + label = error
  
  
  for (i in 1:nrow(edges)) { 
    
    labelx  <-  ( (as.numeric (nodes[edges$N2[i],]$x) +   as.numeric (nodes[edges$N1[i],]$x) )/2 )  
    labely  <-  ( (as.numeric (nodes[edges$N2[i],]$y) +   as.numeric (nodes[edges$N1[i],]$y) )/2 )     
    labeltext <-   format(round(edges$Value[i], 2), nsmall = 2)
    
    
    textlabel <- paste0(
      "geom_text( aes( x = "
      ,labelx
      ,", y = " 
      ,labely
      ,"), label ="  
      ,labeltext
      ,", color = 'black' , nudge_x = 0.1 , alpha = 0.5)")
    
    p <- p + eval(parse(text=textlabel)) 
    
  }
  
  p <- p + scale_colour_manual(values = palette)
  
  return(p)
  
}


#plotflows(edges)

