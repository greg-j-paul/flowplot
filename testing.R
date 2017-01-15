
edges = data.frame(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),
                   N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),
                   Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),
                   stringsAsFactors = F)


edges = edges[sample(c(TRUE, FALSE), nrow(edges), replace = TRUE, prob = c(0.8, 0.2)),]


library(plotflows)
x<- plotflows(edges, spacing = 3)
x+theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()
        #,panel.background = element_blank(), panel.grid = element_blank() , plot.background = element_blank()
        
)+ labs(size="Node Size", colour="State Name")


formattedflowplot <- function(edges, spacing = 2, colorset = "Set1" ){
  
  p <- plotflows(edges, spacing = 2, colorset = "Set1")
  
  return( p+theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()
                     ,panel.background = element_blank(), panel.grid = element_blank() , plot.background = element_blank()
                     
  )+ labs(size="Node Size", colour="State Name") )
  
} 


formattedflowplot(edges)
