source('../EXPANSE_algorithm/scr/fun_call_lib.R')

# Read in the files
file_dir <- 'data/workingData/'
csv_names <- list.files(file_dir, 'gtwr_param_')
csv_names <- csv_names[1:30]
csv_names <- csv_names[!grepl('06-12', csv_names)]
params <- lapply(paste0(file_dir, csv_names), read.csv)
params <- lapply(seq_along(csv_names), function(csv_i){
   params[[csv_i]]$poll <- strsplit(csv_names[csv_i], '_')[[1]][4]
   params[[csv_i]]$period <- strsplit(csv_names[csv_i], '_')[[1]][5]
   params[[csv_i]]$fold <- strsplit(csv_names[csv_i], '_')[[1]][7] %>% 
      gsub('.csv', '', .)
   params[[csv_i]]$csv_name <- paste(strsplit(csv_names[csv_i], '_')[[1]][4:7], collapse = '_') %>% 
      gsub('.csv', '', .)
   params[[csv_i]]
})
params <- do.call(rbind, params)
names(params)  #ksi>1.570     lamda==0.2
varied_lamda <- function(lamdat){
   ggplot(params %>% filter(period=='00-19', lamda==lamdat),
          aes(x=as.factor(ksi),
                                                               y=as.factor(conv_dist), fill=rsq))+
      geom_tile()+
      labs(x='ksi', y='conv_dist', fill='rsq', title=paste0('lamda=', lamdat))+
      facet_wrap(csv_name~.)+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 13),
            axis.text.x = element_text(angle = 90),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text.y = element_text(size = 15))+
      # scale_color_gradientn(colours = topo.colors(5))
      # scale_fill_distiller(palette = "magma")
      scale_fill_gradient2(
         low = 'red', mid = 'green', high = 'blue',
         midpoint = 0,
         guide = 'colourbar', aesthetics = 'fill',
         breaks=c(-0.8, 0, 0.3, 0.4, 0.6)
      )
   # scale_fill_gradient(low = "red", high = "green")
   # scale_colour_gradient(low = "white", high = "black")
   # scale_fill_gradient2(
   #    low = 'red', mid = 'green', high = 'blue',
   #    midpoint = -0.2, guide = 'colourbar', aesthetics = 'fill'
   # )
   # scale_fill_brewer(palette = 'YlOrRd')
}
varied_ksi <- function(ksit){
   ggplot(params %>% mutate(ksi=round(ksi, 1)) %>% filter(period=='00-19', ksi==ksit),
          aes(x=as.factor(lamda),
              y=as.factor(conv_dist), fill=rsq))+
      geom_tile()+
      labs(x='lamda', y='conv_dist', fill='rsq', title=paste0('ksi=', ksit))+
      facet_wrap(csv_name~.)+
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 13),
            axis.text.x = element_text(angle = 90),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            strip.text.y = element_text(size = 15))+
      # scale_color_gradientn(colours = topo.colors(5))
      # scale_fill_distiller(palette = "magma")
      scale_fill_gradient2(
         low = 'red', mid = 'green', high = 'blue',
         midpoint = 0,
         guide = 'colourbar', aesthetics = 'fill',
         breaks=c(-0.8, 0, 0.3, 0.4, 0.6)
      )
   # scale_fill_gradient(low = "red", high = "green")
   # scale_colour_gradient(low = "white", high = "black")
   # scale_fill_gradient2(
   #    low = 'red', mid = 'green', high = 'blue',
   #    midpoint = -0.2, guide = 'colourbar', aesthetics = 'fill'
   # )
   # scale_fill_brewer(palette = 'YlOrRd')
}
lapply(c(0.2, 0.6, 1.0), varied_lamda)
lapply(c(0.0, 0.8, 1.6), varied_ksi)
# ksi has less influence on the validation result
# ksi=0; lamda=0.2; conv_dist=5000
