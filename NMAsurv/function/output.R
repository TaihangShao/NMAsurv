
generate_output <- function(doc,nmodel,values){

  mod = nmodel
  
  if(grepl("FP1", mod) && grepl("Frequentist", mod) && grepl("Fixed-effect", mod)){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_fp1ff_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_fp1ff_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_fp1ff_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp1ff_coeff.data),values$nma1_fp1ff_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    
  }else if(
    grepl("FP1", mod) && grepl("Frequentist", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp1fr_coeff.data),values$nma1_fp1fr_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    
  }else if(
    grepl("FP1", mod) && grepl("Bayesian", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_fp1bf_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_fp1bf_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_fp1bf_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp1bf_coeff.data),values$nma1_fp1bf_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_fp1bf_coeff.data_mu),values$nma1_fp1bf_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()

    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("FP1", mod) && grepl("Bayesian", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp1br_coeff.data),values$nma1_fp1br_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_fp1br_coeff.data_mu),values$nma1_fp1br_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
    
  }else if(
    grepl("FP2", mod) && grepl("Frequentist", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_fp2ff_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_fp2ff_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_fp2ff_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp2ff_coeff.data),values$nma1_fp2ff_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
  }else if(
    grepl("FP2", mod) && grepl("Frequentist", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp2fr_coeff.data),values$nma1_fp2fr_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
  }else if(
    grepl("FP2", mod) && grepl("Bayesian", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_fp2bf_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_fp2bf_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_fp2bf_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp2bf_coeff.data),values$nma1_fp2bf_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_fp2bf_coeff.data_mu),values$nma1_fp2bf_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("FP2", mod) && grepl("Bayesian", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_fp2br_coeff.data),values$nma1_fp2br_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_fp2br_coeff.data_mu),values$nma1_fp2br_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("PWE1", mod) && grepl("Frequentist", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_pwe1f_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_pwe1f_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_pwe1f_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_pwe1f_coeff.data),values$nma1_pwe1f_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
  }else if(
    grepl("PWE1", mod) && grepl("Bayesian", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_pwe1b_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_pwe1b_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_pwe1b_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_pwe1b_coeff.data),values$nma1_pwe1b_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_pwe1b_coeff.data_mu),values$nma1_pwe1b_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("PWE2", mod) && grepl("Frequentist", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_pwe2f_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_pwe2f_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_pwe2f_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_pwe2f_coeff.data),values$nma1_pwe2f_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
  }else if(
    grepl("PWE2", mod) && grepl("Bayesian", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_pwe2b_hr, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_pwe2b_surv, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_pwe2b_haz, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_pwe2b_coeff.data),values$nma1_pwe2b_coeff.data)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_pwe2b_coeff.data_mu),values$nma1_pwe2b_coeff.data_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Weibull", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_1, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_1, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_1, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_1),values$nma1_psm1_coeff.data_1)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_1_mu),values$nma1_psm1_coeff.data_1_mu)
    temp_table2 <- flextable(temp_table2)
    temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Weibull", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_2, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_2, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_2, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_2),values$nma1_psm1_coeff.data_2)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_2_mu),values$nma1_psm1_coeff.data_2_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Gompertz", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_3, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_3, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_3, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_3),values$nma1_psm1_coeff.data_3)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_3_mu),values$nma1_psm1_coeff.data_3_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Gompertz", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_1, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_1, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_1, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_4),values$nma1_psm1_coeff.data_4)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_4_mu),values$nma1_psm1_coeff.data_4_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Log-logistic", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_5, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_5, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_5, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_5),values$nma1_psm1_coeff.data_5)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_5_mu),values$nma1_psm1_coeff.data_5_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Log-logistic", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_6, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_6, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_6, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_6),values$nma1_psm1_coeff.data_6)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_6_mu),values$nma1_psm1_coeff.data_6_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Log-normal", mod) && grepl("Fixed-effect", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_7, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_7, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_7, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_7),values$nma1_psm1_coeff.data_7)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_7_mu),values$nma1_psm1_coeff.data_7_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Log-normal", mod) && grepl("Random-effects", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_plot1 <- tempfile(fileext = ".png")
    ggsave(temp_plot1, plot = values$graph_psm1_hr_8, width = 6, height = 4)
    temp_plot2 <- tempfile(fileext = ".png")
    ggsave(temp_plot2, plot = values$graph_psm1_surv_8, width = 6, height = 4)
    temp_plot3 <- tempfile(fileext = ".png")
    ggsave(temp_plot3, plot = values$graph_psm1_haz_8, width = 6, height = 4)
    temp_table1 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_8),values$nma1_psm1_coeff.data_8)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    temp_table2 <- cbind("Treatment" = rownames(values$nma1_psm1_coeff.data_8_mu),values$nma1_psm1_coeff.data_8_mu)
    temp_table2 <- flextable(temp_table2)
       temp_table2 <-  set_table_properties(temp_table2, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("HR Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot1, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Survival Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot2, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Hazard Plot", style = "Normal")
    doc <- doc %>% body_add_img(src = temp_plot3, width = 6, height = 4) %>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (d)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    doc <- doc %>%
      body_add_par("Coefficient Table (mu)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table2)%>%
      body_add_par("")
  }else if(
    grepl("Cox-PH Model", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$copy_df_nma2_res_hr),values$copy_df_nma2_res_hr)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (HR)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
  }else if(
    grepl("Generalised Gamma Model", mod)
  ){
    doc <- doc %>%
      body_add_par(mod, style = "heading 1")
    temp_table1 <- cbind("Treatment" = rownames(values$copy_df_nma3_res_hr),values$copy_df_nma3_res_hr)
    temp_table1 <- flextable(temp_table1)
    temp_table1 <-  set_table_properties(temp_table1, layout = "autofit") %>% 
      autofit()
    
    doc <- doc %>%
      body_add_par("Coefficient Table (Treatment effects, Beta)", style = "Normal")
    doc <- doc %>% body_add_flextable(temp_table1)%>%
      body_add_par("")
    
  }
  
  return(doc)
  
}


