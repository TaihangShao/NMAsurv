options(encoding = "UTF-8")
options (warn = -1)
# Sys.setenv(CLIPR_ALLOW = "TRUE")
server <- function(input, output, session) {

  source("function/aggregate.R")
  source("function/output.R")
  ###### All the helpers
  observe_helpers(help_dir = "help_files")
  
  ###### Stop R if Shiny window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ###### Dynamic variables
  values <- reactiveValues(sheet3 = NULL)
  models <- reactiveValues(ran_models = list())
  opres <- reactiveValues(ldoc = list())
  
  # example data
  values$sheet_b <- read_excel("www/files/Baseline.xlsx", sheet = 1)[,1:5]
  output$baseline <- renderDT(datatable(values$sheet_b, editable = FALSE, rownames = FALSE,
                                        options = list(
                                          paging = FALSE,   
                                          searching = FALSE, 
                                          dom = 't'         
                                        )
  ))
  

  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("Reference_manual", "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("www/manual.pdf", file)
    }
  )
  
  #### Import ipd ####
  # Upload button
  
  observe({
    infile <- input$upload2
    if (!is.null(infile)){
      tryCatch({
         data <- read_excel(infile$datapath, sheet = 1)
        if (ncol(data) != 7) {
          shinyalert("Error!", "The data must have exactly 7 columns. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        expected_colnames <- c("n", "patid", "time", "event", "arm", "study", "treatment")
        if (!all(colnames(data) == expected_colnames)) {
          shinyalert("Error!", "Column names do not match expected names. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[1:5], is.numeric))) {
          shinyalert("Error!", "Incorrect data type. The first five columns should be numeric. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[6:7], is.character))) {
          shinyalert("Error!", "Incorrect data type. The last two columns should be character. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        values$sheet3 <- data
        shinyalert(title = "Input data successfully!", type = "success")
        
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$ipd_text <- renderText({
      paste0("Loaded IPD: ",infile$datapath)
    })
  })
  
  observeEvent(input$FCexample2, {
    # Load example file
    wd <- getwd()
    infile <- c()
    infile$datapath <- paste0(wd,"/www/files/example2.xlsx")
    
    if (!is.null(infile)){
      tryCatch({
        values$sheet3 <- read_excel(infile$datapath, sheet = 1)[,1:7]
        shinyalert(title = "Load example data successfully!", type = "success")
        
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$ipd_text <- renderText({
      paste0("Loaded IPD: Example IPD")
    })
  })
  
  observeEvent(input$FCexample2 ,{
    DF<-as.data.frame(values$sheet3)
    Treatment_name<-unique(DF$treatment)
    Treatment_code<-Treatment_name
    for (i in 1:length(Treatment_name)) {
      Treatment_code[i]=i
    }
    Study_name<-unique(DF$study)
    Study_arm<-Study_code<-Study_name
    for (i in 1:length(Study_name)) {
      Study_code[i]=i
      Study_arm[i]=length(unique(DF$treatment[DF$study==Study_name[i]]))
    }
    # Treatment_code<-seq(1,length(Treatment_name),1)
    Treatment<-as.data.frame(cbind(Treatment_name,Treatment_code))
    Study<-as.data.frame(cbind(Study_name,Study_code,Study_arm))
    
    index <- match(DF$treatment, Treatment$Treatment_name)
    DF$txCode <- Treatment$Treatment_code[index]
    index <- match(DF$study, Study$Study_name)
    DF$studyCode <- Study$Study_code[index]
    
    values$data2<-DF #data
    values$nma2_trt<-Treatment
    values$nma2_stu<-Study
    output$df_nma_ipd_t <- renderDT(datatable(values$nma2_trt, editable = FALSE, rownames = FALSE, 
                                              options = list(pageLength = 10)
    ))
    output$df_nma_ipd_s <- renderDT(datatable(values$nma2_stu, editable = FALSE, rownames = FALSE, 
                                              options = list(pageLength = 10)
    ))

  })
  
  observeEvent(input$upload2 ,{
    DF<-as.data.frame(values$sheet3)
    Treatment_name<-unique(DF$treatment)
    Treatment_code<-Treatment_name
    for (i in 1:length(Treatment_name)) {
      Treatment_code[i]=i
    }
    Study_name<-unique(DF$study)
    Study_arm<-Study_code<-Study_name
    for (i in 1:length(Study_name)) {
      Study_code[i]=i
      Study_arm[i]=length(unique(DF$treatment[DF$study==Study_name[i]]))
    }
    # Treatment_code<-seq(1,length(Treatment_name),1)
    Treatment<-as.data.frame(cbind(Treatment_name,Treatment_code))
    Study<-as.data.frame(cbind(Study_name,Study_code,Study_arm))
    
    index <- match(DF$treatment, Treatment$Treatment_name)
    DF$txCode <- Treatment$Treatment_code[index]
    index <- match(DF$study, Study$Study_name)
    DF$studyCode <- Study$Study_code[index]
    
    values$data2<-DF #data
    values$nma2_trt<-Treatment
    values$nma2_stu<-Study
    output$df_nma_ipd_t <- renderDT(datatable(values$nma2_trt, editable = FALSE, rownames = FALSE,
                                              options = list(pageLength = 10)
    ))
    output$df_nma_ipd_s <- renderDT(datatable(values$nma2_stu, editable = FALSE, rownames = FALSE,
                                              options = list(pageLength = 10)
    ))
  })
  
  output$df_nma2 <- renderDT({
    req(values$sheet3)
    datatable(values$sheet3, editable = FALSE, rownames = FALSE,
              options = list(pageLength = 10)
    )})
  
  ### control tab
  
  observe({
    if (is.null(values$data2) || nrow(values$data2) == 0) {
      session$sendCustomMessage('controlTab1', list(tabName = "IPD-based NMA", enable = FALSE))
    } else {
      session$sendCustomMessage('controlTab1', list(tabName = "IPD-based NMA", enable = TRUE))
    }
  })
  
  observe({
    if (is.null(values$data1) || nrow(values$data1) == 0) {
      session$sendCustomMessage('controlTab2', list(tabName = "AD-based NMA", enable = FALSE))
    } else {
      session$sendCustomMessage('controlTab2', list(tabName = "AD-based NMA", enable = TRUE))
    }
  })

  observe({
    if(!is.null(input$upload2) || input$FCexample2 > 0 || !is.null(input$upload1) || input$FCexample1 > 0) {
      session$sendCustomMessage('enableTab', list(enable = TRUE))
    }
  })
  
  observe({
    if(!is.null(input$uploade2) || input$FCexample2 > 0) {
      # session$sendCustomMessage('enableTab', list(enable = TRUE))
      session$sendCustomMessage('enableTab2', list(enable = TRUE))
      session$sendCustomMessage('enableTab4', list(enable = TRUE))
    }
  })
  
  observe({
    if(!is.null(input$upload1) || input$FCexample1 > 0) {
      session$sendCustomMessage('enableTab1', list(enable = TRUE))
    }
  })

  observe({
    req(input$ready3)
    if(input$ready3 > 0) {
      session$sendCustomMessage('enableTab3', list(enable = TRUE))
    }
  })
  

  #### end of Import ipd ####
  
  #### import ad ####
  observe({
    infile <- input$upload1
    if (!is.null(infile)){
      tryCatch({
        data <- read_excel(infile$datapath, sheet = 1)
        if (ncol(data) != 8) {
          shinyalert("Error!", "The data must have exactly 8 columns. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        expected_colnames <- c("studyn","trtn","time","timeDelta","nevents","natrisk","study","treatment")
        if (!all(colnames(data) == expected_colnames)) {
          shinyalert("Error!", "Column names do not match expected names. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[1:6], is.numeric))) {
          shinyalert("Error!", "Incorrect data type. The first six columns should be numeric. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[7:8], is.character))) {
          shinyalert("Error!", "Incorrect data type. The last two columns should be character. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        values$sheet2<-data
        shinyalert(title = "Input data successfully!", type = "success")
        
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$ad_text <- renderText({
      paste0("Loaded AD: ",infile$datapath)
    })
  })
  
  observeEvent(input$FCexample1, {
    # Load example file
    wd <- getwd()
    infile <- c()
    infile$datapath <- paste0(wd,"/www/files/example1.xlsx")
    
    if (!is.null(infile)){
      tryCatch({
        values$sheet2 <- read_excel(infile$datapath, sheet = 1)[,1:8]
        shinyalert(title = "Load example data successfully!", type = "success")
        
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    output$ad_text <- renderText({
      paste0("Loaded AD: Example AD")
    })
  })

  observeEvent(input$FCexample1 ,{
    df_ad<-values$sheet2 #data
    Treatment_name<-unique(df_ad$treatment)
    Treatment_code<-unique(df_ad$trtn)
    Study_name<-unique(df_ad$study)
    Study_code<-unique(df_ad$studyn)
    Treatment<-as.data.frame(cbind(Treatment_name,Treatment_code))
    Study<-as.data.frame(cbind(Study_name,Study_code))
    Treatment<-arrange(Treatment,Treatment_code)
    Study<-arrange(Study,Study_code)
    
    output$df_nma_ad_t <- renderDT(datatable(Treatment, editable = FALSE, rownames = FALSE, 
                                             options = list(pageLength = 10)
    ))
    output$df_nma_ad_s <- renderDT(datatable(Study, editable = FALSE, rownames = FALSE, 
                                             options = list(pageLength = 10)
    ))
    values$data1<-values$sheet2[,1:6] #data
    values$nma1_trt<-Treatment
    values$nma1_stu<-Study
  })
  
  observeEvent(input$upload1 ,{
    df_ad<-values$sheet2 #data
    Treatment_name<-unique(df_ad$treatment)
    Treatment_code<-unique(df_ad$trtn)
    Study_name<-unique(df_ad$study)
    Study_code<-unique(df_ad$studyn)
    Treatment<-as.data.frame(cbind(Treatment_name,Treatment_code))
    Study<-as.data.frame(cbind(Study_name,Study_code))
    Treatment<-arrange(Treatment,Treatment_code)
    Study<-arrange(Study,Study_code)
    
    output$df_nma_ad_t <- renderDT(datatable(Treatment, editable = FALSE, rownames = FALSE,
                                             options = list(pageLength = 10)
    ))
    output$df_nma_ad_s <- renderDT(datatable(Study, editable = FALSE, rownames = FALSE,
                                             options = list(pageLength = 10)
    ))
    values$data1<-values$sheet2[,1:6] #data
    values$nma1_trt<-Treatment
    values$nma1_stu<-Study
  })
  
  output$df_nma_ad <- renderDT({
    req(values$sheet2)
    datatable(values$sheet2, editable = FALSE, rownames = FALSE, 
              options = list(pageLength = 10))
  })
  
  #### end of import ad ####
  
  ###### Transform Import page ####
  # Upload button

  #input1
  observeEvent(input$FCexample2, {
    output$tran_length1 <- renderUI({
      numericInput(inputId = "tran_length",label = "The max timepoints (month)",value = 60)%>%
        helper(type = "markdown", content = "tran1")
    })
    output$tran_step1 <- renderUI({
      numericInput(inputId = "tran_step",label = "Step of the timepoints (month)",value = 6)%>%
        helper(type = "markdown", content = "tran2")
    })
    output$tran_ready <- renderUI({
      actionButton("ready3", "Transform to aggregated data", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
  })
  #input2
  observeEvent(input$upload2, {
    output$tran_ref1 <- renderUI({
      numericInput(inputId = "tran_ref",label = "Reference study number",min = 0,max = length(Treatment$Treatment_name),value = 3)%>%
        helper(type = "markdown", content = "trans")
    })
    output$tran_length1 <- renderUI({
      numericInput(inputId = "tran_length",label = "The max timepoints",value = 60)
    })
    output$tran_step1 <- renderUI({
      numericInput(inputId = "tran_step",label = "Step of the timepoints",value = 6)
    })
    output$tran_ready <- renderUI({
      actionButton("ready3", "Transform to aggregated data", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
  })

  observeEvent(input$ready3, {

    data<-values$data2
    len<-input$tran_length
    step<-input$tran_step
    # Select time points for aggregating data
    timepoints=c(seq(step,len,step),120)
    # Time points including zero
    timepoints2=c(seq(0,len,step),120)
    # Apply function
    anova <- anova_data(timepoints=timepoints, timepoints2=timepoints2,
                        df=data)
    anova<-as.data.frame(cbind(anova$studyn,anova$trtn,anova$time,anova$'y.max',anova$nevents,anova$natrisk,anova$study,anova$treatment))
    colnames(anova)<-c("studyn","trtn","time","timeDelta","nevents","natrisk","study","treatment")

    values$anova<-anova
    values$anova$studyn<-as.numeric(values$anova$studyn)
    values$anova$trtn<-as.numeric(values$anova$trtn)
    values$anova$time<-as.numeric(values$anova$time)
    values$anova$timeDelta<-as.numeric(values$anova$timeDelta)
    values$anova$nevents<-as.numeric(values$anova$nevents)
    values$anova$natrisk<-as.numeric(values$anova$natrisk)

    df_ad<-values$anova
    Treatment_name<-unique(df_ad$treatment)
    Treatment_code<-unique(df_ad$trtn)
    Study_name<-unique(df_ad$study)
    Study_code<-unique(df_ad$studyn)
    Treatment<-as.data.frame(cbind(Treatment_name,Treatment_code))
    Study<-as.data.frame(cbind(Study_name,Study_code))
    Treatment<-arrange(Treatment,Treatment_code)
    Study<-arrange(Study,Study_code)
    
    values$df_trans<-Treatment
    values$df_trans1<-Study
    
    output$tran_agg <- renderDT(datatable(values$anova, editable = FALSE, rownames = FALSE,
                                          options = list(pageLength = 10)
    ))
    output$downloadtable_tran_agg <- downloadHandler(
      filename = function() {
        paste("aggregated_data", ".csv", sep="")
      },
      content = function(file) {
        write.csv(anova, file,row.names = FALSE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  observeEvent(input$trans_ad_analysis,{
    values$data1 <- values$anova[,1:6]
    values$nma1_trt <- values$df_trans
    values$nma1_stu <- values$df_trans1

    output$ad_text <- renderText({
      paste0("Loaded AD: Transformed Data")
    })
    shinyalert(title = "Complete!", type = "success")
  })

  observeEvent(input$ready3, {
    updateTabsetPanel(session, "data_transform_subpanel", selected ="show_data_transform_panel")
  })
  
  #####
  
  ##### ph test ####
  # Upload button
  
  #Show the dataframes 
  observeEvent(input$FCexample2,{
    output$pht_ready <- renderUI({
      actionButton("readypht", "Run PH assumption test", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
    data<-as.data.frame(values$sheet3)
    study<-unique(data$study)
    treatment<-unique(data$treatment)
    
    output$selectStudy <- renderUI({
      selectInput("pht_studyInput", "Select Study", choices = study)
    })
    
    output$selectTreatment1 <- renderUI({
      selectInput("pht_treatmentInput1", "Select Treatment 1", 
                  choices = unique(filter(data,study %in% input$pht_studyInput)$treatment))
    })
    
    output$selectTreatment2 <- renderUI({
      selectInput("pht_treatmentInput2", "Select Treatment 2", 
                  choices = unique(filter(data,study %in% input$pht_studyInput)$treatment))
    })
    
  })
  
  observeEvent(input$upload2,{
    output$pht_ready <- renderUI({
      actionButton("readypht", "Run PH assumption test", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
    data<-as.data.frame(values$sheet3)
    study<-unique(data$study)
    treatment<-unique(data$treatment)
    
    output$selectStudy <- renderUI({
      selectInput("pht_studyInput", "Select Study", choices = study)
    })
    
    output$selectTreatment1 <- renderUI({
      selectInput("pht_treatmentInput1", "Select Treatment 1", 
                  choices = unique(filter(data,study %in% input$pht_studyInput)$treatment))
    })
    
    output$selectTreatment2 <- renderUI({
      selectInput("pht_treatmentInput2", "Select Treatment 2", 
                  choices = unique(filter(data,study %in% input$pht_studyInput)$treatment))
    })
    
  })
  
  #input1 
  observeEvent(input$readypht, {
    if (input$pht_treatmentInput1 == input$pht_treatmentInput2) {
      shinyalert("Warning!", "You should not select the same treatment in this test.", type = "error")
    } else {
      data<-as.data.frame(values$sheet3)
      study<-unique(data$study)
      treatment<-unique(data$treatment)
      
      data0 <- data %>%
        filter(study %in% input$pht_studyInput,
               treatment %in% c(input$pht_treatmentInput1, input$pht_treatmentInput2))
      
      fit <- coxph(formula = Surv(time, event) ~ arm, data = data0)
      df_srp <- cox.zph(fit)
      fit1<-survfit(Surv(time, event) ~ arm, data = data0)
      fit2<-tidy(fit1)
      resid <- residuals(fit, type = "martingale")
      
      # get the p value
      global_p <- df_srp$table[nrow(df_srp$table), "p"]
      p_text <- paste0("Global Schoenfeld Test p: ", signif(global_p, 5))
      
      ## print the P value
      output$text_pht_3 <- renderPrint({
        HTML("<u><strong>Schoenfeld Test results (P value)</strong></u>")
      })
      
      output$text_pht_2 <- renderPrint({
        cat(p_text)
      })
      
      
      ## Schoenfeld residual plot
      output$plot_pht_1<-renderPlot({
        f1<-ggcoxzph(df_srp,
                     point.col = "red",
                     point.size = 2,
                     point.shape = 19,
                     point.alpha = 2,
                     ggtheme = theme(
                       axis.title.x = element_text(size = 16),  # x轴标题字体大小和样式
                       axis.title.y = element_text(size = 16),  # y轴标题字体大小和样式
                       axis.text.x = element_text(size = 14),  # x轴刻度标签字体大小
                       axis.text.y = element_text(size = 14),  # y轴刻度标签字体大小
                       axis.line = element_line(size = 1),   # 坐标轴线条粗细
                       axis.ticks = element_line(size = 1),    # 坐标轴刻度线粗细
                       plot.title = element_text(size = 18, hjust = 0,face = "bold.italic", color = "red"),  # 图形标题字体大小、样式和对齐
                       panel.background = element_rect(fill = "white"),
                       panel.grid.major = element_line(colour = "white"),
                       panel.border = element_rect(color = "black", fill = NA, size = 1)
                     )) +  xlab("Time (in months)") + labs(title = "Schoenfeld residual plot (Time in months)")
        print(f1)
        values$plot_pht_1<-f1
      })
      
      # Buttons to customize and download the plot (h and w)
      observeEvent(input$ph_1_dw, {
        output$pht_1_sizebut <- renderUI({
          bt <- tagList()
          bt[[1]] <- numericInput("pht_1_height", "Height (px)", value = 600)
          bt[[2]] <- numericInput("pht_1_width", "Width (px)", value = 800)
          bt
        })
      })
      
      output$downloadPlot_pht_1 <- downloadHandler(
        filename = function() {
          paste0("Schoenfeld_residual_plot.png")
        },
        content = function(file) {
          png(file,width = as.numeric(input$pht_1_width),
              height = as.numeric(input$pht_1_height))
          print(values$plot_pht_1)
          dev.off()
        }
      )
      

      
      ### Log-log
      output$plot_pht_2<-renderPlot({
        f2<-ggplot(fit2, aes(x = time, color = strata)) +
          geom_step(aes(y = log(-log(estimate))),linewidth=1) +
          xlab("Log(Time) (in Months)") +
          scale_x_log10(labels = label_number())+
          ylab("Log(-Log(Survival Probability))") +
          labs(title="Log-Log Plot (Time in months)")+
          scale_color_discrete(
            name = "Treatment", 
            labels = c(treatment[1], treatment[2]))+
          theme(
            axis.title.x = element_text(size = 16),  # x轴标题字体大小和样式
            axis.title.y = element_text(size = 16),  # y轴标题字体大小和样式
            axis.text.x = element_text(size = 14),  # x轴刻度标签字体大小
            axis.text.y = element_text(size = 14),  # y轴刻度标签字体大小
            axis.line = element_line(size = 1),   # 坐标轴线条粗细
            axis.ticks = element_line(size = 1),    # 坐标轴刻度线粗细
            plot.title = element_text(size = 18, face = "bold.italic"),  # 图形标题字体大小、样式和对齐
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "gray"),
            panel.border = element_rect(color = "black", fill = NA, size = 1),
            legend.title = element_text(size = 14, face = "bold", color = "black"),  # 设置图例标题的字体大小、样式和颜色
            legend.text = element_text(size = 12, face = "italic", color = "black"),  # 设置图例文本的字体大小、样式和颜色
            legend.background = element_rect(fill = "lightgrey", color = "black", size = 0.5, linetype = "solid"),  # 设置图例背景的填充颜色和边框
            legend.key = element_rect(fill = "white", color = "black")  # 设置图例键的背景和边框
          )
        print(f2)
        values$plot_pht_2<-f2
      })
      
      observeEvent(input$ph_2_dw, {
        output$pht_2_sizebut <- renderUI({
          bt <- tagList()
          bt[[1]] <- numericInput("pht_2_height", "Height (px)", value = 1600)
          bt[[2]] <- numericInput("pht_2_width", "Width (px)", value = 2800)
          bt
        })
      })
      
      # Download back-end
      output$downloadPlot_pht_2 <- downloadHandler(
        filename = function() { paste0('Log_log_plot.png') },
        content = function(file) {
          ggsave(file,values$plot_pht_2, width = as.numeric(input$pht_2_width),
                 height = as.numeric(input$pht_2_height),
                 units = "px")
        })
      
      output$text_pht_4 <- renderPrint({
        HTML("<u><strong>Grambsch-Therneau Test results</strong></u>")
      })
      
      output$text_pht_1 <- renderPrint({
        df_srp
      })
      
      shinyalert(title = "Complete!", type = "success")
      
    }
  })
  
  # end of PH test
  ##### 
 
  #### Network Plot ####
  # Upload button
  observe({
    infile <- input$uploadnp
    if (!is.null(infile)){
      tryCatch({
        data <- read_excel(infile$datapath, sheet = 1)
        if (ncol(data) != 9) {
          shinyalert("Error!", "The data must have exactly 9 columns. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        expected_colnames <- c("studyCode","Study","t1","t2","treat1","treat2","hr","lower","upper")
        if (!all(colnames(data) == expected_colnames)) {
          shinyalert("Error!", "Column names do not match expected names. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[c(1,3,4,7:9)], is.numeric))) {
          shinyalert("Error!", "Incorrect data type. Check the columns which should be numeric. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        if (!all(sapply(data[c(2,5,6)], is.character))) {
          shinyalert("Error!", "Incorrect data type. Check the columns which should be character. Please refer to the data input instruction panel or review the example data.", type = "error")
          return()
        }
        values$sheetnp <- data
        shinyalert(title = "Input data successfully!", type = "success")
        
        },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
  })
  
  observeEvent(input$FCexamplenp, {
    # Load example file
    wd <- getwd()
    infile <- c()
    infile$datapath <- paste0(wd,"/www/files/examplenp.xlsx")
    
    if (!is.null(infile)){
      tryCatch({
        values$sheetnp <- read_excel(infile$datapath, sheet = 1)
        shinyalert(title = "Load example data successfully!", type = "success")
        
      },
      error = function(e)
        shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
      )
    }
    
  })
  
  #Show the dataframes
  observeEvent(input$FCexamplenp,{
    output$df_nma_np <- renderDT(datatable(values$sheetnp[,1:9], editable = FALSE, rownames = FALSE, 
                                           options = list(pageLength = 10)
    ))
    output$np_ready <- renderUI({
      actionButton("readynp", "Draw the plot", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
    output$nptitle <- renderUI({
      textInput("np_title", "Title of the plot",value="Network Plot")
    })
    output$nplcol <- renderUI({
      textInput("np_lcol", "Colour of the line",value="skyblue")
    })
    output$npmultiarm <- renderUI({
      selectInput("np_multiarm", "Multi-Arm",
                  choices = c("TRUE" = TRUE,
                              "FALSE" = FALSE
                  ),
                  selected = FALSE
      )
    })
    output$npmcol <- renderUI({
      textInput("np_mcol", "Colour of the multiarm area",value="pink")
    })
    output$nppoint <- renderUI({
      textInput("np_point", "Colour of the points",value="purple")
    })
    output$npword <- renderUI({
      textInput("np_word", "Colour of the interior of words",value="black")
    })
    output$npbg <- renderUI({
      textInput("np_bg", "Colour of the edge of the words",value="white")
    })
    
  })
  
  observeEvent(input$uploadnp,{
    output$df_nma_np <- renderDT(datatable(values$sheetnp[,1:9], editable = FALSE, rownames = FALSE, 
                                           options = list(pageLength = 10)
    ))
    output$np_ready <- renderUI({
      actionButton("readynp", "Draw the plot", icon("gears"),
                   style="color: black;
                         background-color: white; border-color: #2e6da4")
    })
    output$nptitle <- renderUI({
      textInput("np_title", "Title of the plot",value="Network Plot")
    })
    output$nplcol <- renderUI({
      textInput("np_lcol", "Colour of the line",value="skyblue")
    })
    output$npmultiarm <- renderUI({
      selectInput("np_multiarm", "Multi-Arm",
                  choices = c("TRUE" = TRUE,
                              "FALSE" = FALSE
                  ),
                  selected = FALSE
      )
    })
    output$npmcol <- renderUI({
      textInput("np_mcol", "Colour of the multiarm area",value="pink")
    })
    output$nppoint <- renderUI({
      textInput("np_point", "Colour of the points",value="purple")
    })
    output$npword <- renderUI({
      textInput("np_word", "Colour of the interior of words",value="black")
    })
    output$npbg <- renderUI({
      textInput("np_bg", "Colour of the edge of the words",value="white")
    })
  })
  
  #input1
  observeEvent(input$readynp, {
    data<-as.data.frame(values$sheetnp)
    data$lhr<-log(data$hr)
    data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
    
    a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
    
    sum_trt<-c(data$t1,data$t2)
    sum_trt_code<-unique(c(data$t1,data$t2))
    sum_trt_name<-unique(c(data$treat1,data$treat2))
    ntrt<-length(unique(sum_trt_code))
    df_trt<-as.data.frame(cbind(sum_trt_code,sum_trt_name))
    df_trt$sum_trt_code<-as.numeric(df_trt$sum_trt_code)
    df_trt<-arrange(df_trt,sum_trt_code)
    count_trt<-as.data.frame(table(sum_trt))
    lab <- c(df_trt$sum_trt_name)
    
    output$plot_np<-renderPlot({
      netgraph(a, labels=lab, offset=0.02, plastic=F, col=input$np_lcol, multiarm=input$np_multiarm, col.multiarm=input$np_mcol, points=T,
               col.points=input$np_point, number.of.studies = T, cex=2,
               cex.number.of.studies =1.5,col.number.of.studies = input$np_word,bg.number.of.studies = input$np_bg,
               cex.points=2*c(count_trt$Freq))
      title(input$np_title,adj=0.5,cex.main=2.5)
    })
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), "_network_plot.png", sep="")
      },
      content = function(file) {
        png(file,width = input$widthnp,
            height = input$heightnp)
        netgraph(a, labels=lab, offset=0.02, plastic=F, col=input$np_lcol, multiarm=input$np_multiarm, col.multiarm=input$np_mcol, points=T,
                 col.points=input$np_point, number.of.studies = T, cex=2,
                 cex.number.of.studies =1.5,col.number.of.studies = input$np_word,bg.number.of.studies = input$np_bg,
                 cex.points=2*c(count_trt$Freq))
        title(input$np_title,adj=0.5,cex.main=2.5)
        dev.off()
      }
    )
    shinyalert(title = "Complete!", type = "success")
  })
  #### end of Network Plot ####

  #####

  ##### FP #####
  observeEvent(input$nma1_fp_run,{
    follow_up<-input$nma1_fp_ex*12
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    values$nma1_fp_km<-km

    models <- list(
      "First order FP, p1=-2" = list(g1=function(x){x^-2},g2=function(x){0},f1=function(x){x^-2},f2=function(x){0}),
      "First order FP,p1=-1" = list(g1=function(x){x^-1},g2=function(x){0},f1=function(x){x^-1},f2=function(x){0}),
      "First order FP,p1=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){0},f1=function(x){x^-0.5},f2=function(x){0}),
      "First order FP,p1=0" = list(g1=function(x){log(x)},g2=function(x){0},f1=function(x){log(x)},f2=function(x){0}),
      "First order FP,p1=0.5" = list(g1=function(x){x^0.5},g2=function(x){0},f1=function(x){x^0.5},f2=function(x){0}),
      "First order FP,p1=1" = list(g1=function(x){x},g2=function(x){0},f1=function(x){x},f2=function(x){0}),
      "First order FP, p1=2" = list(g1=function(x){x^2},g2=function(x){0},f1=function(x){x^2},f2=function(x){0}),
      "First order FP,p1=3" = list(g1=function(x){x^3},g2=function(x){0},f1=function(x){x^3},f2=function(x){0}),
      "Second order FP,p1=-2, p2=-2" = list(g1=function(x){x^-2},g2=function(x){log(x)*x^-2},f1=function(x){x^-2},f2=function(x){log(x)*x^-2}),
      "Second order FP,p1=-2, p2=-1" = list(g1=function(x){x^-2},g2=function(x){x^-1},f1=function(x){x^-2},f2=function(x){x^-1}),
      "Second order FP,p1=-2, p2=-0.5" = list(g1=function(x){x^-2},g2=function(x){x^-0.5},f1=function(x){x^-2},f2=function(x){x^-0.5}),
      "Second order FP,p1=-2, p2=0" = list(g1=function(x){x^-2},g2=function(x){log(x)},f1=function(x){x^-2},f2=function(x){log(x)}),
      "Second order FP,p1=-2, p2=1" = list(g1=function(x){x^-2},g2=function(x){x^1},f1=function(x){x^-2},f2=function(x){x^1}),
      "Second order FP,p1=-2, p2=0.5" = list(g1=function(x){x^-2},g2=function(x){x^0.5},f1=function(x){x^-2},f2=function(x){x^0.5}),
      "Second order FP,p1=-2, p2=2" = list(g1=function(x){x^-2},g2=function(x){x^2},f1=function(x){x^-2},f2=function(x){x^2}),
      "Second order FP,p1=-2, p2=3" = list(g1=function(x){x^-2},g2=function(x){x^3},f1=function(x){x^-2},f2=function(x){x^3}),
      "Second order FP,p1=-1, p2=-1" = list(g1=function(x){x^-1},g2=function(x){log(x)*x^-1},f1=function(x){x^-1},f2=function(x){log(x)*x^-1}),
      "Second order FP,p1=-1, p2=-0.5" = list(g1=function(x){x^-1},g2=function(x){x^-0.5},f1=function(x){x^-1},f2=function(x){x^-0.5}),
      "Second order FP,p1=-1, p2=0" = list(g1=function(x){x^-1},g2=function(x){log(x)},f1=function(x){x^-1},f2=function(x){log(x)}),
      "Second order FP,p1=-1, p2=1" = list(g1=function(x){x^-1},g2=function(x){x^1},f1=function(x){x^-1},f2=function(x){x^1}),
      "Second order FP,p1=-1, p2=0.5" = list(g1=function(x){x^-1},g2=function(x){x^0.5},f1=function(x){x^-1},f2=function(x){x^0.5}),
      "Second order FP,p1=-1, p2=2" = list(g1=function(x){x^-1},g2=function(x){x^2},f1=function(x){x^-1},f2=function(x){x^2}),
      "Second order FP,p1=-1, p2=3" = list(g1=function(x){x^-1},g2=function(x){x^3},f1=function(x){x^-1},f2=function(x){x^3}),
      "Second order FP,p1=-0.5, p2=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){log(x)*x^-0.5},f1=function(x){x^-0.5},f2=function(x){log(x)*x^-0.5}),
      "Second order FP,p1=-0.5, p2=0" = list(g1=function(x){x^-0.5},g2=function(x){log(x)},f1=function(x){x^-0.5},f2=function(x){log(x)}),
      "Second order FP,p1=-0.5, p2=1" = list(g1=function(x){x^-0.5},g2=function(x){x^1},f1=function(x){x^-0.5},f2=function(x){x^1}),
      "Second order FP,p1=-0.5, p2=0.5" = list(g1=function(x){x^-0.5},g2=function(x){x^0.5},f1=function(x){x^-0.5},f2=function(x){x^0.5}),
      "Second order FP,p1=-0.5, p2=2" = list(g1=function(x){x^-0.5},g2=function(x){x^2},f1=function(x){x^-0.5},f2=function(x){x^2}),
      "Second order FP,p1=-0.5, p2=3" = list(g1=function(x){x^-0.5},g2=function(x){x^3},f1=function(x){x^-0.5},f2=function(x){x^3}),
      "Second order FP,p1=0, p2=0" = list(g1=function(x){log(x)},g2=function(x){log(x)*log(x)},f1=function(x){log(x)},f2=function(x){log(x)*log(x)}),
      "Second order FP,p1=0, p2=1" = list(g1=function(x){log(x)},g2=function(x){x^1},f1=function(x){log(x)},f2=function(x){x^1}),
      "Second order FP,p1=0, p2=0.5" = list(g1=function(x){log(x)},g2=function(x){x^0.5},f1=function(x){log(x)},f2=function(x){x^0.5}),
      "Second order FP,p1=0, p2=2" = list(g1=function(x){log(x)},g2=function(x){x^2},f1=function(x){log(x)},f2=function(x){x^2}),
      "Second order FP,p1=0, p2=3" = list(g1=function(x){log(x)},g2=function(x){x^3},f1=function(x){log(x)},f2=function(x){x^3}),
      "Second order FP,p1=0.5, p2=1" = list(g1=function(x){x^0.5},g2=function(x){x^1},f1=function(x){x^0.5},f2=function(x){x^1}),
      "Second order FP,p1=0.5, p2=0.5" = list(g1=function(x){x^0.5},g2=function(x){log(x)*x^0.5},f1=function(x){x^0.5},f2=function(x){log(x)*x^0.5}),
      "Second order FP,p1=0.5, p2=2" = list(g1=function(x){x^0.5},g2=function(x){x^2},f1=function(x){x^0.5},f2=function(x){x^2}),
      "Second order FP,p1=0.5, p2=3" = list(g1=function(x){x^0.5},g2=function(x){x^3},f1=function(x){x^0.5},f2=function(x){x^3}),
      "Second order FP,p1=1, p2=1" = list(g1=function(x){x^1},g2=function(x){log(x)*x^1},f1=function(x){x^1},f2=function(x){log(x)*x^1}),
      "Second order FP,p1=1, p2=2" = list(g1=function(x){x^1},g2=function(x){x^2},f1=function(x){x^1},f2=function(x){x^2}),
      "Second order FP,p1=1, p2=3" = list(g1=function(x){x^1},g2=function(x){x^3},f1=function(x){x^1},f2=function(x){x^3}),
      "Second order FP,p1=2, p2=2" = list(g1=function(x){x^2},g2=function(x){log(x)*x^2},f1=function(x){x^2},f2=function(x){log(x)*x^2}),
      "Second order FP,p1=2, p2=3" = list(g1=function(x){x^2},g2=function(x){x^3},f1=function(x){x^2},f2=function(x){x^3}),
      "Second order FP,p1=3, p2=3" = list(g1=function(x){x^3},g2=function(x){log(x)*x^3},f1=function(x){x^3},f2=function(x){log(x)*x^3})
    )
    fit.KM.NMA<-function(bf){
      km.new=values$nma1_fp_km
      km.new$g0=1
      km.new$f0=1
      km.new$g1=bf[[1]](km.new$time)
      km.new$g2=bf[[2]](km.new$time)
      km.new$f1=bf[[3]](km.new$time)
      km.new$f2=bf[[4]](km.new$time)
      #model formula
      f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
      glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
    }
    fits=lapply(models,fit.KM.NMA)
    aics=lapply(fits,AIC)
    fix_aic<-data.frame(AIC=round(unlist(aics),2))
    fix_aic<-arrange(fix_aic,AIC)
    fix_aic<-data.frame(model=row.names(fix_aic),aic=fix_aic$AIC)
    
    output$nma1_fp_fix_aic <- renderDT(datatable(fix_aic, editable = FALSE, rownames = FALSE, 
                                           options = list(pageLength = 5)
    ))
    
    output$gotofpstep21 <- renderUI({
      actionButton("gotofpstep21_button", label="Move on to FP Step Two (FP1)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
    })
    output$gotofpstep22 <- renderUI({
      actionButton("gotofpstep22_button", label="Move on to FP Step Two (FP2)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
    })
    
  })
  
  observeEvent(input$nma1_fp_run, {
    updateTabsetPanel(session, "subtabs_fp_main", selected ="1.2 FP Step One")
  })
  
  observeEvent(input$gotofpstep21_button, {
    updateTabsetPanel(session, "subtabs_fp_main", selected ="1.3 FP Step Two (FP1)")
  })
  
  observeEvent(input$gotofpstep22_button, {
    updateTabsetPanel(session, "subtabs_fp_main", selected ="1.4 FP Step Two (FP2)")
  })
  
  observeEvent(input$fp1_freq_pow,{
    if (input$fp1_freq_pow %in% c("-2","-1","-0.5","0","0.5","1","2","3")){
      enable("nma1_fp1_freq_run1")
      enable("nma1_fp1_freq_run1r")
    } else {
      shinyalert("Warning!", "The value should be one of the following: -2,-1,-0.5,0,0.5,1,2,3", type = "error")
      disable("nma1_fp1_freq_run1")
      disable("nma1_fp1_freq_run1r")
    }
  })
  
  observeEvent(input$nma1_fp1_freq_run1,{
    follow_up<-input$nma1_fp_ex
    FP1_pow<-input$fp1_freq_pow
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP1;","Power=",FP1_pow,";Frequentist;Fixed-effect")
    
    values$temp_fp1ff<-temp_model_name

    output$output_fp1ff<-renderUI({
      actionButton("goutput_fp1ff", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })

    output$text_fp1ff <- renderUI({
      HTML(paste("<u>","Current model is FP1 with power =",FP1_pow,"</u>"))
    })
    
    if(FP1_pow==-2){
      models <- list( "First order FP, p1=-2" = list(g1=function(x){x^-2},g2=function(x){0},f1=function(x){x^-2},f2=function(x){0}))
    }else if(FP1_pow==-1){
      models <- list( "First order FP,p1=-1" = list(g1=function(x){x^-1},g2=function(x){0},f1=function(x){x^-1},f2=function(x){0}))
    }else if(FP1_pow==-0.5){
      models <- list( "First order FP,p1=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){0},f1=function(x){x^-0.5},f2=function(x){0}))
    }else if(FP1_pow==0){
      models <- list( "First order FP,p1=0" = list(g1=function(x){log(x)},g2=function(x){0},f1=function(x){log(x)},f2=function(x){0}))
    }else if(FP1_pow==0.5){
      models <- list( "First order FP,p1=0.5" = list(g1=function(x){x^0.5},g2=function(x){0},f1=function(x){x^0.5},f2=function(x){0}))
    }else if(FP1_pow==1){
      models <- list( "First order FP,p1=1" = list(g1=function(x){x},g2=function(x){0},f1=function(x){x},f2=function(x){0}))
    }else if(FP1_pow==2){
      models <- list( "First order FP, p1=2" = list(g1=function(x){x^2},g2=function(x){0},f1=function(x){x^2},f2=function(x){0}))
    }else if(FP1_pow==3){
      models <- list( "First order FP,p1=3" = list(g1=function(x){x^3},g2=function(x){0},f1=function(x){x^3},f2=function(x){0}))
    }
    
    fit.KM.NMA<-function(bf){
      km.new=km
      km.new$g0=1
      km.new$f0=1
      km.new$g1=bf[[1]](km.new$time)
      km.new$g2=bf[[2]](km.new$time)
      km.new$f1=bf[[3]](km.new$time)
      km.new$f2=bf[[4]](km.new$time)
      #model formula
      f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
      glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
    }
    fits=lapply(models,fit.KM.NMA)
    pred.KM.NMA<-function(bf){
      trts=data.frame(trtf=unique(km$trtf))
      trts$studyf=sort(unique(km$studyf))[1] # select reference study as baseline
      timehorizon=data.frame(time=2*(1:(follow_up*6)))
      km.pred=merge.data.frame(timehorizon,trts)
      km.pred$g0=1
      km.pred$f0=1
      km.pred$g1=bf[[1]](km.pred$time)
      km.pred$g2=bf[[2]](km.pred$time)
      km.pred$f1=bf[[3]](km.pred$time)
      km.pred$f2=bf[[4]](km.pred$time)
      km.pred$timeDelta<-2
      km.pred
    }
    pred.KM.data=lapply(models,pred.KM.NMA)
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>% 
        filter(trtf == ref.trt)%>%
        mutate(pred.doce = pred)%>% 
        select(pred.doce,time) %>% 
        arrange(time) %>% 
        left_join(d) %>%
        mutate(lnhr = pred-pred.doce) %>% 
        mutate(hr=exp(lnhr))#%>%
      d1$modelc=names(models)[[i]]
      if(i==1)dpred<-d1
      if(i!=1)dpred<-rbind(dpred,d1)
    }
    dpred$Model<-factor(dpred$modelc,levels=names(models))
    
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>%
        dplyr::mutate(haz = exp(pred))%>%
        dplyr::group_by(trtf) %>%
        dplyr::arrange(time) %>%
        dplyr::mutate(cumhaz = cumsum(haz)) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      zero <- unique(d1[,c("trtf", "studyf")])
      zero$time=0
      zero$survProp=1
      d1=rbind(zero,d1)
      
      d1$modelc=names(models)[[i]]
      if(i==1)dpred1<-d1
      if(i!=1)dpred1<-rbind(dpred1,d1)
      
    }
    dpred1$Model<-factor(dpred1$modelc,levels=names(models))
      
    output$nma1_fp1ff_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dpred,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) + 
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") + 
        theme_bw() 
      f1
      if (input$y_axis_hr_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_fp, input$y_max_hr_fp))
      } else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp1ff_hr)
      print(f1)
      values$graph_fp1ff_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp1ff_plot_hr_dw, {
      output$nma1_fp1ff_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_fp1ff_hr", "Title", value = "HR(vs reference treatment)")
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1ff_plot_hr_dw, {
      output$nma1_fp1ff_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1ff_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1ff_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1ff_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1ff_hr, width = as.numeric(input$nma1_fp1ff_plot_hr_width),
               height = as.numeric(input$nma1_fp1ff_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_fp1ff_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=survProp, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_fp1ff_surv)
      print(f2)
      values$graph_fp1ff_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_fp1ff_plot_surv_dw, {
      output$nma1_fp1ff_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_fp1ff_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1ff_plot_surv_dw, {
      output$nma1_fp1ff_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1ff_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1ff_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1ff_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1ff_surv, width = as.numeric(input$nma1_fp1ff_plot_surv_width),
               height = as.numeric(input$nma1_fp1ff_plot_surv_height),
               units = "px")
      }
    )
    
    
    output$nma1_fp1ff_plot_haz<-renderPlot({
      f3= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=haz, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f3
      if (input$y_axis_haz_fp==TRUE){
        f3<-f3+scale_y_log10(limits = c(input$y_min_haz_fp, input$y_max_haz_fp))
      } else {f3<-f3+scale_y_log10()}
      f3<-f3 + ggtitle(input$ggtitle_fp1ff_haz)
      print(f3)
      values$graph_fp1ff_haz<-f3
    })
    
    ### download plot
    observeEvent(input$nma1_fp1ff_plot_haz_dw, {
      output$nma1_fp1ff_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_fp1ff_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1ff_plot_haz_dw, {
      output$nma1_fp1ff_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1ff_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1ff_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1ff_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1ff_haz, width = as.numeric(input$nma1_fp1ff_plot_haz_width),
               height = as.numeric(input$nma1_fp1ff_plot_haz_height),
               units = "px")
      }
    )

    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_fp1ff_coeff.data<-coeff.data
    
    output$nma1_fp1ff_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                    options = list(pageLength = 10)
    )) 
    
    ### calculate hr all
    i_trt<-nlevels(km$trtf)-1
    df_res12<-values$nma1_fp1ff_coeff.data
    
    if(FP1_pow == 0){
      hr_fun0=function(Time){0+0*Time^FP1_pow*log(Time)}
      hr_fun1=function(i,Time){df_res12$est[i]+df_res12$est[i_trt+i]*(Time^FP1_pow)*log(Time)}
    }else{
      hr_fun0=function(Time){0+0*Time^FP1_pow}
      hr_fun1=function(i,Time){df_res12$est[i]+df_res12$est[i_trt+i]*(Time^FP1_pow)}
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(km$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",levels(km$trtf))
    df_hr$time<-seq(1,follow_up*12,1)
    df_hr[,2]<-exp(hr_fun0(df_hr$time))
    for (i in 1:i_trt) {
      df_hr[,i+2]<-exp(hr_fun1(i,df_hr$time))
    }
    values$nma1_fp1ff_hrdata<-df_hr
    ### end: calculate hr all
    
    output$nma1_fp1ff_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_fp1ff_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_fp1ff_hrdata, file,row.names = TRUE)
      }
    )
    

    shinyalert(title = "Complete!", type = "success")
      
  }) 
  
  observeEvent(input$nma1_fp1_freq_run1r,{
    
    follow_up<-input$nma1_fp_ex
    FP1_pow<-input$fp1_freq_pow
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP1;","Power=",FP1_pow,";Frequentist;Random-effects")
    
    values$temp_fp1fr<-temp_model_name
    output$output_fp1fr<-renderUI({
      actionButton("goutput_fp1fr", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp1fr <- renderUI({
      HTML(paste("<u>","Current model is FP1 with power =",FP1_pow,"</u>"))
    })
    
    if(FP1_pow==-2){
      models <- list( "First order FP, p1=-2" = list(g1=function(x){x^-2},g2=function(x){0},f1=function(x){x^-2},f2=function(x){0}))
    }else if(FP1_pow==-1){
      models <- list( "First order FP,p1=-1" = list(g1=function(x){x^-1},g2=function(x){0},f1=function(x){x^-1},f2=function(x){0}))
    }else if(FP1_pow==-0.5){
      models <- list( "First order FP,p1=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){0},f1=function(x){x^-0.5},f2=function(x){0}))
    }else if(FP1_pow==0){
      models <- list( "First order FP,p1=0" = list(g1=function(x){log(x)},g2=function(x){0},f1=function(x){log(x)},f2=function(x){0}))
    }else if(FP1_pow==0.5){
      models <- list( "First order FP,p1=0.5" = list(g1=function(x){x^0.5},g2=function(x){0},f1=function(x){x^0.5},f2=function(x){0}))
    }else if(FP1_pow==1){
      models <- list( "First order FP,p1=1" = list(g1=function(x){x},g2=function(x){0},f1=function(x){x},f2=function(x){0}))
    }else if(FP1_pow==2){
      models <- list( "First order FP, p1=2" = list(g1=function(x){x^2},g2=function(x){0},f1=function(x){x^2},f2=function(x){0}))
    }else if(FP1_pow==3){
      models <- list( "First order FP,p1=3" = list(g1=function(x){x^3},g2=function(x){0},f1=function(x){x^3},f2=function(x){0}))
    }
    
    fit.KM.NMA.REint <- function(bf){
      km.new = km
      km.new$g0 = 1
      km.new$g1 = bf[[1]](km.new$time)
      km.new$g2 = bf[[2]](km.new$time)
      #model formula
      f = cbind(nevents, natrisk - nevents) ~ 1 + (-1 + g0 | interaction(trtf, studyf)) + trtf * g1 + studyf * g1 + trtf * g2 + studyf * g2 
      glmer(f, family = binomial(link = "cloglog"), data = km.new, offset = log(timeDelta))
    }
    fits <- lapply(models, fit.KM.NMA.REint)
    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_fp1fr_coeff.data<-coeff.data
    output$nma1_fp1fr_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                    options = list(pageLength = 10)
    )) 
    shinyalert(title = "Complete!", type = "success")
  }) 
  
  observeEvent(input$nma1_fp1_freq_run1, {
    updateTabsetPanel(session, "fp1_freq_sub", selected ="fppage32")
  })
  
  observeEvent(input$nma1_fp1_freq_run1r, {
    updateTabsetPanel(session, "fp1_freq_sub", selected ="fppage33")
  })
  
  output$copy_nma1_fp1ff_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp1ff_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1ff_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp1fr_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp1fr_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1fr_coeff.data, file,row.names = TRUE)
    }
  )
  
  observeEvent(input$nma1_fp1_bay_run1,{
    follow_up<-input$nma1_fp_ex
    FP1_pow<-input$fp1_bay_pow
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP1;","Power=",FP1_pow,";Bayesian;Fixed-effect")
    
    values$temp_fp1bf<-temp_model_name
    output$output_fp1bf<-renderUI({
      actionButton("goutput_fp1bf", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp1bf <- renderUI({
      HTML(paste("<u>","Current model is FP1 with power =",FP1_pow,"</u>"))
    })
    
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      Na = d_std$n_arms,
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      a = dat$arm,
      t = as.matrix(select(ungroup(d_trts), -studyn)),
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )

    model.pars <- list(P1 = FP1_pow)
    #Fit the second order fractional polynomial model
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))

    fit.fp <- jags(model.file = "www/jags_fp1.txt", 
                   data = c(dat_jg,
                            list(prior.mean = rep(input$fp1_pmean, 2)),
                            list(prior.prec = diag(rep(input$fp1_pprec, 2))),
                            model.pars),
                   parameters = c("d", "mu", "Beta"),
                   n.chains = input$fp1_nchains, n.iter = input$fp1_niter, n.burnin = input$fp1_nburnin, n.thin = input$fp1_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.fp$BUGSoutput$pV,3)
    DIC<-round(fit.fp$BUGSoutput$DIC,3)
    
    output$nma1_fp1bf_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.fp
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6, 1, 5, 4)]
    values$nma1_fp1bf_coeff.data<-res
    
    res_mu0 <- fit.fp
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_fp1bf_coeff.data_mu<-res_mu0
    
    
    output$nma1_fp1bf_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                      options = list(pageLength = 10)
    ))
    
    output$nma1_fp1bf_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE, 
                                                         options = list(pageLength = 10)
    ))
    
    res_mu <- fit.fp
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)-1
    
    df_res1<-res
    df_res1$median[2:(i_trt+1)]<-df_res1$median[2:(i_trt+1)]+mu1
    df_res1$median[(i_trt+3):(2*(i_trt+1))]<-df_res1$median[(i_trt+3):(2*(i_trt+1))]+mu2
    
    df_res12<-res
    
    if(FP1_pow == 0){
      hr_fun0=function(Time){0+0*Time^FP1_pow*log(Time)}
      hr_fun1=function(i,Time){df_res12$median[i+1]+df_res12$median[i_trt+i+2]*(Time^FP1_pow)*log(Time)}
    }else{
      hr_fun0=function(Time){0+0*Time^FP1_pow}
      hr_fun1=function(i,Time){df_res12$median[i+1]+df_res12$median[i_trt+i+2]*(Time^FP1_pow)}
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    df_hr[,2]<-exp(hr_fun0(df_hr$time))
    for (i in 1:i_trt) {
      df_hr[,i+2]<-exp(hr_fun1(i,df_hr$time))
    }
    values$nma1_fp1bf_hrdata<-df_hr
    
    output$nma1_fp1bf_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_fp1bf_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_fp1bf_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    if(FP1_pow == 0){
      haz_fun0=function(Time){mu1+mu2*Time^FP1_pow*log(Time)}
      haz_fun1=function(i,Time){df_res1$median[i+1]+df_res1$median[i_trt+i+2]*(Time^FP1_pow)*log(Time)}
    }else{
      haz_fun0=function(Time){mu1+mu2*Time^FP1_pow}
      haz_fun1=function(i,Time){df_res1$median[i+1]+df_res1$median[i_trt+i+2]*(Time^FP1_pow)}
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    df_haz[,2]<-exp(haz_fun0(df_haz$time))
    for (i in 1:i_trt) {
      df_haz[,i+2]<-exp(haz_fun1(i,df_haz$time))
    }
    
    df_surv<-df_haz
    for ( i in 1:(i_trt+1) ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_fp1bf_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_fp, input$y_max_hr_fp))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp1bf_hr)
      print(f1)
      values$graph_fp1bf_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp1bf_plot_hr_dw, {
      output$nma1_fp1bf_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_fp1bf_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1bf_plot_surv_dw, {
      output$nma1_fp1bf_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1bf_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1bf_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1bf_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1bf_hr, width = as.numeric(input$nma1_fp1bf_plot_hr_width),
               height = as.numeric(input$nma1_fp1bf_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_fp1bf_plot_haz<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_fp, input$y_max_haz_fp))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp1bf_haz)
      print(f1)
      values$graph_fp1bf_haz<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp1bf_plot_haz_dw, {
      output$nma1_fp1bf_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_fp1bf_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1bf_plot_surv_dw, {
      output$nma1_fp1bf_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1bf_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1bf_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1bf_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1bf_haz, width = as.numeric(input$nma1_fp1bf_plot_haz_width),
               height = as.numeric(input$nma1_fp1bf_plot_haz_height),
               units = "px")
      }
    )
    
    output$nma1_fp1bf_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_fp1bf_surv)
      print(f2)
      values$graph_fp1bf_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_fp1bf_plot_surv_dw, {
      output$nma1_fp1bf_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_fp1bf_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp1bf_plot_surv_dw, {
      output$nma1_fp1bf_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp1bf_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp1bf_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp1bf_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp1bf_surv, width = as.numeric(input$nma1_fp1bf_plot_surv_width),
               height = as.numeric(input$nma1_fp1bf_plot_surv_height),
               units = "px")
      }
    )
    rm(fit.fp)
  })
  
  observeEvent(input$nma1_fp1_bay_run1r,{
    follow_up<-input$nma1_fp_ex
    FP1_pow<-input$fp1_bay_pow
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP1;","Power=",FP1_pow,";Bayesian;Random-effects")
    
    values$temp_fp1br<-temp_model_name
    output$output_fp1br<-renderUI({
      actionButton("goutput_fp1br", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp1br <- renderUI({
      HTML(paste("<u>","Current model is FP1 with power =",FP1_pow,"</u>"))
    })
    
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    
    
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    model.pars <- list(P1 = FP1_pow)
    #Fit the second order fractional polynomial model
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))

    fit.fp <- jags(model.file = "www/jags_fp1_bay_d0.txt",
                   data = c(dat_jg,
                            list(prior.mean = rep(input$fp1_pmean, 2)),
                            list(prior.prec = diag(rep(input$fp1_pprec, 2))),
                            model.pars),
                   parameters = c("d", "mu", "Beta","sd"),
                   n.chains = input$fp1_nchains, n.iter = input$fp1_niter, n.burnin = input$fp1_nburnin, n.thin = input$fp1_nthin)
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.fp$BUGSoutput$pV,3)
    DIC<-round(fit.fp$BUGSoutput$DIC,3)
    
    output$nma1_fp1br_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.fp
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6, 1, 5, 4)]
    values$nma1_fp1br_coeff.data<-res
    
    res_mu0 <- fit.fp
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_fp1br_coeff.data_mu<-res_mu0
    
    
    output$nma1_fp1br_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                      options = list(pageLength = 10)
    ))
    
    output$nma1_fp1br_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE, 
                                                         options = list(pageLength = 10)
    ))
    
    shinyalert(title = "Complete!", type = "success")
    rm(fit.fp)
  })
  
  observeEvent(input$nma1_fp1_bay_run1, {
    updateTabsetPanel(session, "fp1_bay_sub", selected ="fppage35")
  })
  
  observeEvent(input$nma1_fp1_bay_run1r, {
    updateTabsetPanel(session, "fp1_bay_sub", selected ="fppage36")
  })
  
  output$copy_nma1_fp1bf_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp1bf_coeff.data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1bf_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp1bf_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_fp1bf_coeff.data_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1bf_coeff.data_mu, file,row.names = TRUE)
    }
  ) 
  
  output$copy_nma1_fp1br_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp1br_coeff.data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1br_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp1br_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_fp1br_coeff.data_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp1br_coeff.data_mu, file,row.names = TRUE)
    }
  ) 
  
  observeEvent(input$nma1_fp2_freq_run2,{
    follow_up<-input$nma1_fp_ex
    FP2_pow1<-input$fp2_freq_pow1
    FP2_pow2<-input$fp2_freq_pow2
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP2;","Power1=",FP2_pow1,";Power2=",FP2_pow2,";Frequentist;Fixed-effect")
    
    values$temp_fp2ff<-temp_model_name
    output$output_fp2ff<-renderUI({
      actionButton("goutput_fp2ff", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp2ff <- renderUI({
      HTML(paste("<u>","Current model is FP2 with power =",FP2_pow1,"and",FP2_pow2,"</u>"))
    })
    
    if(FP2_pow1==-2 & FP2_pow2==-2){
      models <- list( "Second order FP,p1=-2, p2=-2" = list(g1=function(x){x^-2},g2=function(x){log(x)*x^-2},f1=function(x){x^-2},f2=function(x){log(x)*x^-2}))}
    else if(FP2_pow1==-2 & FP2_pow2==-1){
      models <- list("Second order FP,p1=-2, p2=-1" = list(g1=function(x){x^-2},g2=function(x){x^-1},f1=function(x){x^-2},f2=function(x){x^-1}))}
    else if(FP2_pow1==-2 & FP2_pow2==-0.5){
      models <- list("Second order FP,p1=-2, p2=-0.5" = list(g1=function(x){x^-2},g2=function(x){x^-0.5},f1=function(x){x^-2},f2=function(x){x^-0.5}))}
    else if(FP2_pow1==-2 & FP2_pow2==0){
      models <- list("Second order FP,p1=-2, p2=0" = list(g1=function(x){x^-2},g2=function(x){log(x)},f1=function(x){x^-2},f2=function(x){log(x)}))}
    else if(FP2_pow1==-2 & FP2_pow2==1){
      models <- list("Second order FP,p1=-2, p2=1" = list(g1=function(x){x^-2},g2=function(x){x^1},f1=function(x){x^-2},f2=function(x){x^1}))}
    else if(FP2_pow1==-2 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-2, p2=0.5" = list(g1=function(x){x^-2},g2=function(x){x^0.5},f1=function(x){x^-2},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-2 & FP2_pow2==2){
      models <- list("Second order FP,p1=-2, p2=2" = list(g1=function(x){x^-2},g2=function(x){x^2},f1=function(x){x^-2},f2=function(x){x^2}))}
    else if(FP2_pow1==-2 & FP2_pow2==3){
      models <- list("Second order FP,p1=-2, p2=3" = list(g1=function(x){x^-2},g2=function(x){x^3},f1=function(x){x^-2},f2=function(x){x^3}))}
    else if(FP2_pow1==-1 & FP2_pow2==-1){
      models <- list("Second order FP,p1=-1, p2=-1" = list(g1=function(x){x^-1},g2=function(x){log(x)*x^-1},f1=function(x){x^-1},f2=function(x){log(x)*x^-1}))}
    else if(FP2_pow1==-1 & FP2_pow2==-0.5){
      models <- list("Second order FP,p1=-1, p2=-0.5" = list(g1=function(x){x^-1},g2=function(x){x^-0.5},f1=function(x){x^-1},f2=function(x){x^-0.5}))}
    else if(FP2_pow1==-1 & FP2_pow2==0){
      models <- list("Second order FP,p1=-1, p2=0" = list(g1=function(x){x^-1},g2=function(x){log(x)},f1=function(x){x^-1},f2=function(x){log(x)}))}
    else if(FP2_pow1==-1 & FP2_pow2==1){
      models <- list("Second order FP,p1=-1, p2=1" = list(g1=function(x){x^-1},g2=function(x){x^1},f1=function(x){x^-1},f2=function(x){x^1}))}
    else if(FP2_pow1==-1 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-1, p2=0.5" = list(g1=function(x){x^-1},g2=function(x){x^0.5},f1=function(x){x^-1},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-1 & FP2_pow2==2){
      models <- list("Second order FP,p1=-1, p2=2" = list(g1=function(x){x^-1},g2=function(x){x^2},f1=function(x){x^-1},f2=function(x){x^2}))}
    else if(FP2_pow1==-1 & FP2_pow2==3){
      models <- list("Second order FP,p1=-1, p2=3" = list(g1=function(x){x^-1},g2=function(x){x^3},f1=function(x){x^-1},f2=function(x){x^3}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==-0.5){
      models <- list( "Second order FP,p1=-0.5, p2=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){log(x)*x^-0.5},f1=function(x){x^-0.5},f2=function(x){log(x)*x^-0.5}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==0){
      models <- list("Second order FP,p1=-0.5, p2=0" = list(g1=function(x){x^-0.5},g2=function(x){log(x)},f1=function(x){x^-0.5},f2=function(x){log(x)}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==1){
      models <- list("Second order FP,p1=-0.5, p2=1" = list(g1=function(x){x^-0.5},g2=function(x){x^1},f1=function(x){x^-0.5},f2=function(x){x^1}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-0.5, p2=0.5" = list(g1=function(x){x^-0.5},g2=function(x){x^0.5},f1=function(x){x^-0.5},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==2){
      models <- list("Second order FP,p1=-0.5, p2=2" = list(g1=function(x){x^-0.5},g2=function(x){x^2},f1=function(x){x^-0.5},f2=function(x){x^2}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==3){
      models <- list("Second order FP,p1=-0.5, p2=3" = list(g1=function(x){x^-0.5},g2=function(x){x^3},f1=function(x){x^-0.5},f2=function(x){x^3}))}
    else if(FP2_pow1==0 & FP2_pow2==0){
      models <- list("Second order FP,p1=0, p2=0" = list(g1=function(x){log(x)},g2=function(x){log(x)*log(x)},f1=function(x){log(x)},f2=function(x){log(x)*log(x)}))}
    else if(FP2_pow1==0 & FP2_pow2==1){
      models <- list("Second order FP,p1=0, p2=1" = list(g1=function(x){log(x)},g2=function(x){x^1},f1=function(x){log(x)},f2=function(x){x^1}))}
    else if(FP2_pow1==0 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=0, p2=0.5" = list(g1=function(x){log(x)},g2=function(x){x^0.5},f1=function(x){log(x)},f2=function(x){x^0.5}))}
    else if(FP2_pow1==0 & FP2_pow2==2){
      models <- list("Second order FP,p1=0, p2=2" = list(g1=function(x){log(x)},g2=function(x){x^2},f1=function(x){log(x)},f2=function(x){x^2}))}
    else if(FP2_pow1==0 & FP2_pow2==3){
      models <- list("Second order FP,p1=0, p2=3" = list(g1=function(x){log(x)},g2=function(x){x^3},f1=function(x){log(x)},f2=function(x){x^3}))}
    else if(FP2_pow1==0.5 & FP2_pow2==1){
      models <- list("Second order FP,p1=0.5, p2=1" = list(g1=function(x){x^0.5},g2=function(x){x^1},f1=function(x){x^0.5},f2=function(x){x^1}))}
    else if(FP2_pow1==0.5 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=0.5, p2=0.5" = list(g1=function(x){x^0.5},g2=function(x){log(x)*x^0.5},f1=function(x){x^0.5},f2=function(x){log(x)*x^0.5}))}
    else if(FP2_pow1==0.5 & FP2_pow2==2){
      models <- list("Second order FP,p1=0.5, p2=2" = list(g1=function(x){x^0.5},g2=function(x){x^2},f1=function(x){x^0.5},f2=function(x){x^2}))}
    else if(FP2_pow1==0.5 & FP2_pow2==3){
      models <- list("Second order FP,p1=0.5, p2=3" = list(g1=function(x){x^0.5},g2=function(x){x^3},f1=function(x){x^0.5},f2=function(x){x^3}))}
    else if(FP2_pow1==1 & FP2_pow2==1){
      models <- list("Second order FP,p1=1, p2=1" = list(g1=function(x){x^1},g2=function(x){log(x)*x^1},f1=function(x){x^1},f2=function(x){log(x)*x^1}))}
    else if(FP2_pow1==1 & FP2_pow2==2){
      models <- list("Second order FP,p1=1, p2=2" = list(g1=function(x){x^1},g2=function(x){x^2},f1=function(x){x^1},f2=function(x){x^2}))}
    else if(FP2_pow1==1 & FP2_pow2==3){
      models <- list("Second order FP,p1=1, p2=3" = list(g1=function(x){x^1},g2=function(x){x^3},f1=function(x){x^1},f2=function(x){x^3}))}
    else if(FP2_pow1==2 & FP2_pow2==2){
      models <- list("Second order FP,p1=2, p2=2" = list(g1=function(x){x^2},g2=function(x){log(x)*x^2},f1=function(x){x^2},f2=function(x){log(x)*x^2}))}
    else if(FP2_pow1==2 & FP2_pow2==3){
      models <- list("Second order FP,p1=2, p2=3" = list(g1=function(x){x^2},g2=function(x){x^3},f1=function(x){x^2},f2=function(x){x^3}))}
    else if(FP2_pow1==3 & FP2_pow2==3){
      models <- list("Second order FP,p1=3, p2=3" = list(g1=function(x){x^3},g2=function(x){log(x)*x^3},f1=function(x){x^3},f2=function(x){log(x)*x^3}))}
    
    fit.KM.NMA<-function(bf){
      km.new=km
      km.new$g0=1
      km.new$f0=1
      km.new$g1=bf[[1]](km.new$time)
      km.new$g2=bf[[2]](km.new$time)
      km.new$f1=bf[[3]](km.new$time)
      km.new$f2=bf[[4]](km.new$time)
      #model formula
      f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
      glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
    }
    fits=lapply(models,fit.KM.NMA)
    pred.KM.NMA<-function(bf){
      trts=data.frame(trtf=unique(km$trtf))
      trts$studyf=sort(unique(km$studyf))[1] # select reference study as baseline
      timehorizon=data.frame(time=2*(1:(follow_up*6)))
      km.pred=merge.data.frame(timehorizon,trts)
      km.pred$g0=1
      km.pred$f0=1
      km.pred$g1=bf[[1]](km.pred$time)
      km.pred$g2=bf[[2]](km.pred$time)
      km.pred$f1=bf[[3]](km.pred$time)
      km.pred$f2=bf[[4]](km.pred$time)
      km.pred$timeDelta<-2
      km.pred
    }
    pred.KM.data=lapply(models,pred.KM.NMA)
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>% 
        filter(trtf == ref.trt)%>%
        mutate(pred.doce = pred)%>% 
        select(pred.doce,time) %>% 
        arrange(time) %>% 
        left_join(d) %>%
        mutate(lnhr = pred-pred.doce) %>% 
        mutate(hr=exp(lnhr))#%>%
      d1$modelc=names(models)[[i]]
      if(i==1)dpred<-d1
      if(i!=1)dpred<-rbind(dpred,d1)
    }
    dpred$Model<-factor(dpred$modelc,levels=names(models))
    
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>%
        dplyr::mutate(haz = exp(pred))%>%
        dplyr::group_by(trtf) %>%
        dplyr::arrange(time) %>%
        dplyr::mutate(cumhaz = cumsum(haz)) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      zero <- unique(d1[,c("trtf", "studyf")])
      zero$time=0
      zero$survProp=1
      d1=rbind(zero,d1)
      
      d1$modelc=names(models)[[i]]
      if(i==1)dpred1<-d1
      if(i!=1)dpred1<-rbind(dpred1,d1)
      
    }
    dpred1$Model<-factor(dpred1$modelc,levels=names(models))
    
    output$nma1_fp2ff_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dpred,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) + 
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") + 
        theme_bw() 
      f1
      if (input$y_axis_hr_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_fp, input$y_max_hr_fp))
      } else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp2ff_hr)
      print(f1)
      values$graph_fp2ff_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp2ff_plot_hr_dw, {
      output$nma1_fp2ff_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_fp2ff_hr", "Title", value = "HR(vs reference treatment)")
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2ff_plot_hr_dw, {
      output$nma1_fp2ff_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2ff_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2ff_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2ff_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2ff_hr, width = as.numeric(input$nma1_fp2ff_plot_hr_width),
               height = as.numeric(input$nma1_fp2ff_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_fp2ff_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=survProp, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_fp2ff_surv)
      print(f2)
      values$graph_fp2ff_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_fp2ff_plot_surv_dw, {
      output$nma1_fp2ff_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_fp2ff_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2ff_plot_surv_dw, {
      output$nma1_fp2ff_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2ff_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2ff_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2ff_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2ff_surv, width = as.numeric(input$nma1_fp2ff_plot_surv_width),
               height = as.numeric(input$nma1_fp2ff_plot_surv_height),
               units = "px")
      }
    )
    
    
    output$nma1_fp2ff_plot_haz<-renderPlot({
      f3= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=haz, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f3
      if (input$y_axis_haz_fp==TRUE){
        f3<-f3+scale_y_log10(limits = c(input$y_min_haz_fp, input$y_max_haz_fp))
      } else {f3<-f3+scale_y_log10()}
      f3<-f3 + ggtitle(input$ggtitle_fp2ff_haz)
      print(f3)
      values$graph_fp2ff_haz<-f3
    })
    
    ### download plot
    observeEvent(input$nma1_fp2ff_plot_haz_dw, {
      output$nma1_fp2ff_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_fp2ff_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2ff_plot_haz_dw, {
      output$nma1_fp2ff_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2ff_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2ff_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2ff_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2ff_haz, width = as.numeric(input$nma1_fp2ff_plot_haz_width),
               height = as.numeric(input$nma1_fp2ff_plot_haz_height),
               units = "px")
      }
    )
    
    
    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_fp2ff_coeff.data<-coeff.data
    
    
    output$nma1_fp2ff_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                    options = list(pageLength = 10)
    ))
    
    ### calculate hr all
    i_trt<-nlevels(km$trtf)-1
    df_res22<-coeff.data
    
    if(FP2_pow1 == FP2_pow2 & FP2_pow2 != 0){
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*FP2_pow2*log(Time)}
      hr_fun1=function(i,Time){df_res22$est[i]+df_res22$est[i_trt+i]*(Time^FP2_pow1) + df_res22$est[i_trt*2+i]*(Time^FP2_pow2)*log(Time)}
    }else if(FP2_pow1==0 & FP2_pow2== 0){
      hr_fun0=function(Time){0+0*Time^0*log(Time) + 0*Time^0*log(Time)*log(Time)}
      hr_fun1=function(i,Time){df_res22$est[i]+df_res22$est[i_trt+i]*(Time^0)*log(Time) + df_res22$est[i_trt*2+i]*(Time^0)*log(Time)*log(Time)}
    }else if(FP2_pow2==0 & FP2_pow1 != 0){
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*Time^0*log(Time)}
      hr_fun1=function(i,Time){df_res22$est[i]+df_res22$est[i_trt+i]*(Time^FP2_pow1) + df_res22$est[i_trt*2+i]*(Time^0)*log(Time)}
    }else{
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*Time^FP2_pow2}
      hr_fun1=function(i,Time){df_res22$est[i]+df_res22$est[i_trt+i]*(Time^FP2_pow1) + df_res22$est[i_trt*2+i]*(Time^FP2_pow2)}
    }

    df_hr<-as.data.frame(matrix(ncol = (nlevels(km$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",levels(km$trtf))
    df_hr$time<-seq(1,follow_up*12,1)
    df_hr[,2]<-exp(hr_fun0(df_hr$time))
    for (i in 1:i_trt) {
      df_hr[,i+2]<-exp(hr_fun1(i,df_hr$time))
    }
    values$nma1_fp2ff_hrdata<-df_hr
    ### end: calculate hr all
    
    output$nma1_fp2ff_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_fp2ff_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_fp2ff_hrdata, file,row.names = TRUE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")

  })
  
  observeEvent(input$nma1_fp2_freq_run2r,{
    follow_up<-input$nma1_fp_ex
    FP2_pow1<-input$fp2_freq_pow1
    FP2_pow2<-input$fp2_freq_pow2
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    
    temp_model_name<-paste0("FP2;","Power1=",FP2_pow1,";Power2=",FP2_pow2,";Frequentist;Random-effects")
    
    values$temp_fp2fr<-temp_model_name
    output$output_fp2fr<-renderUI({
      actionButton("goutput_fp2fr", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp2fr <- renderUI({
      HTML(paste("<u>","Current model is FP2 with power =",FP2_pow1,"and",FP2_pow2,"</u>"))
    })
    
    if(FP2_pow1==-2 & FP2_pow2==-2){
      models <- list( "Second order FP,p1=-2, p2=-2" = list(g1=function(x){x^-2},g2=function(x){log(x)*x^-2},f1=function(x){x^-2},f2=function(x){log(x)*x^-2}))}
    else if(FP2_pow1==-2 & FP2_pow2==-1){
      models <- list("Second order FP,p1=-2, p2=-1" = list(g1=function(x){x^-2},g2=function(x){x^-1},f1=function(x){x^-2},f2=function(x){x^-1}))}
    else if(FP2_pow1==-2 & FP2_pow2==-0.5){
      models <- list("Second order FP,p1=-2, p2=-0.5" = list(g1=function(x){x^-2},g2=function(x){x^-0.5},f1=function(x){x^-2},f2=function(x){x^-0.5}))}
    else if(FP2_pow1==-2 & FP2_pow2==0){
      models <- list("Second order FP,p1=-2, p2=0" = list(g1=function(x){x^-2},g2=function(x){log(x)},f1=function(x){x^-2},f2=function(x){log(x)}))}
    else if(FP2_pow1==-2 & FP2_pow2==1){
      models <- list("Second order FP,p1=-2, p2=1" = list(g1=function(x){x^-2},g2=function(x){x^1},f1=function(x){x^-2},f2=function(x){x^1}))}
    else if(FP2_pow1==-2 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-2, p2=0.5" = list(g1=function(x){x^-2},g2=function(x){x^0.5},f1=function(x){x^-2},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-2 & FP2_pow2==2){
      models <- list("Second order FP,p1=-2, p2=2" = list(g1=function(x){x^-2},g2=function(x){x^2},f1=function(x){x^-2},f2=function(x){x^2}))}
    else if(FP2_pow1==-2 & FP2_pow2==3){
      models <- list("Second order FP,p1=-2, p2=3" = list(g1=function(x){x^-2},g2=function(x){x^3},f1=function(x){x^-2},f2=function(x){x^3}))}
    else if(FP2_pow1==-1 & FP2_pow2==-1){
      models <- list("Second order FP,p1=-1, p2=-1" = list(g1=function(x){x^-1},g2=function(x){log(x)*x^-1},f1=function(x){x^-1},f2=function(x){log(x)*x^-1}))}
    else if(FP2_pow1==-1 & FP2_pow2==-0.5){
      models <- list("Second order FP,p1=-1, p2=-0.5" = list(g1=function(x){x^-1},g2=function(x){x^-0.5},f1=function(x){x^-1},f2=function(x){x^-0.5}))}
    else if(FP2_pow1==-1 & FP2_pow2==0){
      models <- list("Second order FP,p1=-1, p2=0" = list(g1=function(x){x^-1},g2=function(x){log(x)},f1=function(x){x^-1},f2=function(x){log(x)}))}
    else if(FP2_pow1==-1 & FP2_pow2==1){
      models <- list("Second order FP,p1=-1, p2=1" = list(g1=function(x){x^-1},g2=function(x){x^1},f1=function(x){x^-1},f2=function(x){x^1}))}
    else if(FP2_pow1==-1 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-1, p2=0.5" = list(g1=function(x){x^-1},g2=function(x){x^0.5},f1=function(x){x^-1},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-1 & FP2_pow2==2){
      models <- list("Second order FP,p1=-1, p2=2" = list(g1=function(x){x^-1},g2=function(x){x^2},f1=function(x){x^-1},f2=function(x){x^2}))}
    else if(FP2_pow1==-1 & FP2_pow2==3){
      models <- list("Second order FP,p1=-1, p2=3" = list(g1=function(x){x^-1},g2=function(x){x^3},f1=function(x){x^-1},f2=function(x){x^3}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==-0.5){
      models <- list( "Second order FP,p1=-0.5, p2=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){log(x)*x^-0.5},f1=function(x){x^-0.5},f2=function(x){log(x)*x^-0.5}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==0){
      models <- list("Second order FP,p1=-0.5, p2=0" = list(g1=function(x){x^-0.5},g2=function(x){log(x)},f1=function(x){x^-0.5},f2=function(x){log(x)}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==1){
      models <- list("Second order FP,p1=-0.5, p2=1" = list(g1=function(x){x^-0.5},g2=function(x){x^1},f1=function(x){x^-0.5},f2=function(x){x^1}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=-0.5, p2=0.5" = list(g1=function(x){x^-0.5},g2=function(x){x^0.5},f1=function(x){x^-0.5},f2=function(x){x^0.5}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==2){
      models <- list("Second order FP,p1=-0.5, p2=2" = list(g1=function(x){x^-0.5},g2=function(x){x^2},f1=function(x){x^-0.5},f2=function(x){x^2}))}
    else if(FP2_pow1==-0.5 & FP2_pow2==3){
      models <- list("Second order FP,p1=-0.5, p2=3" = list(g1=function(x){x^-0.5},g2=function(x){x^3},f1=function(x){x^-0.5},f2=function(x){x^3}))}
    else if(FP2_pow1==0 & FP2_pow2==0){
      models <- list("Second order FP,p1=0, p2=0" = list(g1=function(x){log(x)},g2=function(x){log(x)*log(x)},f1=function(x){log(x)},f2=function(x){log(x)*log(x)}))}
    else if(FP2_pow1==0 & FP2_pow2==1){
      models <- list("Second order FP,p1=0, p2=1" = list(g1=function(x){log(x)},g2=function(x){x^1},f1=function(x){log(x)},f2=function(x){x^1}))}
    else if(FP2_pow1==0 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=0, p2=0.5" = list(g1=function(x){log(x)},g2=function(x){x^0.5},f1=function(x){log(x)},f2=function(x){x^0.5}))}
    else if(FP2_pow1==0 & FP2_pow2==2){
      models <- list("Second order FP,p1=0, p2=2" = list(g1=function(x){log(x)},g2=function(x){x^2},f1=function(x){log(x)},f2=function(x){x^2}))}
    else if(FP2_pow1==0 & FP2_pow2==3){
      models <- list("Second order FP,p1=0, p2=3" = list(g1=function(x){log(x)},g2=function(x){x^3},f1=function(x){log(x)},f2=function(x){x^3}))}
    else if(FP2_pow1==0.5 & FP2_pow2==1){
      models <- list("Second order FP,p1=0.5, p2=1" = list(g1=function(x){x^0.5},g2=function(x){x^1},f1=function(x){x^0.5},f2=function(x){x^1}))}
    else if(FP2_pow1==0.5 & FP2_pow2==0.5){
      models <- list("Second order FP,p1=0.5, p2=0.5" = list(g1=function(x){x^0.5},g2=function(x){log(x)*x^0.5},f1=function(x){x^0.5},f2=function(x){log(x)*x^0.5}))}
    else if(FP2_pow1==0.5 & FP2_pow2==2){
      models <- list("Second order FP,p1=0.5, p2=2" = list(g1=function(x){x^0.5},g2=function(x){x^2},f1=function(x){x^0.5},f2=function(x){x^2}))}
    else if(FP2_pow1==0.5 & FP2_pow2==3){
      models <- list("Second order FP,p1=0.5, p2=3" = list(g1=function(x){x^0.5},g2=function(x){x^3},f1=function(x){x^0.5},f2=function(x){x^3}))}
    else if(FP2_pow1==1 & FP2_pow2==1){
      models <- list("Second order FP,p1=1, p2=1" = list(g1=function(x){x^1},g2=function(x){log(x)*x^1},f1=function(x){x^1},f2=function(x){log(x)*x^1}))}
    else if(FP2_pow1==1 & FP2_pow2==2){
      models <- list("Second order FP,p1=1, p2=2" = list(g1=function(x){x^1},g2=function(x){x^2},f1=function(x){x^1},f2=function(x){x^2}))}
    else if(FP2_pow1==1 & FP2_pow2==3){
      models <- list("Second order FP,p1=1, p2=3" = list(g1=function(x){x^1},g2=function(x){x^3},f1=function(x){x^1},f2=function(x){x^3}))}
    else if(FP2_pow1==2 & FP2_pow2==2){
      models <- list("Second order FP,p1=2, p2=2" = list(g1=function(x){x^2},g2=function(x){log(x)*x^2},f1=function(x){x^2},f2=function(x){log(x)*x^2}))}
    else if(FP2_pow1==2 & FP2_pow2==3){
      models <- list("Second order FP,p1=2, p2=3" = list(g1=function(x){x^2},g2=function(x){x^3},f1=function(x){x^2},f2=function(x){x^3}))}
    else if(FP2_pow1==3 & FP2_pow2==3){
      models <- list("Second order FP,p1=3, p2=3" = list(g1=function(x){x^3},g2=function(x){log(x)*x^3},f1=function(x){x^3},f2=function(x){log(x)*x^3}))}
    
    fit.KM.NMA.REint <- function(bf){
      km.new = km
      km.new$g0 = 1
      km.new$g1 = bf[[1]](km.new$time)
      km.new$g2 = bf[[2]](km.new$time)
      #model formula
      f = cbind(nevents, natrisk - nevents) ~ 1 + (-1 + g0 | interaction(trtf, studyf)) + trtf * g1 + studyf * g1 + trtf * g2 + studyf * g2 # add random trt*study interaction (on g0 only)
      glmer(f, family = binomial(link = "cloglog"), data = km.new, offset = log(timeDelta))
    }
    fits <- lapply(models, fit.KM.NMA.REint)
    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_fp2fr_coeff.data<-coeff.data
    
    
    output$nma1_fp2fr_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                    options = list(pageLength = 10)
    )) 
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  observeEvent(input$nma1_fp2_freq_run2, {
    updateTabsetPanel(session, "fp2_freq_sub", selected ="fppage38")
  })
  
  observeEvent(input$nma1_fp2_freq_run2r, {
    updateTabsetPanel(session, "fp2_freq_sub", selected ="fppage39")
  })
  
  output$copy_nma1_fp2ff_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp2ff_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2ff_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp2fr_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp2fr_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2fr_coeff.data, file,row.names = TRUE)
    }
  )
  
  observeEvent(input$nma1_fp2_bay_run2,{
    follow_up<-input$nma1_fp_ex
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    FP2_pow1<-input$fp2_bay_pow1
    FP2_pow2<-input$fp2_bay_pow2
    
    temp_model_name<-paste0("FP2;","Power1=",FP2_pow1,";Power2=",FP2_pow2,";Bayesian;Fixed-effect")
    
    values$temp_fp2bf<-temp_model_name
    output$output_fp2bf<-renderUI({
      actionButton("goutput_fp2bf", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp2bf <- renderUI({
      HTML(paste("<u>","Current model is FP2 with power =",FP2_pow1,"and",FP2_pow2,"</u>"))
    })
    
    if (FP2_pow1 == 0){
      FP2_pow1<-FP2_pow2
      FP2_pow2<-0
    }
    
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      Na = d_std$n_arms,
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      a = dat$arm,
      t = as.matrix(select(ungroup(d_trts), -studyn)),
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    model.pars <- list(P1 = FP2_pow1, P2=FP2_pow2)
    #Fit the second order fractional polynomial model
    # set.seed(9487397)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    
    fit.fp <- jags(model.file = "www/jags_fp2.txt", 
                   data = c(dat_jg,
                            list(prior.mean = rep(input$fp2_pmean, 3)),
                            list(prior.prec = diag(rep(input$fp2_pprec, 3))),
                            model.pars),
                   parameters = c("d", "mu", "Beta"),
                   n.chains = input$fp2_nchains, n.iter = input$fp2_niter, n.burnin = input$fp2_nburnin, n.thin = input$fp2_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.fp$BUGSoutput$pV,3)
    DIC<-round(fit.fp$BUGSoutput$DIC,3)
    
    output$nma1_fp2bf_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.fp
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_fp2bf_coeff.data<-res
    
    res_mu0 <- fit.fp
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]    
    values$nma1_fp2bf_coeff.data_mu<-res_mu0
    
    
    output$nma1_fp2bf_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE, 
                                                      options = list(pageLength = 10)
    ))
    
    output$nma1_fp2bf_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE, 
                                                         options = list(pageLength = 10)
    ))
    
    res_mu <- fit.fp
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    res_mu3<-res_mu[(2*length(d_trts$studyn)+1):(3*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-mu3<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        mu3<-res_mu3[i]+mu3
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    mu3<-mu3/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    df_res2$median[(2*i_trt+2):(3*i_trt)]<-df_res2$median[(2*i_trt+2):(3*i_trt)]+mu3
    
    df_res22<-res
    
    if(FP2_pow1 == FP2_pow2 & FP2_pow2 != 0){
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*FP2_pow2*log(Time)}
      hr_fun1=function(i,Time){df_res22$median[i+1]+df_res22$median[i_trt+i+1]*(Time^FP2_pow1) + df_res22$median[i_trt*2+i+1]*(Time^FP2_pow2)*log(Time)}
    }else if(FP2_pow1==0 & FP2_pow2== 0){
      hr_fun0=function(Time){0+0*Time^0*log(Time) + 0*Time^0*log(Time)*log(Time)}
      hr_fun1=function(i,Time){df_res22$median[i+1]+df_res22$median[i_trt+i+1]*(Time^0)*log(Time) + df_res22$median[i_trt*2+i+1]*(Time^0)*log(Time)*log(Time)}
    }else if(FP2_pow2==0 & FP2_pow1 != 0){
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*Time^0*log(Time)}
      hr_fun1=function(i,Time){df_res22$median[i+1]+df_res22$median[i_trt+i+1]*(Time^FP2_pow1) + df_res22$median[i_trt*2+i+1]*(Time^0)*log(Time)}
    }else{
      hr_fun0=function(Time){0+0*Time^FP2_pow1 + 0*Time^FP2_pow2}
      hr_fun1=function(i,Time){df_res22$median[i+1]+df_res22$median[i_trt+i+1]*(Time^FP2_pow1) + df_res22$median[i_trt*2+i+1]*(Time^FP2_pow2)}
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    df_hr[,2]<-exp(hr_fun0(df_hr$time))
    for (i in 1:(i_trt-1)) {
      df_hr[,i+2]<-exp(hr_fun1(i,df_hr$time))
    }
    values$nma1_fp2bf_hrdata<-df_hr
    
    output$nma1_fp2bf_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_fp2bf_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_fp2bf_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    if(FP2_pow1 == FP2_pow2 & FP2_pow2 != 0){
      haz_fun0=function(Time){mu1+mu2*Time^FP2_pow1 + mu3*FP2_pow2*log(Time)}
      haz_fun1=function(i,Time){df_res2$median[i+1]+df_res2$median[i_trt+i+1]*(Time^FP2_pow1) + df_res2$median[i_trt*2+i+1]*(Time^FP2_pow2)*log(Time)}
    }else if(FP2_pow1==0 & FP2_pow2== 0){
      haz_fun0=function(Time){mu1+mu2*Time^0*log(Time) + mu3*Time^0*log(Time)*log(Time)}
      haz_fun1=function(i,Time){df_res2$median[i+1]+df_res2$median[i_trt+i+1]*(Time^0)*log(Time) + df_res2$median[i_trt*2+i+1]*(Time^0)*log(Time)*log(Time)}
    }else if(FP2_pow2==0 & FP2_pow1 != 0){
      haz_fun0=function(Time){mu1+mu2*Time^FP2_pow1 + mu3*Time^0*log(Time)}
      haz_fun1=function(i,Time){df_res2$median[i+1]+df_res2$median[i_trt+i+1]*(Time^FP2_pow1) + df_res2$median[i_trt*2+i+1]*(Time^0)*log(Time)}
    }else{
      haz_fun0=function(Time){mu1+mu2*Time^FP2_pow1 + mu3*Time^FP2_pow2}
      haz_fun1=function(i,Time){df_res2$median[i+1]+df_res2$median[i_trt+i+1]*(Time^FP2_pow1) + df_res2$median[i_trt*2+i+1]*(Time^FP2_pow2)}
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    df_haz[,2]<-exp(haz_fun0(df_haz$time))
    for (i in 1:(i_trt-1)) {
      df_haz[,i+2]<-exp(haz_fun1(i,df_haz$time))
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf)) 
    
    output$nma1_fp2bf_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_fp, input$y_max_hr_fp))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp2bf_hr)
      print(f1)
      values$graph_fp2bf_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp2bf_plot_hr_dw, {
      output$nma1_fp2bf_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_fp2bf_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2bf_plot_surv_dw, {
      output$nma1_fp2bf_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2bf_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2bf_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2bf_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2bf_hr, width = as.numeric(input$nma1_fp2bf_plot_hr_width),
               height = as.numeric(input$nma1_fp2bf_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_fp2bf_plot_haz<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_fp==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_fp, input$y_max_haz_fp))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_fp2bf_haz)
      print(f1)
      values$graph_fp2bf_haz<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_fp2bf_plot_haz_dw, {
      output$nma1_fp2bf_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_fp2bf_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2bf_plot_surv_dw, {
      output$nma1_fp2bf_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2bf_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2bf_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2bf_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2bf_haz, width = as.numeric(input$nma1_fp2bf_plot_haz_width),
               height = as.numeric(input$nma1_fp2bf_plot_haz_height),
               units = "px")
      }
    )
    
    output$nma1_fp2bf_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_fp2bf_surv)
      print(f2)
      values$graph_fp2bf_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_fp2bf_plot_surv_dw, {
      output$nma1_fp2bf_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_fp2bf_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_fp2bf_plot_surv_dw, {
      output$nma1_fp2bf_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_fp2bf_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_fp2bf_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_fp2bf_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_fp2bf_surv, width = as.numeric(input$nma1_fp2bf_plot_surv_width),
               height = as.numeric(input$nma1_fp2bf_plot_surv_height),
               units = "px")
      }
    )
    rm(fit.fp)
  })
  
  observeEvent(input$nma1_fp2_bay_run2r,{
    follow_up<-input$nma1_fp_ex
    km<-values$nma1_fp_km
    ref.study <- input$nma1_fp_refs
    ref.trt   <- input$nma1_fp_reft
    FP2_pow1<-input$fp2_bay_pow1
    FP2_pow2<-input$fp2_bay_pow2
    
    temp_model_name<-paste0("FP2;","Power1=",FP2_pow1,";Power2=",FP2_pow2,";Bayesian;Random-effects")
    
    values$temp_fp2br<-temp_model_name
    output$output_fp2br<-renderUI({
      actionButton("goutput_fp2br", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    output$text_fp2br <- renderUI({
      HTML(paste("<u>","Current model is FP2 with power =",FP2_pow1,"and",FP2_pow2,"</u>"))
    })
    
    if (FP2_pow1 == 0){
      FP2_pow1<-FP2_pow2
      FP2_pow2<-0
    }
    
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )

    model.pars <- list(P1 = FP2_pow1, P2=FP2_pow2)
    #Fit the second order fractional polynomial model
    # set.seed(9487397)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.fp <- jags(model.file = "www/jags_fp2_bay_d0.txt", 
                   data = c(dat_jg,
                            list(prior.mean = rep(input$fp2_pmean, 3)),
                            list(prior.prec = diag(rep(input$fp2_pprec, 3))),
                            model.pars),
                   parameters = c("d", "mu", "Beta","sd"),
                   n.chains = input$fp2_nchains, n.iter = input$fp2_niter, n.burnin = input$fp2_nburnin, n.thin = input$fp2_nthin)
    
    progress$close()
    removeModal()
    
    
    # REPORT dic
    pD<-round(fit.fp$BUGSoutput$pV,3)
    DIC<-round(fit.fp$BUGSoutput$DIC,3)
    
    output$nma1_fp2br_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.fp
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_fp2br_coeff.data<-res
    
    res_mu0 <- fit.fp
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_fp2br_coeff.data_mu<-res_mu0
    
    
    output$nma1_fp2br_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE, 
                                                      options = list(pageLength = 10)
    ))
    
    output$nma1_fp2br_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE, 
                                                         options = list(pageLength = 10)
    ))
    
    shinyalert(title = "Complete!", type = "success")
    
    rm(fit.fp)
  })
  
  observeEvent(input$nma1_fp2_bay_run2, {
    updateTabsetPanel(session, "fp2_bay_sub", selected ="fppage311")
  })
  
  observeEvent(input$nma1_fp2_bay_run2r, {
    updateTabsetPanel(session, "fp2_bay_sub", selected ="fppage312")
  })
  
  output$copy_nma1_fp2bf_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp2bf_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2bf_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp2bf_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_fp2bf_coeff_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2bf_coeff.data_mu, file,row.names = TRUE)
    }
  ) 
  
  output$copy_nma1_fp2br_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_fp2br_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2br_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_fp2br_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_fp2br_coeff_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_fp2br_coeff.data_mu, file,row.names = TRUE)
    }
  ) 
  
  observe({
    if(input$nma1_fp_run > 0) {
      session$sendCustomMessage('enableTab5', list(enable = TRUE))
      session$sendCustomMessage('enableTab6', list(enable = TRUE))
      session$sendCustomMessage('enableTab7', list(enable = TRUE))
    }
  })
  
  ##### end of fp #####

  ##### PWE #####
  
  observeEvent(input$nma1_pwe1f_run1, {
    updateTabsetPanel(session, "subtabs_pwe1", selected ="pwepage22")
  })
  
  observeEvent(input$nma1_pwe1b_run1, {
    updateTabsetPanel(session, "subtabs_pwe1", selected ="pwepage23")
  })
  
  observeEvent(input$nma1_pwe2f_run2, {
    updateTabsetPanel(session, "subtabs_pwe2", selected ="pwepage25")
  })
  
  observeEvent(input$nma1_pwe2b_run2, {
    updateTabsetPanel(session, "subtabs_pwe2", selected ="pwepage26")
  })
  
  observeEvent(input$nma1_pwe1f_run1,{
    follow_up<-input$nma1_pwe_ex
    pwe1_pow<-input$pwe1_pow
    ref.study <- input$nma1_pwe_refs
    ref.trt   <- input$nma1_pwe_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PWE1;","Cutpoint=",pwe1_pow,";Frequentist;Fixed-effect")
    
    values$temp_pwe1f<-temp_model_name
    output$output_pwe1f<-renderUI({
      actionButton("goutput_pwe1f", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    models <- list( "PWE1" = list(g1=function(x){as.numeric(x>pwe1_pow)},g2=function(x){0},f1=function(x){as.numeric(x>pwe1_pow)},f2=function(x){0}))
    
    fit.KM.NMA<-function(bf){
      km.new=km
      km.new$g0=1
      km.new$f0=1
      km.new$g1=bf[[1]](km.new$time)
      km.new$g2=bf[[2]](km.new$time)
      km.new$f1=bf[[3]](km.new$time)
      km.new$f2=bf[[4]](km.new$time)
      #model formula
      f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
      glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
    }
    fits=lapply(models,fit.KM.NMA)
    pred.KM.NMA<-function(bf){
      trts=data.frame(trtf=unique(km$trtf))
      trts$studyf=sort(unique(km$studyf))[1] # select reference study as baseline
      timehorizon=data.frame(time=2*(1:(follow_up*6)))
      km.pred=merge.data.frame(timehorizon,trts)
      km.pred$g0=1
      km.pred$f0=1
      km.pred$g1=bf[[1]](km.pred$time)
      km.pred$g2=bf[[2]](km.pred$time)
      km.pred$f1=bf[[3]](km.pred$time)
      km.pred$f2=bf[[4]](km.pred$time)
      km.pred$timeDelta<-2
      km.pred
    }
    pred.KM.data=lapply(models,pred.KM.NMA)
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>% 
        filter(trtf == ref.trt)%>%
        mutate(pred.doce = pred)%>% 
        select(pred.doce,time) %>% 
        arrange(time) %>% 
        left_join(d) %>%
        mutate(lnhr = pred-pred.doce) %>% 
        mutate(hr=exp(lnhr))#%>%
      d1$modelc=names(models)[[i]]
      if(i==1)dpred<-d1
      if(i!=1)dpred<-rbind(dpred,d1)
    }
    dpred$Model<-factor(dpred$modelc,levels=names(models))
    
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>%
        dplyr::mutate(haz = exp(pred))%>%
        dplyr::group_by(trtf) %>%
        dplyr::arrange(time) %>%
        dplyr::mutate(cumhaz = cumsum(haz)) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      zero <- unique(d1[,c("trtf", "studyf")])
      zero$time=0
      zero$survProp=1
      d1=rbind(zero,d1)
      
      d1$modelc=names(models)[[i]]
      if(i==1)dpred1<-d1
      if(i!=1)dpred1<-rbind(dpred1,d1)
      
    }
    dpred1$Model<-factor(dpred1$modelc,levels=names(models))
    
    output$nma1_pwe1f_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dpred,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) + 
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") + 
        theme_bw() 
      f1
      if (input$y_axis_hr_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_pwe, input$y_max_hr_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe1f_hr)
      print(f1)
      values$graph_pwe1f_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1f_plot_hr_dw, {
      output$nma1_pwe1f_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_pwe1f_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1f_plot_surv_dw, {
      output$nma1_pwe1f_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1f_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1f_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1f_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1f_hr, width = as.numeric(input$nma1_pwe1f_plot_hr_width),
               height = as.numeric(input$nma1_pwe1f_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_pwe1f_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=survProp, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_pwe1f_surv)
      print(f2)
      values$graph_pwe1f_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1f_plot_surv_dw, {
      output$nma1_pwe1f_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_pwe1f_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1f_plot_surv_dw, {
      output$nma1_pwe1f_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1f_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1f_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1f_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1f_surv, width = as.numeric(input$nma1_pwe1f_plot_surv_width),
               height = as.numeric(input$nma1_pwe1f_plot_surv_height),
               units = "px")
      }
    )
    
    output$nma1_pwe1f_plot_haz<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=haz, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      if (input$y_axis_haz_pwe==TRUE){
        f2<-f2+scale_y_log10(limits = c(input$y_min_haz_pwe, input$y_max_haz_pwe))
      }else {f2<-f2+scale_y_log10()}
      f2<-f2 + ggtitle(input$ggtitle_pwe1f_haz)
      print(f2)
      values$graph_pwe1f_haz<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1f_plot_haz_dw, {
      output$nma1_pwe1f_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_pwe1f_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1f_plot_haz_dw, {
      output$nma1_pwe1f_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1f_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1f_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1f_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1f_haz, width = as.numeric(input$nma1_pwe1f_plot_haz_width),
               height = as.numeric(input$nma1_pwe1f_plot_haz_height),
               units = "px")
      }
    )
    
    
    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_pwe1f_coeff.data<-coeff.data
    
    output$nma1_pwe1f_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                     options = list(pageLength = 10)
    )) 
    
    i_trt<-nlevels(km$trtf)-1
    df_res3<-coeff.data
    
    hr_fun0=function(Time){
      y<- if (Time <= pwe1_pow){
        0
      } else if (Time > pwe1_pow) {
        0
      }
      return(y)
    }
    hr_fun1=function(i,Time){
      y<- if (Time <= pwe1_pow){
        df_res3$est[i]
      } else if (Time > pwe1_pow){
        df_res3$est[i_trt+i] - df_res3$est[i]
      }
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(km$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",levels(km$trtf))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    values$nma1_pwe1f_hrdata<-df_hr
    output$nma1_pwe1f_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_pwe1f_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_pwe1f_hrdata, file,row.names = TRUE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  output$copy_nma1_pwe1f_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_pwe1f_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe1f_coeff.data, file,row.names = TRUE)
    }
  )

  observeEvent(input$nma1_pwe1b_run1,{
    follow_up<-input$nma1_pwe_ex
    pwe1_pow<-input$pwe1_pow
    ref.study <- input$nma1_pwe_refs
    ref.trt   <- input$nma1_pwe_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PWE1;","Cutpoint=",pwe1_pow,";Bayesian;Fixed-effect")
    
    values$temp_pwe1b<-temp_model_name
    output$output_pwe1b<-renderUI({
      actionButton("goutput_pwe1b", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      Na = d_std$n_arms,
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      a = dat$arm,
      t = as.matrix(select(ungroup(d_trts), -studyn)),
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    #Gnerate the time segment data
    cut.points <- c(pwe1_pow)
    cut.points0 <- c(0, cut.points)
    cut.pointsInf <- c(cut.points, Inf)
    
    dat$segment <- rep(NA, Nobs)
    for (i in 1:Nobs){
      dat$segment[i] <- last(which(dat$time[i] > cut.points0)) # grouped TTE data over [time - time Delta, time]
    }
    dat_jg[["time"]] <- NULL
    dat_jg$segment <- dat$segment
    dat_jg$Ncuts <- length(cut.points)
    
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.pwe <- jags(model.file = "www/jags_pwe_1.txt", 
                    data = c(dat_jg,
                             list(prior.mean = input$pwe1_pmean),
                             list(prior.prec = input$pwe1_pprec)),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$pwe1_nchains, n.iter = input$pwe1_niter, n.burnin = input$pwe1_nburnin, n.thin = input$pwe1_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.pwe$BUGSoutput$pV,3)
    DIC<-round(fit.pwe$BUGSoutput$DIC,3)
    
    output$nma1_pwe1b_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.pwe
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_pwe1b_coeff.data<-res
    
    res_mu0 <- fit.pwe
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_pwe1b_coeff.data_mu<-res_mu0
    
    
    output$nma1_pwe1b_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    output$nma1_pwe1b_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    res_mu <- fit.pwe
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    # data.frame(median=res$median)
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    
    
    hr_fun0=function(Time){
      y<- if (Time <= pwe1_pow){
        0
      } else if (Time > pwe1_pow) {
        0
      }
      return(y)
    }
    hr_fun1=function(i,Time){
      y<- if (Time <= pwe1_pow){
        df_res3$median[i+1]
      } else if (Time > pwe1_pow){
        df_res3$median[i_trt+i+1] - df_res3$median[i+1]
      }
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    values$nma1_pwe1b_hrdata<-df_hr
    output$nma1_pwe1b_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_pwe1b_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_pwe1b_hrdata, file,row.names = TRUE)
      }
    )
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    
    haz_fun0=function(Time){
      y<- if (Time <= pwe1_pow){
        mu1
      } else if (Time > pwe1_pow) {
        mu2 - mu1
      }
      return(y)
    }
    haz_fun1=function(i,Time){
      y<- if (Time <= pwe1_pow){
        df_res2$median[i+1]
      } else if (Time > pwe1_pow){
        df_res2$median[i_trt+i+1] - df_res2$median[i+1]
      }
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf)) 
    
    output$nma1_pwe1b_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_pwe, input$y_max_hr_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe1b_hr)
      print(f1)
      values$graph_pwe1b_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1b_plot_hr_dw, {
      output$nma1_pwe1b_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_pwe1b_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1b_plot_survb_dw, {
      output$nma1_pwe1b_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1b_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1b_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1b_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1b_hr, width = as.numeric(input$nma1_pwe1b_plot_hr_width),
               height = as.numeric(input$nma1_pwe1b_plot_hr_height),
               units = "px")
      }
    )
    
    
    output$nma1_pwe1b_plot_haz<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_pwe, input$y_max_haz_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe1b_haz)
      print(f1)
      values$graph_pwe1b_haz<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1b_plot_haz_dw, {
      output$nma1_pwe1b_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_pwe1b_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1b_plot_surv_dw, {
      output$nma1_pwe1b_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1b_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1b_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1b_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1b_haz, width = as.numeric(input$nma1_pwe1b_plot_haz_width),
               height = as.numeric(input$nma1_pwe1b_plot_haz_height),
               units = "px")
      }
    )
    
    output$nma1_pwe1b_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_pwe1b_surv)
      print(f2)
      values$graph_pwe1b_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe1b_plot_surv_dw, {
      output$nma1_pwe1b_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_pwe1b_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe1b_plot_surv_dw, {
      output$nma1_pwe1b_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe1b_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe1b_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe1b_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe1b_surv, width = as.numeric(input$nma1_pwe1b_plot_surv_width),
               height = as.numeric(input$nma1_pwe1b_plot_surv_height),
               units = "px")
      }
    )
    rm(fit.pwe)
  })
  
  output$copy_nma1_pwe1b_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_pwe1b_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe1b_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_pwe1b_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_pwe1b_coeff_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe1b_coeff.data_mu, file,row.names = TRUE)
    }
  )
  
  observeEvent(input$nma1_pwe2f_run2,{
    follow_up<-input$nma1_pwe_ex
    pwe2_pow1<-input$pwe2_pow1
    pwe2_pow2<-input$pwe2_pow2
    ref.study <- input$nma1_pwe_refs
    ref.trt   <- input$nma1_pwe_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PWE2;","Cutpoint1=",pwe2_pow1,";Cutpoint2=",pwe2_pow2,";Frequentist;Fixed-effect")
    
    values$temp_pwe2f<-temp_model_name
    output$output_pwe2f<-renderUI({
      actionButton("goutput_pwe2f", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    models <- list("PWE2" = list(g1=function(x){as.numeric(x>pwe2_pow1&x<=pwe2_pow2)},g2=function(x){as.numeric(x>pwe2_pow2)},
                                 f1=function(x){as.numeric(x>pwe2_pow1&x<=pwe2_pow2)},f2=function(x){as.numeric(x>pwe2_pow2)}))
    
    fit.KM.NMA<-function(bf){
      km.new=km
      km.new$g0=1
      km.new$f0=1
      km.new$g1=bf[[1]](km.new$time)
      km.new$g2=bf[[2]](km.new$time)
      km.new$f1=bf[[3]](km.new$time)
      km.new$f2=bf[[4]](km.new$time)
      #model formula
      f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
      glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
    }
    fits=lapply(models,fit.KM.NMA)
    pred.KM.NMA<-function(bf){
      trts=data.frame(trtf=unique(km$trtf))
      trts$studyf=sort(unique(km$studyf))[1] # select reference study as baseline
      timehorizon=data.frame(time=2*(1:(follow_up*6)))
      km.pred=merge.data.frame(timehorizon,trts)
      km.pred$g0=1
      km.pred$f0=1
      km.pred$g1=bf[[1]](km.pred$time)
      km.pred$g2=bf[[2]](km.pred$time)
      km.pred$f1=bf[[3]](km.pred$time)
      km.pred$f2=bf[[4]](km.pred$time)
      km.pred$timeDelta<-2
      km.pred
    }
    pred.KM.data=lapply(models,pred.KM.NMA)
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>% 
        filter(trtf == ref.trt)%>%
        mutate(pred.doce = pred)%>% 
        select(pred.doce,time) %>% 
        arrange(time) %>% 
        left_join(d) %>%
        mutate(lnhr = pred-pred.doce) %>% 
        mutate(hr=exp(lnhr))#%>%
      d1$modelc=names(models)[[i]]
      if(i==1)dpred<-d1
      if(i!=1)dpred<-rbind(dpred,d1)
    }
    dpred$Model<-factor(dpred$modelc,levels=names(models))
    
    for(i in 1:length(models)){
      pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
      d=pred.KM.data[[i]]
      d1<-d %>%
        dplyr::mutate(haz = exp(pred))%>%
        dplyr::group_by(trtf) %>%
        dplyr::arrange(time) %>%
        dplyr::mutate(cumhaz = cumsum(haz)) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      zero <- unique(d1[,c("trtf", "studyf")])
      zero$time=0
      zero$survProp=1
      d1=rbind(zero,d1)
      
      d1$modelc=names(models)[[i]]
      if(i==1)dpred1<-d1
      if(i!=1)dpred1<-rbind(dpred1,d1)
      
    }
    dpred1$Model<-factor(dpred1$modelc,levels=names(models))
    
    output$nma1_pwe2f_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dpred,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) + 
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") + 
        theme_bw() 
      f1
      if (input$y_axis_hr_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_pwe, input$y_max_hr_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe2f_hr)
      print(f1)
      values$graph_pwe2f_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2f_plot_hr_dw, {
      output$nma1_pwe2f_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_pwe2f_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2f_plot_surv_dw, {
      output$nma1_pwe2f_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2f_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2f_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2f_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2f_hr, width = as.numeric(input$nma1_pwe2f_plot_hr_width),
               height = as.numeric(input$nma1_pwe2f_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_pwe2f_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=survProp, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_pwe2f_surv)
      print(f2)
      values$graph_pwe2f_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2f_plot_surv_dw, {
      output$nma1_pwe2f_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_pwe2f_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2f_plot_surv_dw, {
      output$nma1_pwe2f_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2f_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2f_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2f_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2f_surv, width = as.numeric(input$nma1_pwe2f_plot_surv_width),
               height = as.numeric(input$nma1_pwe2f_plot_surv_height),
               units = "px")
      }
    )
    
    
    output$nma1_pwe2f_plot_haz<-renderPlot({
      f2= ggplot() +
        geom_line(data=dpred1, aes(x=time, y=haz, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      if (input$y_axis_haz_pwe==TRUE){
        f2<-f2+scale_y_log10(limits = c(input$y_min_haz_pwe, input$y_max_haz_pwe))
      }else {f2<-f2+scale_y_log10()}
      f2<-f2 + ggtitle(input$ggtitle_pwe2f_haz)
      print(f2)
      values$graph_pwe2f_haz<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2f_plot_haz_dw, {
      output$nma1_pwe2f_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_pwe2f_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2f_plot_haz_dw, {
      output$nma1_pwe2f_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2f_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2f_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2f_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2f_haz, width = as.numeric(input$nma1_pwe2f_plot_haz_width),
               height = as.numeric(input$nma1_pwe2f_plot_haz_height),
               units = "px")
      }
    )
    
    coeff.data            <- as.data.frame(coef(summary(fits[[1]])))[,1:2]
    names(coeff.data)     <- c("est","std.err")
    attach(coeff.data)
    coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
    coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
    coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
    coeff.data$pn         <- rownames(coeff.data)
    coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
        coeff.data<-coeff.data[,1:4]     
        rownames(coeff.data) <- substr(rownames(coeff.data), 5, nchar(rownames(coeff.data)))
    values$nma1_pwe2f_coeff.data<-coeff.data
    
    
    output$nma1_pwe2f_coeff <- renderDT(datatable(coeff.data, editable = FALSE, rownames = TRUE, 
                                                     options = list(pageLength = 10)
    ))
    
    i_trt<-nlevels(km$trtf)-1
    df_res3<-coeff.data
    hr_fun0=function(Time){
      y<- if (Time <= pwe2_pow1){
        0
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        0
      } else if (Time >= pwe2_pow2) {
        0
      }
      return(y)
    }
    hr_fun1=function(i,Time){
      y<- if (Time <= pwe2_pow1){
        df_res3$est[i]
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        df_res3$est[i_trt+i] - df_res3$est[i]
      } else if (Time >= pwe2_pow2) {
        df_res3$est[i_trt*2+i] - df_res3$est[i]
      }
      return(y)
    }
    df_hr<-as.data.frame(matrix(ncol = (nlevels(km$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",levels(km$trtf))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    values$nma1_pwe2f_hrdata<-df_hr
    ### end: calculate hr all
    output$nma1_pwe2f_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_pwe2f_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_pwe2f_hrdata, file,row.names = TRUE)
      }
    )
    
    shinyalert(title = "Complete!", type = "success")
    
  })
  
  output$copy_nma1_pwe2f_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_pwe2f_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe2f_coeff.data, file,row.names = TRUE)
    }
  )
  
  observeEvent(input$nma1_pwe2b_run2,{
    follow_up<-input$nma1_pwe_ex
    ref.study <- input$nma1_pwe_refs
    ref.trt   <- input$nma1_pwe_reft
    pwe2_pow1<-input$pwe2_pow1
    pwe2_pow2<-input$pwe2_pow2
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PWE2;","Cutpoint1=",pwe2_pow1,";Cutpoint2=",pwe2_pow2,";Bayesian;Fixed-effect")
    
    values$temp_pwe2b<-temp_model_name
    output$output_pwe2b<-renderUI({
      actionButton("goutput_pwe2b", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      Na = d_std$n_arms,
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      a = dat$arm,
      t = as.matrix(select(ungroup(d_trts), -studyn)),
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    cut.points <- c(pwe2_pow1,pwe2_pow2)
    cut.points0 <- c(0, cut.points)
    cut.pointsInf <- c(cut.points, Inf)
    
    dat$segment <- rep(NA, Nobs)
    for (i in 1:Nobs){
      dat$segment[i] <- last(which(dat$time[i] > cut.points0)) # grouped TTE data over [time - time Delta, time]
    }
    dat_jg[["time"]] <- NULL
    dat_jg$segment <- dat$segment
    dat_jg$Ncuts <- length(cut.points)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    #Fit the piecewise exponential model
    # set.seed(9487397)
    fit.pwe <- jags(model.file = "www/jags_pwe_2.txt", 
                    data = c(dat_jg,
                             list(prior.mean = input$pwe2_pmean),
                             list(prior.prec = input$pwe2_pprec)
                    ),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$pwe2_nchains, n.iter = input$pwe2_niter, n.burnin = input$pwe2_nburnin, n.thin = input$pwe2_nthin)
    
    progress$close()
    removeModal()   
    
    # REPORT dic
    pD<-round(fit.pwe$BUGSoutput$pV,3)
    DIC<-round(fit.pwe$BUGSoutput$DIC,3)
    
    output$nma1_pwe2b_DIC <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.pwe
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_pwe2b_coeff.data<-res
    
    res_mu0 <- fit.pwe
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_pwe2b_coeff.data_mu<-res_mu0
    
    
    output$nma1_pwe2b_coeff <- renderDT(datatable(res, editable = FALSE, rownames = TRUE, 
                                                       options = list(pageLength = 10)
    ))
    
    output$nma1_pwe2b_coeff_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE, 
                                                          options = list(pageLength = 10)
    ))
    
    res_mu <- fit.pwe
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    res_mu3<-res_mu[(2*length(d_trts$studyn)+1):(3*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-mu3<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        mu3<-res_mu3[i]+mu3
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    mu3<-mu3/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    # data.frame(median=res$median)
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    df_res2$median[(2*i_trt+2):(3*i_trt)]<-df_res2$median[(2*i_trt+2):(3*i_trt)]+mu3
    
    df_res3<-res
    
    hr_fun0=function(Time){
      y<- if (Time <= pwe2_pow1){
        0
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        0
      } else if (Time >= pwe2_pow2) {
        0
      }
      return(y)
    }
    hr_fun1=function(i,Time){
      y<- if (Time <= pwe2_pow1){
        df_res3$median[i+1]
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        df_res3$median[i_trt+i+1] - df_res3$median[i+1]
      } else if (Time >= pwe2_pow2) {
        df_res3$median[i_trt*2+i+1] - df_res3$median[i+1]
      }
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_pwe2b_hrdata<-df_hr
    output$nma1_pwe2b_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_pwe2b_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_pwe2b_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    haz_fun0=function(Time){
      y<- if (Time <= pwe2_pow1){
        mu1
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        mu2 - mu1
      } else if (Time >= pwe2_pow2) {
        mu3 - mu1
      }
      return(y)
    }
    haz_fun1=function(i,Time){
      y<- if (Time <= pwe2_pow1){
        df_res2$median[i+1]
      } else if (Time > pwe2_pow1 & Time < pwe2_pow2){
        df_res2$median[i_trt+i+1] - df_res2$median[i+1]
      } else if (Time >= pwe2_pow2) {
        df_res2$median[i_trt*2+i+1] - df_res2$median[i+1]
      }
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf)) 
    
    output$nma1_pwe2b_plot_hr<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_pwe, input$y_max_hr_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe2b_hr)
      print(f1)
      values$graph_pwe2b_hr<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2b_plot_hr_dw, {
      output$nma1_pwe2b_plot_hr_titlebut <- renderUI({
        textInput("ggtitle_pwe2b_hr", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2b_plot_surv_dw, {
      output$nma1_pwe2b_plot_hr_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2b_plot_hr_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2b_plot_hr_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2b_plot_hr <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2b_hr, width = as.numeric(input$nma1_pwe2b_plot_hr_width),
               height = as.numeric(input$nma1_pwe2b_plot_hr_height),
               units = "px")
      }
    )
    
    output$nma1_pwe2b_plot_haz<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_pwe==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_pwe, input$y_max_haz_pwe))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_pwe2b_haz)
      print(f1)
      values$graph_pwe2b_haz<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2b_plot_haz_dw, {
      output$nma1_pwe2b_plot_haz_titlebut <- renderUI({
        textInput("ggtitle_pwe2b_haz", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2b_plot_surv_dw, {
      output$nma1_pwe2b_plot_haz_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2b_plot_haz_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2b_plot_haz_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2b_plot_haz <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2b_haz, width = as.numeric(input$nma1_pwe2b_plot_haz_width),
               height = as.numeric(input$nma1_pwe2b_plot_haz_height),
               units = "px")
      }
    )
    
    output$nma1_pwe2b_plot_surv<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_pwe2b_surv)
      print(f2)
      values$graph_pwe2b_surv<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_pwe2b_plot_surv_dw, {
      output$nma1_pwe2b_plot_surv_titlebut <- renderUI({
        textInput("ggtitle_pwe2b_surv", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_pwe2b_plot_surv_dw, {
      output$nma1_pwe2b_plot_surv_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_pwe2b_plot_surv_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_pwe2b_plot_surv_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_pwe2b_plot_surv <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_pwe2b_surv, width = as.numeric(input$nma1_pwe2b_plot_surv_width),
               height = as.numeric(input$nma1_pwe2b_plot_surv_height),
               units = "px")
      }
    )
    rm(fit.pwe)
  })
  
  output$copy_nma1_pwe2b_coeff <- downloadHandler(
    filename = function() {
      paste("nma1_pwe2b_coeff", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe2b_coeff.data, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_pwe2b_coeff_mu <- downloadHandler(
    filename = function() {
      paste("nma1_pwe2b_coeff_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_pwe2b_coeff.data_mu, file,row.names = TRUE)
    }
  )
  
  ##### end of PWE #####
  
  ##### PSM #####
  observeEvent(input$nma1_psm1_run, {
    updateTabsetPanel(session, "subtabs_wei", selected ="psmpage22")
  })
  
  observeEvent(input$nma1_psm1_runr, {
    updateTabsetPanel(session, "subtabs_wei", selected ="psmpage23")
  })
  
  observeEvent(input$nma1_psm2_run, {
    updateTabsetPanel(session, "subtabs_gom", selected ="psmpage25")
  })
  
  observeEvent(input$nma1_psm2_runr, {
    updateTabsetPanel(session, "subtabs_gom", selected ="psmpage26")
  })
  
  observeEvent(input$nma1_psm3_run, {
    updateTabsetPanel(session, "subtabs_loglog", selected ="psmpage28")
  })
  
  observeEvent(input$nma1_psm3_runr, {
    updateTabsetPanel(session, "subtabs_loglog", selected ="psmpage29")
  })
  
  observeEvent(input$nma1_psm4_run, {
    updateTabsetPanel(session, "subtabs_lognor", selected ="psmpage211")
  })
  
  observeEvent(input$nma1_psm4_runr, {
    updateTabsetPanel(session, "subtabs_lognor", selected ="psmpage212")
  })
  
  observeEvent(input$nma1_psm1_run,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Weibull;","Bayesian;Fixed-effect")
    
    values$temp_psm11<-temp_model_name
    output$output_psm11<-renderUI({
      actionButton("goutput_psm11", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    
    fit.psm <- jags(model.file = "www/weibull_f.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm1_pmean, 2)),
                             list(prec2 = diag(rep(input$psm1_pprec, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm1_nchains, n.iter = input$psm1_niter, n.burnin = input$psm1_nburnin, n.thin = input$psm1_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_1 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_1<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_1_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_1_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_1 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    
    hr_fun0=function(Time){
      y= 0+log(Time)*0
      return(y)
    }
    hr_fun1=function(i,Time){
      y=df_res3$median[i+1]+log(Time)*df_res3$median[i_trt+i+1]
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm11_hrdata<-df_hr
    output$nma1_psm11_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm11_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm11_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    
    
    haz_fun0=function(Time){
      y= mu1+log(Time)*mu2
      return(y)
    }
    haz_fun1=function(i,Time){
      y=df_res2$median[i+1]+log(Time)*df_res2$median[i_trt+i+1]
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_psm1_plot_hr_1<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_1)
      print(f1)
      values$graph_psm1_hr_1<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_1_dw, {
      output$nma1_psm1_plot_hr_1_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_1", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_1_dw, {
      output$nma1_psm1_plot_hr_1_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_1_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_1_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_1 <- downloadHandler(
      filename = function() { paste0('hr_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_1, width = as.numeric(input$nma1_psm1_plot_hr_1_width),
               height = as.numeric(input$nma1_psm1_plot_hr_1_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_haz_1<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_1)
      print(f1)
      values$graph_psm1_haz_1<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_1_dw, {
      output$nma1_psm1_plot_haz_1_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_1", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_1_dw, {
      output$nma1_psm1_plot_haz_1_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_1_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_1_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_1 <- downloadHandler(
      filename = function() { paste0('haz_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_1, width = as.numeric(input$nma1_psm1_plot_haz_1_width),
               height = as.numeric(input$nma1_psm1_plot_haz_1_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_1<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_1)
      print(f2)
      values$graph_psm1_surv_1<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_1_dw, {
      output$nma1_psm1_plot_surv_1_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_1", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_1_dw, {
      output$nma1_psm1_plot_surv_1_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_1_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_1_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_1 <- downloadHandler(
      filename = function() { paste0('surv_plot_1.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_1, width = as.numeric(input$nma1_psm1_plot_surv_1_width),
               height = as.numeric(input$nma1_psm1_plot_surv_1_height),
               units = "px")
      }
    )

    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm1_runr,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Weibull;","Bayesian;Random-effects")
    
    values$temp_psm12<-temp_model_name
    output$output_psm12<-renderUI({
      actionButton("goutput_psm12", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    
    fit.psm <- jags(model.file = "www/weibull_ra.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm1_pmean, 2)),
                             list(prec2 = diag(rep(input$psm1_pprec, 2))),
                             list(R = diag(rep(input$psm1_pr, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm1_nchains, n.iter = input$psm1_niter, n.burnin = input$psm1_nburnin, n.thin = input$psm1_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_2 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_2<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_2_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_2_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_2 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    
    hr_fun0=function(Time){
      y= 0+log(Time)*0
      return(y)
    }
    hr_fun1=function(i,Time){
      y=df_res3$median[i+1]+log(Time)*df_res3$median[i_trt+i+1]
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    values$nma1_psm12_hrdata<-df_hr
    output$nma1_psm12_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm12_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm12_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    
    haz_fun0=function(Time){
      y= mu1+log(Time)*mu2
      return(y)
    }
    haz_fun1=function(i,Time){
      y=df_res2$median[i+1]+log(Time)*df_res2$median[i_trt+i+1]
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    
    output$nma1_psm1_plot_hr_2<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_2)
      print(f1)
      values$graph_psm1_hr_2<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_2_dw, {
      output$nma1_psm1_plot_hr_2_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_2", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_2_dw, {
      output$nma1_psm1_plot_hr_2_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_2_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_2_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_2 <- downloadHandler(
      filename = function() { paste0('hr_plot_2.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_2, width = as.numeric(input$nma1_psm1_plot_hr_2_width),
               height = as.numeric(input$nma1_psm1_plot_hr_2_height),
               units = "px")
      }
    )
    
    
    output$nma1_psm1_plot_haz_2<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_2)
      print(f1)
      values$graph_psm1_haz_2<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_2_dw, {
      output$nma1_psm1_plot_haz_2_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_2", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_2_dw, {
      output$nma1_psm1_plot_haz_2_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_2_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_2_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_2 <- downloadHandler(
      filename = function() { paste0('haz_plot_2.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_2, width = as.numeric(input$nma1_psm1_plot_haz_2_width),
               height = as.numeric(input$nma1_psm1_plot_haz_2_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_2<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        # facet_wrap(~Model,nrow=3)+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_2)
      print(f2)
      values$graph_psm1_surv_2<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_2_dw, {
      output$nma1_psm1_plot_surv_2_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_2", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_2_dw, {
      output$nma1_psm1_plot_surv_2_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_2_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_2_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_2 <- downloadHandler(
      filename = function() { paste0('surv_plot_2.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_2, width = as.numeric(input$nma1_psm1_plot_surv_2_width),
               height = as.numeric(input$nma1_psm1_plot_surv_2_height),
               units = "px")
      }
    )
    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm2_run,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Gompertz;","Bayesian;Fixed-effect")
    
    values$temp_psm13<-temp_model_name
    output$output_psm13<-renderUI({
      actionButton("goutput_psm13", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.psm <- jags(model.file = "www/gompertz_f.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm2_pmean, 2)),
                             list(prec2 = diag(rep(input$psm2_pprec, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm2_nchains, n.iter = input$psm2_niter, n.burnin = input$psm2_nburnin, n.thin = input$psm2_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_3 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_3<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_3_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_3_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_3 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    hr_fun0=function(Time){
      y= 0
      return(y)
    }
    hr_fun1=function(i,Time){
      y=df_res3$median[i+1]+Time*df_res3$median[i_trt+i+1]
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm13_hrdata<-df_hr
    output$nma1_psm13_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm13_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm13_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    
    haz_fun0=function(Time){
      y= mu1+Time*mu2
      return(y)
    }
    haz_fun1=function(i,Time){
      y=df_res2$median[i+1]+Time*df_res2$median[i_trt+i+1]
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    
    output$nma1_psm1_plot_hr_3<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_3)
      print(f1)
      values$graph_psm1_hr_3<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_3_dw, {
      output$nma1_psm1_plot_hr_3_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_3", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_3_dw, {
      output$nma1_psm1_plot_hr_3_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_3_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_3_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_3 <- downloadHandler(
      filename = function() { paste0('hr_plot_3.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_3, width = as.numeric(input$nma1_psm1_plot_hr_3_width),
               height = as.numeric(input$nma1_psm1_plot_hr_3_height),
               units = "px")
      }
    )
    
    
    output$nma1_psm1_plot_haz_3<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_3)
      print(f1)
      values$graph_psm1_haz_3<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_3_dw, {
      output$nma1_psm1_plot_haz_3_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_3", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_3_dw, {
      output$nma1_psm1_plot_haz_3_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_3_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_3_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_3 <- downloadHandler(
      filename = function() { paste0('haz_plot_3.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_3, width = as.numeric(input$nma1_psm1_plot_haz_3_width),
               height = as.numeric(input$nma1_psm1_plot_haz_3_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_3<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_3)
      print(f2)
      values$graph_psm1_surv_3<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_3_dw, {
      output$nma1_psm1_plot_surv_3_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_3", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_3_dw, {
      output$nma1_psm1_plot_surv_3_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_3_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_3_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_3 <- downloadHandler(
      filename = function() { paste0('surv_plot_3.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_3, width = as.numeric(input$nma1_psm1_plot_surv_3_width),
               height = as.numeric(input$nma1_psm1_plot_surv_3_height),
               units = "px")
      }
    )
    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm2_runr,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Gompertz;","Bayesian;Random-effects")
    
    values$temp_psm14<-temp_model_name
    output$output_psm14<-renderUI({
      actionButton("goutput_psm14", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    
    fit.psm <- jags(model.file = "www/gompertz_ra.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm2_pmean, 2)),
                             list(prec2 = diag(rep(input$psm2_pprec, 2))),
                             list(R = diag(rep(input$psm2_pr, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm2_nchains, n.iter = input$psm2_niter, n.burnin = input$psm2_nburnin, n.thin = input$psm2_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_4 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_4<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_4_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_4_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_4 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    hr_fun0=function(Time){
      y= 0
      return(y)
    }
    hr_fun1=function(i,Time){
      y=df_res3$median[i+1]+Time*df_res3$median[i_trt+i+1]
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm14_hrdata<-df_hr
    output$nma1_psm14_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm14_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm14_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    haz_fun0=function(Time){
      y= mu1+Time*mu2
      return(y)
    }
    haz_fun1=function(i,Time){
      y=df_res2$median[i+1]+Time*df_res2$median[i_trt+i+1]
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    
    output$nma1_psm1_plot_hr_4<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_4)
      print(f1)
      values$graph_psm1_hr_4<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_4_dw, {
      output$nma1_psm1_plot_hr_4_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_4", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_4_dw, {
      output$nma1_psm1_plot_hr_4_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_4_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_4_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_4 <- downloadHandler(
      filename = function() { paste0('hr_plot_4.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_4, width = as.numeric(input$nma1_psm1_plot_hr_4_width),
               height = as.numeric(input$nma1_psm1_plot_hr_4_height),
               units = "px")
      }
    )
    
    
    output$nma1_psm1_plot_haz_4<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_4)
      print(f1)
      values$graph_psm1_haz_4<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_4_dw, {
      output$nma1_psm1_plot_haz_4_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_4", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_4_dw, {
      output$nma1_psm1_plot_haz_4_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_4_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_4_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_4 <- downloadHandler(
      filename = function() { paste0('haz_plot_4.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_4, width = as.numeric(input$nma1_psm1_plot_haz_4_width),
               height = as.numeric(input$nma1_psm1_plot_haz_4_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_4<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_4)
      print(f2)
      values$graph_psm1_surv_4<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_4_dw, {
      output$nma1_psm1_plot_surv_4_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_4", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_4_dw, {
      output$nma1_psm1_plot_surv_4_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_4_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_4_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_4 <- downloadHandler(
      filename = function() { paste0('surv_plot_4.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_4, width = as.numeric(input$nma1_psm1_plot_surv_4_width),
               height = as.numeric(input$nma1_psm1_plot_surv_4_height),
               units = "px")
      }
    )
    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm3_run,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Log-logistic;","Bayesian;Fixed-effect")
    
    values$temp_psm15<-temp_model_name
    output$output_psm15<-renderUI({
      actionButton("goutput_psm15", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.psm <- jags(model.file = "www/loglogistic_f.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm3_pmean, 2)),
                             list(prec2 = diag(rep(input$psm3_pprec, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm3_nchains, n.iter = input$psm3_niter, n.burnin = input$psm3_nburnin, n.thin = input$psm3_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_5 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_5<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_5_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_5_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_5 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    
    df_res3<-res
    hr_fun0=function(Time){
      y=log((exp(mu2)/exp(mu1))*((Time/exp(mu1))^(exp(mu2)-1))/(1+(Time/exp(mu1))^exp(mu2)))
      return(y)
    }
    hr_fun1=function(i,Time){
      y=log((exp(df_res3$median[i_trt+i+1])/exp(df_res3$median[i+1]))*((Time/exp(df_res3$median[i+1]))^(exp(df_res3$median[i_trt+i+1])-1))/(1+(Time/exp(df_res3$median[i+1]))^exp(df_res3$median[i_trt+i+1])))
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm15_hrdata<-df_hr
    output$nma1_psm15_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm15_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm15_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    
    haz_fun0=function(Time){
      y=log((exp(mu2)/exp(mu1))*((Time/exp(mu1))^(exp(mu2)-1))/(1+(Time/exp(mu1))^exp(mu2)))
      return(y)
    }
    haz_fun1=function(i,Time){
      y=log((exp(df_res2$median[i_trt+i+1])/exp(df_res2$median[i+1]))*((Time/exp(df_res2$median[i+1]))^(exp(df_res2$median[i_trt+i+1])-1))/(1+(Time/exp(df_res2$median[i+1]))^exp(df_res2$median[i_trt+i+1])))
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_psm1_plot_hr_5<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_5)
      print(f1)
      values$graph_psm1_hr_5<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_5_dw, {
      output$nma1_psm1_plot_hr_5_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_5", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_5_dw, {
      output$nma1_psm1_plot_hr_5_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_5_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_5_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_5 <- downloadHandler(
      filename = function() { paste0('hr_plot_5.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_5, width = as.numeric(input$nma1_psm1_plot_hr_5_width),
               height = as.numeric(input$nma1_psm1_plot_hr_5_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_haz_5<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_5)
      print(f1)
      values$graph_psm1_haz_5<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_5_dw, {
      output$nma1_psm1_plot_haz_5_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_5", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_5_dw, {
      output$nma1_psm1_plot_haz_5_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_5_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_5_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_5 <- downloadHandler(
      filename = function() { paste0('haz_plot_5.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_5, width = as.numeric(input$nma1_psm1_plot_haz_5_width),
               height = as.numeric(input$nma1_psm1_plot_haz_5_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_5<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_5)
      print(f2)
      values$graph_psm1_surv_5<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_5_dw, {
      output$nma1_psm1_plot_surv_5_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_5", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_5_dw, {
      output$nma1_psm1_plot_surv_5_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_5_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_5_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_5 <- downloadHandler(
      filename = function() { paste0('surv_plot_5.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_5, width = as.numeric(input$nma1_psm1_plot_surv_5_width),
               height = as.numeric(input$nma1_psm1_plot_surv_5_height),
               units = "px")
      }
    )

    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm3_runr,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Log-logistic;","Bayesian;Random-effects")
    
    values$temp_psm16<-temp_model_name
    output$output_psm16<-renderUI({
      actionButton("goutput_psm16", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    
    fit.psm <- jags(model.file = "www/loglogistic_ra.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm3_pmean, 2)),
                             list(prec2 = diag(rep(input$psm3_pprec, 2))),
                             list(R = diag(rep(input$psm3_pr, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm3_nchains, n.iter = input$psm3_niter, n.burnin = input$psm3_nburnin, n.thin = input$psm3_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_6 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_6<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_6_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_6_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_6 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    hr_fun0=function(Time){
      y=log((exp(mu2)/exp(mu1))*((Time/exp(mu1))^(exp(mu2)-1))/(1+(Time/exp(mu1))^exp(mu2)))
      return(y)
    }
    hr_fun1=function(i,Time){
      y=log((exp(df_res3$median[i_trt+i+1])/exp(df_res3$median[i+1]))*((Time/exp(df_res3$median[i+1]))^(exp(df_res3$median[i_trt+i+1])-1))/(1+(Time/exp(df_res3$median[i+1]))^exp(df_res3$median[i_trt+i+1])))
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm16_hrdata<-df_hr
    output$nma1_psm16_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm16_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm16_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    haz_fun0=function(Time){
      y=log((exp(mu2)/exp(mu1))*((Time/exp(mu1))^(exp(mu2)-1))/(1+(Time/exp(mu1))^exp(mu2)))
      return(y)
    }
    haz_fun1=function(i,Time){
      y=log((exp(df_res2$median[i_trt+i+1])/exp(df_res2$median[i+1]))*((Time/exp(df_res2$median[i+1]))^(exp(df_res2$median[i_trt+i+1])-1))/(1+(Time/exp(df_res2$median[i+1]))^exp(df_res2$median[i_trt+i+1])))
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_psm1_plot_hr_6<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_6)
      print(f1)
      values$graph_psm1_hr_6<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_6_dw, {
      output$nma1_psm1_plot_hr_6_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_6", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_6_dw, {
      output$nma1_psm1_plot_hr_6_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_6_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_6_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_6 <- downloadHandler(
      filename = function() { paste0('hr_plot_6.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_6, width = as.numeric(input$nma1_psm1_plot_hr_6_width),
               height = as.numeric(input$nma1_psm1_plot_hr_6_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_haz_6<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_6)
      print(f1)
      values$graph_psm1_haz_6<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_6_dw, {
      output$nma1_psm1_plot_haz_6_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_6", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_6_dw, {
      output$nma1_psm1_plot_haz_6_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_6_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_6_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_6 <- downloadHandler(
      filename = function() { paste0('haz_plot_6.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_6, width = as.numeric(input$nma1_psm1_plot_haz_6_width),
               height = as.numeric(input$nma1_psm1_plot_haz_6_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_6<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_6)
      print(f2)
      values$graph_psm1_surv_6<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_6_dw, {
      output$nma1_psm1_plot_surv_6_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_6", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_6_dw, {
      output$nma1_psm1_plot_surv_6_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_6_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_6_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_6 <- downloadHandler(
      filename = function() { paste0('surv_plot_6.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_6, width = as.numeric(input$nma1_psm1_plot_surv_6_width),
               height = as.numeric(input$nma1_psm1_plot_surv_6_height),
               units = "px")
      }
    )
    
    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm4_run,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Log-normal;","Bayesian;Fixed-effect")
    
    values$temp_psm17<-temp_model_name
    output$output_psm17<-renderUI({
      actionButton("goutput_psm17", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.psm <- jags(model.file = "www/lognormal_f.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm4_pmean, 2)),
                             list(prec2 = diag(rep(input$psm4_pprec, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm4_nchains, n.iter = input$psm4_niter, n.burnin = input$psm4_nburnin, n.thin = input$psm4_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2)
    
    output$nma1_psm1_fix_DIC_7 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_7<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_7_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_7_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_7 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    hr_fun0=function(Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-mu1)/exp(mu2))^2)*0.5) / (exp(mu2)*Time*pnorm(-(log(Time)-mu1)/exp(mu2))))
      return(y)
    }
    hr_fun1=function(i,Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-df_res3$median[i+1])/exp(df_res3$median[i_trt+i+1]))^2)*0.5) / (exp(df_res3$median[i_trt+i+1])*Time*pnorm(-(log(Time)-df_res3$median[i+1])/exp(df_res3$median[i_trt+i+1]))))
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm17_hrdata<-df_hr
    output$nma1_psm17_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm17_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm17_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    haz_fun0=function(Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-mu1)/exp(mu2))^2)*0.5) / (exp(mu2)*Time*pnorm(-(log(Time)-mu1)/exp(mu2))))
      return(y)
    }
    haz_fun1=function(i,Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-df_res2$median[i+1])/exp(df_res2$median[i_trt+i+1]))^2)*0.5) / (exp(df_res2$median[i_trt+i+1])*Time*pnorm(-(log(Time)-df_res2$median[i+1])/exp(df_res2$median[i_trt+i+1]))))
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_psm1_plot_hr_7<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_7)
      print(f1)
      values$graph_psm1_hr_7<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_7_dw, {
      output$nma1_psm1_plot_hr_7_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_7", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_7_dw, {
      output$nma1_psm1_plot_hr_7_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_7_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_7_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_7 <- downloadHandler(
      filename = function() { paste0('hr_plot_7.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_7, width = as.numeric(input$nma1_psm1_plot_hr_7_width),
               height = as.numeric(input$nma1_psm1_plot_hr_7_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_haz_7<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_7)
      print(f1)
      values$graph_psm1_haz_7<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_7_dw, {
      output$nma1_psm1_plot_haz_7_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_7", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_7_dw, {
      output$nma1_psm1_plot_haz_7_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_7_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_7_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_7 <- downloadHandler(
      filename = function() { paste0('haz_plot_7.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_7, width = as.numeric(input$nma1_psm1_plot_haz_7_width),
               height = as.numeric(input$nma1_psm1_plot_haz_7_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_7<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_7)
      print(f2)
      values$graph_psm1_surv_7<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_7_dw, {
      output$nma1_psm1_plot_surv_7_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_7", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_7_dw, {
      output$nma1_psm1_plot_surv_7_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_7_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_7_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_7 <- downloadHandler(
      filename = function() { paste0('surv_plot_7.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_7, width = as.numeric(input$nma1_psm1_plot_surv_7_width),
               height = as.numeric(input$nma1_psm1_plot_surv_7_height),
               units = "px")
      }
    )
    
    rm(fit.psm)
  })
  
  observeEvent(input$nma1_psm4_runr,{
    follow_up<-input$nma1_psm_ex
    ref.study <- input$nma1_psm_refs
    ref.trt   <- input$nma1_psm_reft
    km<-values$data1
    Treatment<-values$nma1_trt
    Study<-values$nma1_stu
    km$studyf <- factor(km$studyn, labels = Study$Study_name)
    km$trtf   <- factor(km$trtn, labels = Treatment$Treatment_name)
    km$studyf <- relevel(km$studyf, ref=ref.study)
    km$trtf <- relevel(km$trtf, ref=ref.trt)
    km$trtn=as.numeric(km$trtf)
    km$studyn=as.numeric(km$studyf)
    
    temp_model_name<-paste0("PSM;","Log-normal;","Bayesian;Random-effects")
    
    values$temp_psm18<-temp_model_name
    output$output_psm18<-renderUI({
      actionButton("goutput_psm18", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    d_arms <- km %>% 
      group_by(studyn, trtn) %>%
      slice(1) %>%
      group_by(studyn) %>%
      dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
      select(studyf, trtf, studyn, trtn, arm, n_arms)
    
    d_std <- d_arms %>%
      group_by(studyn) %>%
      select(studyn, n_arms) %>%
      slice(1)
    
    dat <- km %>%
      left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))
    
    d_trts <- dat %>%
      mutate(studyn.arm = interaction(studyn, arm)) %>%
      filter(!duplicated(studyn.arm)) %>%
      select(studyn, arm, trtn) %>%
      arrange(studyn, arm) %>%
      tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
    
    Nobs <- nrow(dat)
    ### add
    dat$ref<-dat$trtn
    for (i in 1:length(dat$studyn)) {
      for (j in 1:length(d_trts$studyn)) {
        if (dat$studyn[i]==d_trts$studyn[j] & dat$trtn[i]==d_trts$`2`[j]) {
          dat$ref[i]=d_trts$`1`[j]
        }
      }
    }
    
    trts<-as.data.frame(select(ungroup(d_trts), -studyn))
    colnames(trts)<-c("bs","ks")
    
    #Data list for jags fit
    dat_jg <- list(
      Nobs = Nobs,
      Ns = nrow(d_std),
      r = dat$nevents,
      n = dat$natrisk,
      time = dat$time,
      dt = dat$timeDelta,
      s = dat$studyn,
      k=dat$trtn,
      b=dat$ref,
      bs=trts$bs,
      ks=trts$ks,
      Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
    )
    
    # set.seed(9487396)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    fit.psm <- jags(model.file = "www/lognormal_ra.txt", 
                    data = c(dat_jg,
                             list(mean = rep(input$psm4_pmean, 2)),
                             list(prec2 = diag(rep(input$psm4_pprec, 2))),
                             list(R = diag(rep(input$psm4_pr, 2)))),
                    parameters = c("d", "mu", "Beta"),
                    n.chains = input$psm4_nchains, n.iter = input$psm4_niter, n.burnin = input$psm4_nburnin, n.thin = input$psm4_nthin)
    
    progress$close()
    removeModal()
    
    # REPORT dic
    pD<-round(fit.psm$BUGSoutput$pV,2)
    DIC<-round(fit.psm$BUGSoutput$DIC,2) 
    
    output$nma1_psm1_fix_DIC_8 <- renderText(paste(paste0("pD=",pD),paste0("DIC=",DIC),sep = ";"))
    
    res <- fit.psm
    res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7,8)])
    res <- res[grep("d\\[",rownames(res)),]
    res <- round(res,3)
    names(res) <- c("median","lower","upper","Rhat")
    res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
    res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
    res <- res[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_8<-res
    
    res_mu0 <- fit.psm
    res_mu0 <- as.data.frame(res_mu0$BUGSoutput$summary[,c(5,3,7,8)])
    res_mu0<-res_mu0[grep("mu\\[",rownames(res_mu0)),]
    res_mu0 <- round(res_mu0,3)
    names(res_mu0) <- c("median","lower","upper","Rhat")
    res_mu0$ci <- paste("(",res_mu0$lower,",",res_mu0$upper,")",sep="")
    res_mu0$study<-rep(levels(km$studyf),length.out = nrow(res_mu0))
    res_mu0 <- res_mu0[,c(6,1,5,4)]
    values$nma1_psm1_coeff.data_8_mu<-res_mu0
    
    output$nma1_psm1_fix_coeff_8_mu <- renderDT(datatable(res_mu0, editable = FALSE, rownames = TRUE,
                                                          options = list(pageLength = 10)
    ))
    
    output$nma1_psm1_fix_coeff_8 <- renderDT(datatable(res, editable = FALSE, rownames = TRUE,
                                                       options = list(pageLength = 10)
    ))
    
    res_mu <- fit.psm
    res_mu <- as.data.frame(res_mu$BUGSoutput$summary[,c(5)])
    res_mu<-res_mu[grep("mu\\[",rownames(res_mu)),]
    res_mu <- round(res_mu,3)
    res_mu1<-res_mu[1:length(d_trts$studyn)]
    res_mu2<-res_mu[(length(d_trts$studyn)+1):(2*length(d_trts$studyn))]
    
    index<-mu1<-mu2<-0
    for (i in 1:length(d_trts$studyn)) {
      if (d_trts$`1`[i] == 1){
        mu1<-res_mu1[i]+mu1
        mu2<-res_mu2[i]+mu2
        index<-index+1
      }
    }
    mu1<-mu1/index
    mu2<-mu2/index
    
    shinyalert(title = "Complete!", type = "success")
    
    i_trt<-nlevels(d_arms$trtf)
    
    df_res2<-res
    df_res2$median[2:i_trt]<-df_res2$median[2:i_trt]+mu1
    df_res2$median[(i_trt+2):(2*i_trt)]<-df_res2$median[(i_trt+2):(2*i_trt)]+mu2
    
    df_res3<-res
    hr_fun0=function(Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-mu1)/exp(mu2))^2)*0.5) / (exp(mu2)*Time*pnorm(-(log(Time)-mu1)/exp(mu2))))
      return(y)
    }
    hr_fun1=function(i,Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-df_res3$median[i+1])/exp(df_res3$median[i_trt+i+1]))^2)*0.5) / (exp(df_res3$median[i_trt+i+1])*Time*pnorm(-(log(Time)-df_res3$median[i+1])/exp(df_res3$median[i_trt+i+1]))))
      return(y)
    }
    
    df_hr<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_hr)<-c("time",unique(res$comparison))
    df_hr$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_hr$time)) {
      df_hr[j,2]<-exp(hr_fun0(df_hr$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_hr$time)) {
        df_hr[j,i+2]<-exp(hr_fun1(i,df_hr$time[j]))
      }
    }
    
    values$nma1_psm18_hrdata<-df_hr
    output$nma1_psm18_hrdata_dw <- downloadHandler(
      filename = function() {
        paste("nma1_psm18_hrdata", ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$nma1_psm18_hrdata, file,row.names = TRUE)
      }
    )
    
    df_hr<-df_hr[,-2]
    dfFig_hr = df_hr %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    
    haz_fun0=function(Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-mu1)/exp(mu2))^2)*0.5) / (exp(mu2)*Time*pnorm(-(log(Time)-mu1)/exp(mu2))))
      return(y)
    }
    haz_fun1=function(i,Time){
      y=log(((2*3.1415926)^(-0.5))*exp(-(((log(Time)-df_res2$median[i+1])/exp(df_res2$median[i_trt+i+1]))^2)*0.5) / (exp(df_res2$median[i_trt+i+1])*Time*pnorm(-(log(Time)-df_res2$median[i+1])/exp(df_res2$median[i_trt+i+1]))))
      return(y)
    }
    
    df_haz<-as.data.frame(matrix(ncol = (nlevels(d_arms$trtf)+1),nrow = follow_up*12))
    colnames(df_haz)<-c("time",levels(d_arms$trtf))
    df_haz$time<-seq(1,follow_up*12,1)
    for (j in 1:length(df_haz$time)) {
      df_haz[j,2]<-exp(haz_fun0(df_haz$time[j]))
    }
    for (i in 1:(i_trt-1)) {
      for (j in 1:length(df_haz$time)) {
        df_haz[j,i+2]<-exp(haz_fun1(i,df_haz$time[j]))
      }
    }
    
    df_surv<-df_haz
    for ( i in 1:i_trt ) {
      dftemp<-data.frame(df_haz$time,df_haz[[(i+1)]])
      dftemp<-dftemp %>%
        dplyr::arrange(dftemp[[1]]) %>%
        dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>%
        dplyr::mutate(survProp = exp(-1*cumhaz))
      df_surv[[i+1]]<-dftemp[[4]]
    }
    colnames(df_surv)<-colnames(df_haz)
    df_surv<-rbind(c(0,rep(1,(ncol(df_surv)-1))),df_surv)
    
    dfFig_haz = df_haz %>%
      gather(key = "trtf", value = "hr", -time) %>% mutate(trtf = factor(trtf))
    dfFig_surv = df_surv %>%
      gather(key = "trtf", value = "surv", -time) %>% mutate(trtf = factor(trtf))
    
    output$nma1_psm1_plot_hr_8<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_hr,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        geom_hline(yintercept=1, lty=2) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard Ratio") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_hr_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_hr_psm, input$y_max_hr_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_hr_8)
      print(f1)
      values$graph_psm1_hr_8<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_hr_8_dw, {
      output$nma1_psm1_plot_hr_8_titlebut <- renderUI({
        textInput("ggtitle_psm1_hr_8", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_8_dw, {
      output$nma1_psm1_plot_hr_8_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_hr_8_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_hr_8_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_hr_8 <- downloadHandler(
      filename = function() { paste0('hr_plot_8.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_hr_8, width = as.numeric(input$nma1_psm1_plot_hr_8_width),
               height = as.numeric(input$nma1_psm1_plot_hr_8_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_haz_8<-renderPlot({
      f1<-ggplot() +
        geom_line(data=dfFig_haz,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Log Hazard") +
        xlab("Time(months)") +
        theme(legend.position = "bottom") +
        theme_bw()
      f1
      if (input$y_axis_haz_psm==TRUE){
        f1<-f1+scale_y_log10(limits = c(input$y_min_haz_psm, input$y_max_haz_psm))
      }else {f1<-f1+scale_y_log10()}
      f1<-f1 + ggtitle(input$ggtitle_psm1_haz_8)
      print(f1)
      values$graph_psm1_haz_8<-f1
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_haz_8_dw, {
      output$nma1_psm1_plot_haz_8_titlebut <- renderUI({
        textInput("ggtitle_psm1_haz_8", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_8_dw, {
      output$nma1_psm1_plot_haz_8_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_haz_8_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_haz_8_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_haz_8 <- downloadHandler(
      filename = function() { paste0('haz_plot_8.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_haz_8, width = as.numeric(input$nma1_psm1_plot_haz_8_width),
               height = as.numeric(input$nma1_psm1_plot_haz_8_height),
               units = "px")
      }
    )
    
    output$nma1_psm1_plot_surv_8<-renderPlot({
      f2= ggplot() +
        geom_line(data=dfFig_surv, aes(x=time, y=surv, group=trtf, colour=trtf), size=1) +
        scale_color_discrete(name="Treatment")+
        expand_limits(y=c(0,1),x=c(0,follow_up*12)) +
        scale_x_continuous(breaks = c(seq(from=0, to=follow_up*12,by = 12))) +
        ylab("Proportion surviving") +
        xlab("Time(months)") +
        guides(color = guide_legend(ncol = 1))  +
        theme(legend.position = "bottom") +
        theme_bw()
      f2
      f2<-f2 + ggtitle(input$ggtitle_psm1_surv_8)
      print(f2)
      values$graph_psm1_surv_8<-f2
    })
    
    ### download plot
    observeEvent(input$nma1_psm1_plot_surv_8_dw, {
      output$nma1_psm1_plot_surv_8_titlebut <- renderUI({
        textInput("ggtitle_psm1_surv_8", "Title", value = input$tipo)
      })
    })
    
    # Buttons to customize and download the plot (h and w)
    observeEvent(input$nma1_psm1_plot_surv_8_dw, {
      output$nma1_psm1_plot_surv_8_sizebut <- renderUI({
        bt <- tagList()
        bt[[1]] <- numericInput("nma1_psm1_plot_surv_8_height", "Height (px)", value = 1600)
        bt[[2]] <- numericInput("nma1_psm1_plot_surv_8_width", "Width (px)", value = 2800)
        bt
      })
    })
    
    # Download back-end
    output$downloadPlot_nma1_psm1_plot_surv_8 <- downloadHandler(
      filename = function() { paste0('surv_plot_8.png') },
      content = function(file) {
        ggsave(file,values$graph_psm1_surv_8, width = as.numeric(input$nma1_psm1_plot_surv_8_width),
               height = as.numeric(input$nma1_psm1_plot_surv_8_height),
               units = "px")
      }
    )
    
    rm(fit.psm)
  })
  
  output$copy_nma1_psm1_fix_coeff_1 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_1", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_1, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_2 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_2", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_2, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_3 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_3", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_3, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_4 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_4", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_4, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_5 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_5", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_5, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_6 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_6", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_6, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_7 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_7", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_7, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_8 <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_8", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_8, file,row.names = TRUE)
    }
  )
  
  
  output$copy_nma1_psm1_fix_coeff_1_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_1_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_1_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_2_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_2_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_2_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_3_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_3_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_3_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_4_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_4_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_4_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_5_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_5_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_5_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_6_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_6_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_6_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_7_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_7_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_7_mu, file,row.names = TRUE)
    }
  )
  
  output$copy_nma1_psm1_fix_coeff_8_mu <- downloadHandler(
    filename = function() {
      paste("nma1_psm1_fix_coeff_8_mu", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$nma1_psm1_coeff.data_8_mu, file,row.names = TRUE)
    }
  )
  
  ##### end of PSM #####
  
  ##### COX PH #####
  # observeEvent(input$run_nma_coxph, {
  #   updateTabsetPanel(session, "subset_cph_panel", selected ="1.2 Show the COX PH results")
  # }) 
  
  observeEvent(input$run_nma_coxph,{
    data<-values$data2
    Treatment<-values$nma2_trt
    Study<-values$nma2_stu
    
    temp_model_name<-paste0("Cox-PH Model;","Bayesian;Fixed-effect")
    
    values$temp_cph<-temp_model_name
    output$output_cph<-renderUI({
      actionButton("goutput_cph", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    
    df_cox_ph<-list()
    res_cox_ph<-list()
    temp_row<-length(Study$Study_name[Study$Study_arm==2])+length(Study$Study_name[Study$Study_arm==3])*2
    
    ns2<-length(Study$Study_name[Study$Study_arm==2])
    ns3<-length(Study$Study_name[Study$Study_arm==3])
    
    hr_data<-as.data.frame(matrix(ncol = 7,nrow = temp_row))
    colnames(hr_data)<-c("study","treat1","treat2","hr","se","lhr","V")
    
    for (i in 1:length(Study$Study_arm)) {
      df_cox_ph[[i]] = data[data$studyCode==i,]
    }
    index=1
    for (i in 1:length(Study$Study_arm)) {
      if (Study$Study_arm[i]==2) {
        res_cox_ph[[i]] = broom::tidy(coxph(formula = Surv(time, event) ~ arm, data=df_cox_ph[[i]]), exp=T)
        hr_data$study[index]<-Study$Study_name[Study$Study_code==i]
        hr_data$treat1[index]<-unique(data$treatment[data$study==Study$Study_name[i]])[1]
        hr_data$treat2[index]<-unique(data$treatment[data$study==Study$Study_name[i]])[2]
        hr_data$hr[index]<-res_cox_ph[[i]]$estimate
        hr_data$se[index]<-res_cox_ph[[i]]$std.error
        hr_data$lhr[index]<-log(hr_data$hr[index])
        hr_data$V[index]<-NA
        index=index+1
      } else if (Study$Study_arm[i]==3) {
        df_cox_ph[[i]]$treat <- factor(df_cox_ph[[i]]$arm, labels=c("A", "B", "C"))
        res_cox_ph[[i]] <- broom::tidy(coxph(formula = Surv(time, event) ~ treat, data=df_cox_ph[[i]]), exp=T)
      }
    }
    
    if (length(Study$Study_name[Study$Study_arm==3]) != 0){
      for (j in 1:length(Study$Study_name[Study$Study_arm==3])) {
        i=as.numeric(Study$Study_code[Study$Study_arm==3][j])
        hr_data$study[index]<-Study$Study_name[Study$Study_code==i]
        hr_data$study[index+1]<-Study$Study_name[Study$Study_code==i]
        hr_data$treat1[index]<-unique(data$treatment[data$study==Study$Study_name[i]])[1]
        hr_data$treat2[index]<-unique(data$treatment[data$study==Study$Study_name[i]])[2]
        hr_data$treat1[index+1]<-unique(data$treatment[data$study==Study$Study_name[i]])[1]
        hr_data$treat2[index+1]<-unique(data$treatment[data$study==Study$Study_name[i]])[3]
        hr_data$hr[index]<-res_cox_ph[[i]]$estimate[1]
        hr_data$se[index]<-res_cox_ph[[i]]$std.error[1]
        hr_data$lhr[index]<-log(hr_data$hr[index])
        hr_data$hr[index+1]<-res_cox_ph[[i]]$estimate[2]
        hr_data$se[index+1]<-res_cox_ph[[i]]$std.error[2]
        hr_data$lhr[index+1]<-log(hr_data$hr[index+1])
        
        sd1 <- sd(df_cox_ph[[i]]$time[df_cox_ph[[i]]$txCode==Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat1[index]]])
        sd2 <- sd(df_cox_ph[[i]]$time[df_cox_ph[[i]]$txCode==Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat2[index]]])
        n1 <- length(df_cox_ph[[i]]$time[df_cox_ph[[i]]$txCode==Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat1[index]]])
        n2 <- length(df_cox_ph[[i]]$time[df_cox_ph[[i]]$txCode==Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat2[index]]])
        
        swithin <- (((n1-1)*sd1^2)+((n2-1)*sd2^2))/((n1+n2-2)^0.5)
        V <- (sd1^2/n1) / (swithin^2)
        
        hr_data$V[index:(index+1)] <- V
        
        index=index+2
      }
    }
    
    for (i in 1:length(hr_data$treat1)) {
      hr_data$treat1[i]=Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat1[i]]
      hr_data$treat2[i]=Treatment$Treatment_code[Treatment$Treatment_name==hr_data$treat2[i]] 
    }
    
    data_wide <- data.frame(study=hr_data$study)
    data_wide$t1 <- as.numeric(hr_data$treat1)
    data_wide$t2 <- as.numeric(hr_data$treat2)
    data_wide$t3 <- NA
    data_wide$y2 <- hr_data$lhr
    data_wide$y3 <- NA
    data_wide$se2 <- hr_data$se
    data_wide$se3 <- NA
    data_wide$V <- hr_data$V
    
    if (length(Study$Study_name[Study$Study_arm==3]) != 0){
      for (i in 1:(length(Study$Study_name[Study$Study_arm==3]))) {
        index0=length(Study$Study_name[Study$Study_arm==2])+2*(i-1)+1
        data_wide$t3[index0] <- hr_data$treat2[index0+1]
        data_wide$y3[index0] <- hr_data$lhr[index0+1]
        data_wide$se3[index0] <- hr_data$se[index0+1]
        data_wide <- data_wide[-(index0+1),]
      }
    }
    
    hr_data0 <- data_wide
    hr_data0$y1 <- 0
    hr_data0$se1 <- 0
    
    hr_data0$na <- 2
    if (length(Study$Study_name[Study$Study_arm==3]) != 0){
      for (i in 1:(length(Study$Study_name[Study$Study_arm==3]))) {
        index0=length(Study$Study_name[Study$Study_arm==2])+i
        hr_data0$na[index0] <- 3
      }
    }
    
    # WinBUGS burn-in & simulation size
    num.sims <- input$cph_sim
    burn.in <- input$cph_nburnin
    ####
    
    # No. of studies
    ns <- nrow(hr_data0)
    
    # No. of treatments
    nt <- max(hr_data0$t2)
    
    y <- array(c(hr_data0$y1, hr_data0$y2, hr_data0$y3), dim=c(ns,3))
    se <- array(c(hr_data0$se1, hr_data0$se2, hr_data0$se3), dim=c(ns,3))
    t <- array(c(as.numeric(hr_data0$t1), as.numeric(hr_data0$t2), as.numeric(hr_data0$t3)), dim=c(ns,3))
    
    bugs_hr_data0 <- list(ns2=ns2, ns3=ns3, nt=nt, t= t, y=y, se=se, na=hr_data0$na, V=hr_data0$V)
    
    #-----------------------------------------------------------------------------
    # Initial values
    #-----------------------------------------------------------------------------
    
    d1 <- c(NA, rep(input$cph_d1,nt-1))
    d2 <- c(NA, rep(input$cph_d2,nt-1))
    d3 <- c(NA, rep(input$cph_d3,nt-1))
    
    fe_inits <- list(list(d=d1), 
                     list(d=d2),
                     list(d=d3))
    
    # code_cph<-input$nma1_cph_work_d
    
    #-----------------------------------------------------------------------------
    # Fit FE model in WinBUGS
    #-----------------------------------------------------------------------------
    
    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    # set.seed(385916)
    bugs.fe <- jags(model.file = "www/FE_model_cph.txt", 
                    data = bugs_hr_data0,
                    inits = fe_inits,
                    parameters.to.save = c("d", "hrd"),
                    n.chains = 3, n.iter = (num.sims+burn.in), n.burnin = burn.in, n.thin = 3) 
    
    progress$close()
    removeModal()
    
    fe_results <- bugs.fe$BUGSoutput$summary
    res <- fe_results
    res_hr <- as.data.frame(fe_results[,c(5,3,7,8)])
    res_hr <- res_hr[grep("hrd\\[",rownames(res_hr)),]
    res_hr <- round(res_hr,3)
    names(res_hr) <- c("median","lower","upper","Rhat")
    res_hr$ci <- paste("(",res_hr$lower,",",res_hr$upper,")",sep="")
    res_hr$comparison<-paste(Treatment$Treatment_name[-1],Treatment$Treatment_name[1],sep=" vs ")
    res_hr <- res_hr[,c(6,1,5,4)]
    values$copy_df_nma2_res_hr<-res_hr
    output$df_nma2_res_hr <- renderDT(datatable(res_hr, editable = FALSE, rownames = FALSE, 
                                                options = list(pageLength = 10)
    ))
    
    shinyalert(title = "Complete!", type = "success")
  
  })
  
  output$copy_output.df_nma2_res_hr <- downloadHandler(
    filename = function() {
      paste("df_nma2_res_hr", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$copy_df_nma2_res_hr, file,row.names = TRUE)
    }
  )
  
  # observe({
  #   infile <- input$upload2
  #   if (!is.null(infile)){
  #     tryCatch({
  #       values$sheet3 <- read_excel(infile$datapath, sheet = 1)[,1:7]
  #     },
  #     error = function(e)
  #       shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
  #     )
  #   }
  # })
  # 
  # observeEvent(input$FCexample2, {
  #   # Load example file
  #   wd <- getwd()
  #   infile <- c()
  #   infile$datapath <- paste0(wd,"/www/files/example2.xlsx")
  #   
  #   if (!is.null(infile)){
  #     tryCatch({
  #       values$sheet3 <- read_excel(infile$datapath, sheet = 1)[,1:7]
  #     },
  #     error = function(e)
  #       shinyalert("Warning!", "There is one or more errors in the Excel file. Please modify the data and re-import it.", type = "error")
  #     )
  #   }
  # })
  
  ##### end of cox ph ####
  
  ##### GenGamma #####
  # observeEvent(input$run_nma_gengamma, {
  #   updateTabsetPanel(session, "subset_gengamma_panel", selected ="2.2 Show the Gen-Gamma results")
  # }) 
  
  observeEvent(input$run_nma_gengamma,{
    data<-values$data2
    Treatment<-values$nma2_trt
    Study<-values$nma2_stu
    DF<-data
    selected <- data.frame(trt=DF$treatment, study=DF$study, time=DF$time, event=DF$event, 
                           studyCode=DF$studyCode, txCode=DF$txCode)
    # selected$trt <- as.factor(selected$trt)
    # selected$study <- as.factor(selected$study)
    Studies <- Study$Study_name
    Treat <- Treatment$Treatment_name
    nStudies<-length(Studies)
    nTreat<-length(Treat)
    maxFU<-round(max(selected$time),-1)
    follow_up<-input$nma1_gengamma_ex
    
    temp_model_name<-paste0("Generalised Gamma Model;","Bayesian;Fixed-effect")
    
    values$temp_gg<-temp_model_name
    output$output_gg<-renderUI({
      actionButton("goutput_gg", label="Export the results of this page",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
    })
    # Create data set for winbugs NMA
    study_GG<-rep(NA, nStudies)
    for (i in 1:nStudies){
      data_i<-selected[selected$study==Studies[i],]
      data_i$txCode<-factor(data_i$txCode)
      nmastud<-flexsurvreg(formula=Surv(time,event)~txCode,
                           data=data_i, dist="gengamma", method="BFGS")
      labels<-paste("txCode",levels(data_i$txCode)[2:length(levels(data_i$txCode))],
                    sep="")
      contrasts<-length(labels)
      beta<-nmastud$coefficients[labels]
      se<-sqrt(diag(nmastud$cov))[labels]
      cov<-NA
      if (contrasts>1) {cov<-sqrt(nmastud$cov[labels,labels][1,2])}
      study_i<-data.frame(STUDY=rep(Studies[i],contrasts),
                          COMP=gsub("txCode","",labels),
                          REF=rep(levels(data_i$txCode)[1],contrasts),
                          MEAN=beta,
                          MEANSE=se,
                          COV=cov)
      study_GG<-rbind(study_GG,study_i)
    }
    study_GG<-study_GG[-1,]
    
    for (i in 1:length(study_GG$STUDY)) {
      study_GG$COMP[i]<-Treatment$Treatment_name[Treatment$Treatment_code==study_GG$COMP[i]]
      study_GG$REF[i]<-Treatment$Treatment_name[Treatment$Treatment_code==study_GG$REF[i]]
    }
    
    study_GG$multi<-0
    multi_index<-study_GG$STUDY%in%rownames(table(study_GG$STUDY))[table(study_GG$STUDY)>1]
    
    if (TRUE %in% multi_index) {
      study_GG[multi_index,]$multi<-1
      study_GG[multi_index,"MEANSE"]<-sqrt(study_GG[multi_index,"MEANSE"]^2-study_GG[multi_index,"COV"]^2)
      multi_study<-unique(study_GG$STUDY[study_GG$multi==1])
      multi_first<-match(multi_study,study_GG$STUDY)
      study_GG<-rbind(study_GG,
                      data.frame(STUDY=multi_study,
                                 COMP=study_GG$REF[multi_first],
                                 REF=study_GG$REF[multi_first],
                                 MEAN=rep(0,length(multi_study)),
                                 MEANSE=study_GG$COV[multi_first],
                                 COV=rep(NA,length(multi_study)),
                                 multi=rep(1,length(multi_study))
                      )   
      )
    }
    
    row.names(study_GG)<-NULL
    study_GG$studyCode <- as.factor(study_GG$STUDY)
    

    
    #-----------------------------------------------------------------------------
    # Prepare data for analysis in WinBUGS
    #-----------------------------------------------------------------------------
    
    #Use the list command to group together data needed for WinBUGS
    # LnObs - number of rows of data
    # nTx - number of treatments in network
    # nStudies - number of studies in network
    # Lstudy - study ID number
    # Ltx - comparison treatment
    # Lbase - reference treatment
    # Lmean - mean treatment effect
    # Lse - mean SE
    # multi - indicator for whether row belongs to a multi-arm trial

    #####
    #######
    #####
    #######
    # original_wd <- getwd()
    # bugs.directory <- input$nma1_geng_bugs_d

    # setwd(bugs.path)
    num.sims <- input$geng_sim
    burn.in <- input$geng_nburnin
    
    BUGS_data<-list(LnObs=dim(study_GG)[1], nTx=nTreat, nStudies=nStudies, Lstudy=as.numeric(study_GG$studyCode),
                    Ltx=match(study_GG$COMP,Treat), Lbase=match(study_GG$REF,Treat),
                    Lmean=study_GG[,"MEAN"], Lse=study_GG[,"MEANSE"], multi=study_GG$multi)
    
    # Set up initial values for FE model
    initsFE1<-list(alpha = rep(input$geng_d1,nStudies), beta = c(NA,rep(input$geng_d1,nTreat-1)))
    initsFE2<-list(alpha = rep(input$geng_d2,nStudies), beta = c(NA,rep(input$geng_d2,nTreat-1)))
    initsFE3<-list(alpha = rep(input$geng_d3,nStudies), beta = c(NA,rep(input$geng_d3,nTreat-1)))
    initsFE<-list(initsFE1, initsFE2, initsFE3)
    
    # code_gg<-input$nma1_geng_work_d
    
    
    #-----------------------------------------------------------------------------
    # Fit FE model in WinBUGS
    #-----------------------------------------------------------------------------

    progress <- shiny::Progress$new()
    progress$set(message = "Processing",
                 detail = "This may take a while...")
    showModal(modalDialog(
      title = "Running JAGS",
      "Please wait while running the model...",
      footer = NULL
    ))
    
    # set.seed(9487397)
    bugs.fe <- jags(model.file = "www/FE_model_gg.txt", 
                   data = BUGS_data,
                   inits = initsFE,
                   parameters.to.save = c("beta", "aft"),
                   n.chains = 3, n.iter = (num.sims+burn.in), n.burnin = burn.in, n.thin = 3) 

    progress$close()
    removeModal()
    
    fe_results <- bugs.fe$BUGSoutput$summary
    res <- fe_results
    res_hr <- as.data.frame(fe_results[,c(5,3,7,8)])
    res_hr <- res_hr[grep("beta\\[",rownames(res_hr)),]
    res_hr <- round(res_hr,3)
    names(res_hr) <- c("median","lower","upper","Rhat")
    res_hr$ci <- paste("(",res_hr$lower,",",res_hr$upper,")",sep="")
    # res_hr<-res_hr[-1,]
    # res_hr <- as.data.frame(res_hr[,1:2])
    res_hr$comparison<-paste(Treatment$Treatment_name,Treatment$Treatment_name[1],sep=" vs ")
    res_hr <- res_hr[,c(6,1,5,4)]
    values$copy_df_nma3_res_hr<-res_hr
    output$df_nma3_res_hr <- renderDT(datatable(res_hr, editable = FALSE, rownames = FALSE,
                                                options = list(pageLength = 10)
    ))
    
    shinyalert(title = "Complete!", type = "success")
    
    # calculate the hr 
    df <- data[data$study==Study$Study_name[1] & data$treatment==Treatment$Treatment_name[1],]
    ma <- flexsurvreg(formula=Surv(time,event)~1,
                      data=df, dist="gengamma", method="BFGS")
    mu <- ma$coefficients["mu"]
    sigma <- exp(ma$coefficients["sigma"])
    q <- ma$coefficients["Q"]
    x <- seq(0,follow_up*12,1)

    hr_gengamma <- as.data.frame(matrix(nrow = length(x),ncol = (length(Treatment$Treatment_name)+1)))
    colnames(hr_gengamma) <- c("Time (Month)",Treatment$Treatment_name)
    hr_gengamma[,1]<-x
    hazard_gengamma<-hgengamma(x, mu = mu , sigma = sigma, Q=q)
    
    for (i in 1:length(Treatment$Treatment_name)) {
      hazard_temp = hgengamma(x, mu = mu + res_hr$median[i], sigma = sigma, Q=q)
      hr_gengamma[i+1] = round(hazard_temp/hazard_gengamma,3)
    }
    
    values$df_gengamma_hr<-hr_gengamma
    
    output$df_gengamma_hr <- renderDT(datatable(hr_gengamma[-1,], editable = FALSE, rownames = FALSE,
                                                options = list(pageLength = 10)
    ))

  })
  
  
  output$copy_output.df_nma3_res_hr <- downloadHandler(
    filename = function() {
      paste("df_nma3_res_hr", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$copy_df_nma3_res_hr, file,row.names = TRUE)
    }
  )
  
  output$copy_df_gengamma_hr <- downloadHandler(
    filename = function() {
      paste("df_gengamma_hr", ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$df_gengamma_hr, file,row.names = TRUE)
    }
  )
  
  ##### end of gengamma #####
  
  observe({
    if(input$run_nma_coxph > 0) {
      session$sendCustomMessage('enableTab8', list(enable = TRUE))
    }
  })
  observe({
    if(input$run_nma_gengamma > 0) {
      session$sendCustomMessage('enableTab9', list(enable = TRUE))
    }
  })
  
  observeEvent(input$run_nma_coxph, {
    updateTabsetPanel(session, "subset_cph_panel", selected ="1.2 Show the COX PH results")
  })
  
  observeEvent(input$run_nma_gengamma, {
    updateTabsetPanel(session, "subset_gengamma_panel", selected ="2.2 Show the Gen-Gamma results")
  })
  
  ##### codes for running output #####
  # observe({
  #   if(length(models$ran_models) > 0) {
  #     session$sendCustomMessage('enableTab10', list(enable = TRUE))
  #   }
  # })
  
  output$model_checkboxes <- renderUI({
    if (length(models$ran_models) > 0) {
      checkboxGroupInput(
        "selected_models", 
        "Show selected model outcomes", 
        choices = models$ran_models,
        selected = models$ran_models 
      )
    } else {
      div(
        HTML('<h4 style="color: red; font-weight: bold;">No model selected at this time!</h4>')
      )
    }
  })
  
  observeEvent(input$goutput_fp1ff,{
    temp_model_name <- values$temp_fp1ff
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp1fr,{
    temp_model_name <- values$temp_fp1fr
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp1bf,{
    temp_model_name <- values$temp_fp1bf
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp1br,{
    temp_model_name <- values$temp_fp1br
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp2ff,{
    temp_model_name <- values$temp_fp2ff
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp2fr,{
    temp_model_name <- values$temp_fp2fr
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp2bf,{
    temp_model_name <- values$temp_fp2bf
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_fp2br,{
    temp_model_name <- values$temp_fp2br
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_pwe1f,{
    temp_model_name <- values$temp_pwe1f
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_pwe1b,{
    temp_model_name <- values$temp_pwe1b
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_pwe2f,{
    temp_model_name <- values$temp_pwe2f
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_pwe2b,{
    temp_model_name <- values$temp_pwe2b
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm11,{
    temp_model_name <- values$temp_psm11
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm12,{
    temp_model_name <- values$temp_psm12
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm13,{
    temp_model_name <- values$temp_psm13
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm14,{
    temp_model_name <- values$temp_psm14
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm15,{
    temp_model_name <- values$temp_psm15
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm16,{
    temp_model_name <- values$temp_psm16
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm17,{
    temp_model_name <- values$temp_psm17
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_psm18,{
    temp_model_name <- values$temp_psm18
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_cph,{
    temp_model_name <- values$temp_cph
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })
  
  observeEvent(input$goutput_gg,{
    temp_model_name <- values$temp_gg
    models$ran_models <- unique(c(models$ran_models, temp_model_name))
    temp_file <- tempfile(fileext = ".docx")
    doc <- read_docx()
    doc<-generate_output(doc,temp_model_name,values)
    print(doc, target = temp_file)
    opres$ldoc[[temp_model_name]] <- temp_file
    shinyalert(title = "Export data successfully! Please go to the 'Application output - Output report' to check! ",
               type = "success")
  })

  output$downloadWord <- downloadHandler(
    filename = function() {
      paste0("output_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$selected_models)
      final_doc <- read_docx()
      
      for (selected_model in input$selected_models) {
        temp_file <- opres$ldoc[[selected_model]]
        if (!is.null(temp_file)) {
          final_doc <- body_add_docx(final_doc, src = temp_file)
        }
      }
      
      print(final_doc, target = file)
    }
  )
  

  
  ##### edn of codes for running output #####
  
  
  

  
  
  
### final line 
}



