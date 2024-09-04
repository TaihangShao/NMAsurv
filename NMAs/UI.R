options(encoding = "UTF-8")
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(magrittr)))
options(encoding = "UTF-8")


# package
#----
suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(writexl)))
suppressWarnings(suppressMessages(library(readxl)))
suppressWarnings(suppressMessages(library(DT)))
suppressWarnings(suppressMessages(library(heemod)))
suppressWarnings(suppressMessages(library(flexsurv)))
suppressWarnings(suppressMessages(library(shinycssloaders)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(shinydashboardPlus)))
suppressWarnings(suppressMessages(library(slickR)))
suppressWarnings(suppressMessages(library(rhandsontable)))
suppressWarnings(suppressMessages(library(shinyWidgets)))
suppressWarnings(suppressMessages(library(shinyhelper)))
suppressWarnings(suppressMessages(library(shinytitle)))
suppressWarnings(suppressMessages(library(clipr)))
suppressWarnings(suppressMessages(library(rmarkdown)))
suppressWarnings(suppressMessages(library(shinyjs)))
suppressWarnings(suppressMessages(library(shinyalert)))
suppressWarnings(suppressMessages(library(markdown)))
suppressWarnings(suppressMessages(library(markdownInput)))
suppressWarnings(suppressMessages(library(shinymanager)))
suppressWarnings(suppressMessages(library(rclipboard)))

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(gridExtra)))
suppressWarnings(suppressMessages(library(survHE)))
suppressWarnings(suppressMessages(library(discSurv)))
suppressWarnings(suppressMessages(library(survminer)))
suppressWarnings(suppressMessages(library(doBy)))
suppressWarnings(suppressMessages(library(netmeta)))
suppressWarnings(suppressMessages(library(survival)))
suppressWarnings(suppressMessages(library(R2jags)))
suppressWarnings(suppressMessages(library(ggmcmc)))
suppressWarnings(suppressMessages(library(lme4)))
suppressWarnings(suppressMessages(library(broom)))
suppressWarnings(suppressMessages(library(metafor)))
suppressWarnings(suppressMessages(library(R2WinBUGS)))
suppressWarnings(suppressMessages(library(MatrixModels)))
suppressWarnings(suppressMessages(library(grid)))


cat("\n Welcome to this APP! \n")

# Side bar tabs
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              style = "font-size: 18px;",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("house")),
              menuItem("User Manual", tabName = "um", icon = icon("book")),
              menuItem("Data transform", tabName = "transform", icon = icon("clipboard")),
              menuItem("Network plot", tabName = "np", icon = icon("chart-area")),
              menuItem("PH assumptions tests", tabName = "pht", icon = icon("flask")),
              menuItem("AD-based NMA", tabName = "survival1", icon = icon("bars"),
                       menuSubItem("Import data",icon = icon("file-import"), tabName = "one1"),
                       menuSubItem("1.Fractional polynomial",icon = icon("chart-area"), tabName = "one2"),
                       menuSubItem("2.Piecewise exponential model",icon = icon("chart-area"), tabName = "one3"),
                       menuSubItem("3.Parametric survival model",icon = icon("chart-area"), tabName = "one4"),
                       hr(),
                       HTML("<h6>Adjusting the Y axis (AD-based NMA)</h6>"),
                       HTML("<h6>Slider for Hazard plot</h6>"),
                       checkboxInput("y_axis_haz", HTML("<h6>Activate axis control (Hazard)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_haz==true",
                         sliderInput("y_min_haz", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_haz", "Y-axis Max", min = 1, max = 100, value = 10, step = 1)),
                       HTML("<h6>Slider for HR plot</h6>"),
                       checkboxInput("y_axis_hr", HTML("<h6>Activate axis control (HR)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_hr==true",
                       sliderInput("y_min_hr", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                       sliderInput("y_max_hr", "Y-axis Max", min = 1, max = 100, value = 10, step = 1))
              ),
              menuItem("IPD-based NMA", tabName = "survival2", icon = icon("bars"),
                       menuSubItem("Import data",icon = icon("file-import"), tabName = "two1"),
                       menuSubItem("1.Cox PH Model",icon = icon("chart-area"), tabName = "two2"),
                       menuSubItem("2.Generalised Gamma Model",icon = icon("chart-area"), tabName = "two3")
              )
  )
)
# Interface
dashboardPage(
  skin = "blue",
  # Title with name of the app
  dashboardHeader(title = "NMAs",
                  dropdownMenu(type = "notifications", badgeStatus = "warning",headerText="If the content is not fully displayed, 
                               you can adjust the zoom ratio of the web page display.
                               For example, you can try adjusting the zoom ratio to 75%."

                    )
                  ),
  sidebar,
  dashboardBody(
    rclipboardSetup(),
    withMathJax(),
    useShinyjs(),
    useShinyalert(force = TRUE),
    tags$head(
      tags$style(HTML("
      .scrollable-panel {
        max-height: 200px;
        overflow-y: auto;
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
    "))
    ),
    tags$head(
      tags$style(HTML("
      .dashboard-title {
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
    "))
    ),
    tags$head(
      tags$style(HTML("
      .text-op {
        color: red;
        font-weight: bold;
      }
    "))
    ),
    tabItems(
      ### Dashboard tab
      tabItem(tabName = "dashboard",
              #----
              column(width = 12,
                     wellPanel(
                       class = "dashboard-title",
                       HTML("<h1>Welcome to the tool for Network Meta-Analysis of survival data (NMAs) "),
                       HTML("<h4>This tool provides an open-access tool powered by R that can be used to conduct network meta-analysis (NMA) based on survival data. 
                       In this APP, we provided several methods including Aggregated data(AD)-based NMA and Individual patient data(IPD)-based NMA. 
                       Please note that not all existing NMA methods are included in this app, so please be careful when using this APP (You can find more in the references).
                     Under many real-world situations, researchers do not have the IPD. Therefore,
                     this tool focused on survival data only, covariates can not be included in this APP.
                     If you have survival data with covariates, we recommend you to conduct further analyses like meta-regression or considering treatment-by-covariates interactions.
                     Please refer to the references in this app for relevant methodology and codes.
                     You can get started by pressing the button below or using the sidebar button.</h4>")
                     ),
                     fluidRow(
                       column(width = 12, align ="center",
                              actionButton("FCSubmit6", label="Read the User Manual!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                              actionButton("FCSubmit3", label="Go to Data Transform!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              actionButton("FCSubmit4", label="Go to Network Plot!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              actionButton("FCSubmit5", label="Go to PH Assumption Test!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                              actionButton("FCSubmit1", label="Get start with AD-based NMA!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              actionButton("FCSubmit2", label="Get start with IPD-based NMA!",style="color: black;
                       background-color: white; border-color: black; font-size: 18px; font-weight: bold; height: 50px; width: 300px;"),
                              # actionButton("FCvi", label="Visual inspection"),
                              # actionButton("downloadmanual", "Download manual",
                              #              onclick = "location.href='files/manual.rar'"),
                              # actionButton("FCcitation", label="Copy citation"),
                       ),
                       # textOutput("original_wd0")
                     ),
                     p("")
              ),
              column(width = 12,
                     # height = 1,
                     # wellPanel(
                     #   class = "custom-well",
                       HTML("<h3><b>ABOUT</b></h3>"),
                     # ),
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h4>BugReports: <u>travis_shao@outlook.com</u></h4>"),
                       HTML("<h4>This tool has been developed by:<br></h4>"),
                       HTML("<h5>Taihang Shao<sup>[1][2]</sup>, Mingye Zhao<sup>[1]</sup>, Fenghao Shi<sup>[3]</sup>, Mingjun Rui<sup>[4]</sup>, Wenxi Tang<sup>[1]</sup><br></h5>"),
                       HTML("<h5>[1] Center for Pharmacoeconomics and Outcome Research, China Pharmaceutical University </em>
                       <br> [2] School of Public Health and Primary Care, The Chinese University of Hong Kong </em>
                       <br> [3] International Research Center for Medicinal Administration, Peking University </em>
                       <br> [4] School of Pharmacy, The Chinese University of Hong Kong </em>
                          </h5>")
                     ),
              ),
              column(width = 12,
                     # wellPanel(
                       HTML("<h3><b>LOG</b></h3>")
                     # ),
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h4><em> June 28, 2024 </em>: Version 1.4 PH assumption tests are available in this APP now.</h5>"),
                       HTML("<h4><em> May 4, 2024 </em>: Version 1.3 Some improvements are made in this APP.</h5>"),
                       tags$p(style = "text-indent: 1em;", "1.DIC of Parametric survival model is available; 2.All tables can be downloaded; 
                            3.Winbugs is replaced by JAGS in IPD-based NMA; 4.Results of Rank in IPD-based NMA are removed."),
                       HTML("<h4><em> April 30, 2024 </em>: Version 1.2 Some improvements are made in this APP. More results are available now.</h5>"),
                       tags$p(style = "text-indent: 1em;", "1.Advanced setting of JAGS is improved; 2.Rhat results of Bayesian Analysis are available; 
                            3.All estimates of parameters are available."),
                       HTML("<h4><em> April 6, 2024 </em>: Version 1.1 Gengamma model is available now.</h5>"),
                       HTML("<h4><em> December 17, 2023 </em>: Version 1.0 NMA is constructed and 4 methods are provided.</h5>"),
                       tags$p(style = "text-indent: 1em;", "Fractional polynomial, Piecewise exponential model, Parametric survival model and COX-PH model are now available."),
                     ),
              ),
              column(width = 12,
                     # wellPanel(
                       HTML("<h3><b>USEFUL REFERENCE</b></h3>"),
                       HTML("<h4>If you use this tool, these references will be helpful to you:</h4>")
                     # )
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("
                          <p>[1]Shao T, Zhao M, Liang L, Tang W. A systematic review and network meta-analysis of first-line immune checkpoint inhibitor combination therapies in patients with advanced non-squamous non-small cell lung cancer. Front Immunol. 2022;13:948597. Published 2022 Oct 26. doi:10.3389/fimmu.2022.948597</p>
                          <p>[2]Guyot P, Ades AE, Ouwens MJ, Welton NJ. Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. BMC Med Res Methodol. 2012;12:9 doi:10.1186/1471-2288-12-9</p>
                          <p>[3]Jansen JP. Network meta-analysis of survival data with fractional polynomials. BMC Med Res Methodol. 2011;11:61 doi:10.1186/1471-2288-11-61</p>
                          <p>[4]Royston P, Parmar MK. Restricted mean survival time: an alternative to the hazard ratio for the design  and analysis of randomized trials with a time-to-event outcome. BMC Med Res Methodol. 2013;13:152 doi:10.1186/1471-2288-13-152</p>
                          <p>[5]Wiksten A, Hawkins N, Piepho HP, Gsteiger S. Nonproportional Hazards in Network Meta-Analysis: Efficient Strategies for Model  Building and Analysis. Value Health. 2020;23(7):918-927 doi:10.1016/j.jval.2020.03.010</p>
                          <p>[6]Freeman SC, Cooper NJ, Sutton AJ et al. Challenges of modelling approaches for network meta-analysis of time-to-event  outcomes in the presence of non-proportional hazards to aid decision making:  Application to a melanoma network. Stat Methods Med Res. 2022;31(5):839-861 doi:10.1177/09622802211070253</p>
                          <p>[7]Zhao M, Shao T, Ren Y, Zhou C, Tang W. Identifying optimal PD-1/PD-L1 inhibitors in first-line treatment of patients  with advanced squamous non-small cell lung cancer in China: Updated systematic  review and network meta-analysis. Front Pharmacol. 2022;13:910656 doi:10.3389/fphar.2022.910656</p>
                          <p>[8]Ng'Andu NH. An empirical comparison of statistical tests for assessing the proportional hazards assumption of Cox's model. Stat Med. 1997;16(6):611-26 doi:10.1002/(sici)1097-0258(19970330)16:6<611::aid-sim437>3.0.co;2-t</p>
                          <p>[9]Freeman SC, Carpenter JR. Bayesian one-step IPD network meta-analysis of time-to-event data using Royston-Parmar models. Res Synth Methods. 2017;8(4):451-464 doi:10.1002/jrsm.1253</p>
                          <p>[10]Fisher D. IPDMETAN: Stata module for performing two-stage IPD meta-analysis. Statistical Software Components. 2019;</p>
                          <p>[11]Cope S, Chan K, Jansen JP. Multivariate network meta-analysis of survival function parameters. Res Synth Methods. 2020;11(3):443-456. doi:10.1002/jrsm.1405</p>
                          <p>[12]Cope S, Chan K, Campbell H, et al. A Comparison of Alternative Network Meta-Analysis Methods in the Presence of Nonproportional Hazards: A Case Study in First-Line Advanced or Metastatic Renal Cell Carcinoma. Value Health. 2023;26(4):465-476. doi:10.1016/j.jval.2022.11.017</p>
                          <p>[13]Jansen JP, Cope S. Meta-regression models to address heterogeneity and inconsistency in network meta-analysis of survival outcomes. BMC Med Res Methodol. 2012;12:152. Published 2012 Oct 8. doi:10.1186/1471-2288-12-152</p>
                          <p>[14]Ouwens MJ, Philips Z, Jansen JP. Network meta-analysis of parametric survival curves. Res Synth Methods. 2010;1(3-4):258-271. doi:10.1002/jrsm.25</p>
                          <p>[15]Petersohn S, McGregor B, Klijn SL, et al. Challenges in conducting fractional polynomial and standard parametric network meta-analyses of immune checkpoint inhibitors for first-line advanced renal cell carcinoma. J Comp Eff Res. 2023;12(8):e230004. doi:10.57264/cer-2023-0004</p>
                          <p>[16]Dias S, Sutton AJ, Ades AE, Welton NJ. Evidence synthesis for decision making 2: a generalized linear modeling framework for pairwise and network meta-analysis of randomized controlled trials. Med Decis Making. 2013;33(5):607-617. doi:10.1177/0272989X12458724</p>
                          <p>[17]Piepho HP, Madden LV, Roger J, Payne R, Williams ER. Estimating the variance for heterogeneity in arm-based network meta-analysis. Pharm Stat. 2018;17(3):264-277. doi:10.1002/pst.1857</p>
                          <p>[18]Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network Meta-Analysis for Decision Making. Hoboken, NJ: Wiley; 2018.</p>
                          <p>[19]The National Institute for Health and Care Excellence. CHTE2020 SOURCES AND SYNTHESIS OF EVIDENCE; UPDATE TO EVIDENCE SYNTHESIS METHODS. https://nicedsu.sites.sheffield.ac.uk/methods-development/chte2020-sources-and-synthesis-of-evidence. Accessed 19 December, 2023</p>
                          <p>[20]Welton NJ, Sutton AJ, Cooper NJ, Abrams KR, Ades AE. Evidence Synthesis for Decision Making in Healthcare. Chichester, UK: John Wiley & Sons; 2012.</p>
                          <p>[21]Wiecek W, Karcher H. Nivolumab versus Cabozantinib: Comparing Overall Survival in Metastatic Renal Cell Carcinoma. PLoS One. 2016;11(6):e0155389. Published 2016 Jun 6. doi:10.1371/journal.pone.0155389</p>
                          <p>[22]de Jong VMT, Moons KGM, Riley RD, et al. Individual participant data meta-analysis of intervention studies with time-to-event outcomes: A review of the methodology and an applied example. Res Synth Methods. 2020;11(2):148-168. doi:10.1002/jrsm.1384</p>
                          <p>[23]Cox C, Chu H, Schneider MF, Muñoz A. Parametric survival analysis and taxonomy of hazard functions for the generalized gamma distribution. Stat Med. 2007;26(23):4352-4374. doi:10.1002/sim.2836</p>  
                          <p>[24]Heeg B, Garcia A, Beekhuizen SV, et al. Novel and existing flexible survival methods for network meta-analyses. J Comp Eff Res. Published online September 12, 2022. doi:10.2217/cer-2022-0044</p>  
                            ")
                     )
              )
       ),
      # End of dashboard
      #----
      
      tabItem(tabName = "um",div(style = "text-align: center;",tags$iframe(src = "NMAs_user_manual.pdf", width = "100%", height = "1440px")),
      ),
      
      
      #Import data tab
      tabItem(tabName = "transform",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Data Transform (From IPD to Aggregated data)"))%>%
                    helper(type = "markdown", content = "idreadme",icon = "triangle-exclamation"),
                  p(""),
                  fileInput("upload", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload"),
                  p(""),
                  actionButton("downloadtemplate", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                               onclick = "location.href='files/example2.xlsx'"),
                  p(""),
                  actionButton("FCexample", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  # uiOutput("tran_ref1"),
                  # p(""),
                  uiOutput("tran_length1"),
                  p(""),
                  uiOutput("tran_step1"),
                  p(""),
                  hr(),
                  p(""),
                  uiOutput("tran_ready"),
                  hr()
                ),
                mainPanel(
                  checkboxInput("show_table1", strong("Show IPD data"), value = FALSE),
                  conditionalPanel(
                    condition = "input.show_table1 == true",
                    DTOutput("upl_outDT")),
                  checkboxInput("show_table2", strong("Show the included treatment and reference number"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table2 == true",
                    fluidRow(
                      column(width = 6,DTOutput("tran_trt")),
                      column(width = 6,DTOutput("tran_stu")))
                    ),
                  checkboxInput("show_table3", strong("Show Aggreated data"), value = TRUE),
                  conditionalPanel(
                    condition = "input.show_table3 == true",
                    DTOutput("tran_agg"),
                    conditionalPanel(
                      condition = "output.tran_agg",
                      downloadButton("downloadtable_tran_agg", "Download Aggregated Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ))
                )
              )
      ), 
      # End of Import data tab
      #----
### PH test      
      tabItem(tabName = "pht",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("PH assumptions tests")),
                  p(""),
                  fileInput("uploadpht", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload_pht"),
                  p(""),
                  actionButton("downloadtemplatepht", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                               onclick = "location.href='files/exampleph.xlsx'"),
                  p(""),
                  actionButton("FCexamplepht", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  selectInput("select_pht", 
                              label = "Select a method for PH assumptions test",
                              choices = list("Schoenfeld residual plot" = "pht1", 
                                             "Log-Log plot" = "pht2", 
                                             "Grambsch-Therneau test" = "pht3"),
                              selected = "pht1"),
                  p(""),
                  hr(),
                  uiOutput("pht_ready"),
                  p(""),
                  hr(),
                  p("The example data here is obtained from the trial Keynote 006 of the melanoma network. 
                    Full data of the melanoma network can be obtained from the section 'IPD-based NMA'"),
                  p(""),
                  HTML("This APP provided three methods to test the PH assumption. They are Schoenfeld residual plot, Log-Log plot and Grambsch-Therneau test.
                  Here is the summarized introduction for these three methods:<br><br>
                  <b>Schoenfeld residual plot</b><br>
                       The Schoenfeld residual test primarily examines the proportional hazards assumption by analyzing the changes in Schoenfeld residuals over time. 
                       The basic idea is that if the proportional hazards assumption holds, 
                       the Schoenfeld residuals should be randomly distributed over time without showing any systematic trend.
                       In this APP, user can get the P value through the Schoenfeld residual plot. 
                       Typically, if P value is smaller than 0.05, PH assumption is considered not held.
                       <br><br>
                  <b>Grambsch-Therneau test</b><br>
                       The Grambsch-Therneau test is a more formal statistical test based on Schoenfeld residuals. 
                       It evaluates the proportional hazards assumption by examining the correlation between Schoenfeld residuals and the ranks of time.
                       In this APP, user can get the P value through the Grambsch-Therneau test. 
                       Typically, if P value is smaller than 0.05, PH assumption is considered not held.
                       <br><br>
                  <b>Log-Log plot</b><br>
                       If the proportional hazards (PH) assumption holds, the log-cumulative hazard curves for different groups should be parallel.
                       Through visual inspection, if two lines are not parallel, PH assumption is considered not held.
                       <br><br>
                       Usually, PH assumption uncertain in > 1 trials in the network, users should consider NMA that do not rely on PH assumption; 
                       No evidence of a violation of the PH assumption in any of the trials in the network, users can consider PH models.<br>
                       Some usedful documents can be found here:
                       "),
                  p(""),
                  tags$a(href = "https://academic.oup.com/biomet/article-abstract/69/1/239/243012", "Partial residuals for the proportional hazards regression model; DAVID SCHOENFELD (1982)", target = "_blank"),br(),
                  tags$a(href = "https://academic.oup.com/biomet/article-abstract/81/3/515/257037", "Proportional hazards tests and diagnostics based on weighted residuals; PATRICIA M. GRAMBSCH, TERRY M. THERNEAU (1994)", target = "_blank"),br(),
                  tags$a(href = "https://www.sciencedirect.com/science/article/pii/S1098301522047490", "A Comparison of Alternative Network Meta-Analysis Methods in the Presence of Nonproportional Hazards: A Case Study in First-Line Advanced or Metastatic Renal Cell Carcinoma; Shannon Cope.et.al. (2023)", target = "_blank"),br()
                ),
                mainPanel( 
                  HTML("<b>Data used to test the PH assumption</b>"),
                  DTOutput("df_nma_pht"),
                  HTML("<b>Selected PH assumption test results</b>"),
                  conditionalPanel(
                    condition = "input.select_pht == 'pht1'",
                    plotOutput("plot_pht_1",width = "800px",height  = "600px"),
                    conditionalPanel(
                      condition = "output.plot_pht_1",
                      actionButton("ph_1_dw", "Download plot", icon("download"),
                                   style="color: black;background-color: white; border-color: #2e6da4"),
                      conditionalPanel(
                        condition = "input.ph_1_dw",
                        uiOutput("pht_1_sizebut"),
                        downloadButton("downloadPlot_pht_1", "", icon = icon("download"))
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.select_pht == 'pht2'",
                    plotOutput("plot_pht_2",width = "800px",height  = "600px"),
                      conditionalPanel(
                        condition = "output.plot_pht_2",
                      actionButton("ph_2_dw", "Download plot", icon("download"),
                                   style="color: black;background-color: white; border-color: #2e6da4"),
                      conditionalPanel(
                        condition = "input.ph_2_dw",
                        uiOutput("pht_2_sizebut"),
                        downloadButton("downloadPlot_pht_2", "", icon = icon("download"))
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.select_pht == 'pht3'",
                    verbatimTextOutput("text_pht_1"),
                  )
                )
              ) 
      ),
### end of PH test
      tabItem(tabName = "np",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Network Plot")),
                  p(""),
                  fileInput("uploadnp", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload_np"),
                  p(""),
                  actionButton("downloadtemplatenp", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                               onclick = "location.href='files/examplenp.xlsx'"),
                  p(""),
                  actionButton("FCexamplenp", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  uiOutput("nptitle"),
                  uiOutput("nplcol"),
                  uiOutput("npmultiarm"),
                  conditionalPanel(
                    condition = "input.np_multiarm == 'TRUE'",
                    uiOutput("npmcol"),
                    
                  ),
                  uiOutput("nppoint"),
                  uiOutput("npword"),
                  uiOutput("npbg"),
                  uiOutput("np_ready"),
                  p(""),
                  hr(),
                  p("The example data here is obtained from Shao T (2022). This NSCLC (OS) network included 10 treatments and 6 studies.
                    Since study 6 (Impower150) is a three-arm study, we will split it into 3 lines for input.
                    If you want to know more about the data source or the methodology, please refer to the following two links:"),
                  p(""),
                  tags$a(href = "https://www.frontiersin.org/articles/10.3389/fimmu.2022.948597/full", "A systematic review and network meta-analysis of first-line immune checkpoint inhibitor combination therapies in patients with advanced non-squamous non-small cell lung cancer; Shao T (2022)", target = "_blank"),br(),
                  tags$a(href = "https://www.rdocumentation.org/packages/netmeta/versions/2.8-2/topics/netmeta", "netmeta: Network meta-analysis using graph-theoretical method", target = "_blank"),br()
                ),
                mainPanel( #### 需要修改
                  HTML("<b>Data used to draw the network plot</b>"),
                  DTOutput("df_nma_np"),
                  HTML("<b>Network Plot</b>"),
                  plotOutput("plot_np",width = "800px",height  = "600px"),
                  conditionalPanel(
                    condition = "output.plot_np",
                    numericInput("heightnp", "Height (px)", value = 600),
                    numericInput("widthnp", "Width (px)", value = 800),
                    downloadButton("downloadPlot", "", icon = icon("download"))
                  )
                )
              ) 
      ),
      #----
      #### Start of nma page
      #NMA1
      tabItem(tabName = "one1",  
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Import Data (AD-based NMA)")),
                  p(""),
                  fileInput("upload1", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload_a"),
                  p(""),
                  actionButton("downloadtemplate1", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                               onclick = "location.href='files/example1.xlsx'"),
                  p(""),
                  actionButton("FCexample1", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  p("The example aggregated data here is obtained from Jansen JP (2011). This NSCLC network included 4 treatments and 7 studies. 
                  Treatment 1 is the reference treatment.", strong("In this APP, the reference treatment should be set as treatment 1 (code 1)."),
                    "The aggregated data input here can be extracted directly from published survival curves or obtained from reconstructed IPD.
                    If you want to know how to extract data from survival curves, please refer to the following two articles:"),
                  p(""),
                  tags$a(href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-11-61", "[1]Network meta-analysis of survival data with fractional polynomials; Jansen JP (2011)", target = "_blank"),br(),
                  tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1002/jrsm.25", "[2]Network meta-analysis of parametric survival curves; Ouwens MJ et.al. (2010)", target = "_blank"),br(),
                  p(""),
                  p("A useful note before running the model: Some models may not be suitable for your data. 
                  (The fitted or extrapolated survival curve exhibits strange shapes.) 
                    For example, Piecewise exponential models have a key assumption that the treatment effects are proportional within a time interval.
                    Thus, in some case, PWE do not perform well. In addition, PWE may not perform well when extrapolating survival curves beyond the observed data.
                    In order to find the most suitable, users should try different models with different parameters input, and use methods like statistical indicators or visual inspection to select models."),
                  p(""),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                    tags$hr(style="border-top: 1px solid black;"),
                    actionButton("ready_rm1", label="Move to Fractional polynomials",icon("play"),style="color: black;
                       background-color: white; border-color: #2e6da4"),br(),
                    p(),
                    actionButton("ready_rm2", label="Move to Piecewise exponential models",icon("play"),style="color: black;
                       background-color: white; border-color: #2e6da4"),br(),
                    p(),
                    actionButton("ready_rm3", label="Move to Parametric survival model",icon("play"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  )
                ),
                mainPanel(
                  HTML("<b>AD data used in NMA</b>"),
                  DTOutput("df_nma_ad"),
                  HTML("<b>Included treatments and studies, and reference numbers</b>"),
                  fluidRow(
                    column(width = 6,DTOutput("df_nma_ad_t")),
                    column(width = 6,DTOutput("df_nma_ad_s")))
                )
              )
      ),
      #----
      tabItem(tabName = "one2",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Fractional polynomials (FP)"))%>%
                    helper(type = "markdown", content = "table1"),
                  hr(),
                  div(style = "height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 8px;",
                      HTML("<b>Read first (Some notes for FP NMA)</b>"),
                      p("In this FP NMA section, we use the framework of Wiksten et al.(2020). We propose a two-step process for fitting FP models. 
                  In the first step, an ANOVA-like parameterisation is used to express and fit the models as GLM with time-varying covariates in a frequentist framework. 
                  The fit of the models in terms of the AIC is compared.
                  The model with the lowest AIC can be selected to fit in the Bayesian setting (Frequentist setting) in the second step.
                  For fixed effect model, we provide the parameter estimates. Hazard plot and survival plot are also drawn based on these parameters."),
                  HTML("<u>Use the Difference in Beta and the Beta of reference treatment to calculate other Beta values. 
                  Then, hazard over time for each of the interventions can be calculated through the function with Beta. 
                  In addition, through d (trtf in Frequentist setting), Hazard Ratio between selected treatments can be calculated.
                  This process can also be realized easily through EXCEL.</u><br><br>"),
                  HTML("<b>For Example:</b> For reference treatment a and intervention b, we construct a FP1 model with power = -2.
                       We get d<sub>0ab</sub>, d<sub>1ab</sub>, Beta<sub>0a</sub>, Beta<sub>1a</sub>, 
                       Beta<sub>0b</sub>, Beta<sub>1b</sub>.<br>
                       Note:Beta<sub>0b</sub>=Beta<sub>0a</sub>+d<sub>0ab</sub>;Beta<sub>1b</sub>=Beta<sub>1a</sub>+d<sub>1ab</sub>.<br>
                       Thus, Log(HR<sub>ab</sub>(t))=d<sub>0ab</sub>+d<sub>1ab</sub>*t<sup>-2</sup>,<br>
                       Log(Hazard<sub>a</sub>(t))=Beta<sub>0a</sub>+Beta<sub>1a</sub>*t<sup>-2</sup>,<br>
                       Log(Hazard<sub>b</sub>(t))=Beta<sub>0b</sub>+Beta<sub>1b</sub>*t<sup>-2</sup>.<br>
                       Note:Beta<sub>0a</sub>, Beta<sub>0b</sub> are usually calculated as average from 
                       study specific estimates(μ0, μ1, μ2) of the reference treatment.
                       One alternative is to fit and extrapolate the selected reference treatment with FP model to calculate the Beta.
                       However, biases may be introduced through this method."
                       ),
                  p(),
                  p("For random effect model, we provide the parameter estimates only. Plots can be drawn through the same methods.
                    In addition, we only consider the model with a heterogeneity parameter for d0 in random effect model. 
                    We do not consider all heterogeneity parameters (d0, d1, d2 in FP2) since we believe that the methodology still need further development.
                    (Bayesian analysis using the random-effects model would require careful specification of the prior for between-study heterogeneity. 
                    Also, maximum likelihood estimation can be problematic at this time, in which the number of parameters increases with the number of studies.)
                    An example of Bugs codes for random effect FP models with all heterogeneity parameters can be found in Jansen JP (2011).
                    In Jansen's study, two kinds of random effect models showed similar fit results. 
                    ")),
                  p("For more information about FP, please refer to the following two articles:"),
                  p(""),
                  tags$a(href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-11-61", "[1]Network meta-analysis of survival data with fractional polynomials; Jansen JP (2011)", target = "_blank"),br(),
                  tags$a(href = "https://doi.org/10.1016/j.jval.2020.03.010", "[2]Nonproportional Hazards in Network Meta-Analysis: Efficient Strategies for Model Building and Analysis; Anna Wiksten et.al. (2020)", target = "_blank"),br(),
                  p(""), 
                  hr(),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                    textInput("nma1_fp_refs","Reference Study",value = "Kim2008"),
                    textInput("nma1_fp_reft","Reference Treatment",value = "Docetaxel"),
                    numericInput("nma1_fp_ex","Extrapolation time (Year)",value = 5,min = 0),
                    actionButton("nma1_fp_run", label="Run FP NMA (Frequentist analysis)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                    p(""),
                  ),
                  hr(),
                  conditionalPanel(
                    condition = "output.nma1_fp_fix_aic",
                    selectInput("fp1_fp2","Run specific FP1 or FP2 (Frequentist analysis)",choices = c(
                      "FP1:Fixed effect model"=1,
                      "FP2:Fixed effect model"=2,
                      "Do not show Frequentist analysis results"=3
                    ),selected = 3),
                    checkboxInput("ran_freq","Consider random effect",value = FALSE),
                    hr(),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 1",
                      numericInput("fp1_pow","Power of FP1",value = -2),
                      actionButton("nma1_fp_run1", label="Run FP1 NMA",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 2",
                      numericInput("fp2_pow1","Power of FP2_1",value = -2),
                      numericInput("fp2_pow2","Power of FP2_2",value = 1),
                      actionButton("nma1_fp_run2", label="Run FP2 NMA",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                  ),
                  conditionalPanel(
                    condition = "output.nma1_fp_fix_aic",
                    selectInput("fp1_fp2_bay","Run specific FP1 or FP2 (Bayesian analysis)",choices = c(
                      "FP1:Fixed effect model"=1,
                      "FP2:Fixed effect model"=2,
                      "Do not show Bayesian analysis results"=3
                    ),selected = 3),
                    checkboxInput("ran_bay","Consider random effect",value = FALSE),
                    hr(),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 1",
                      numericInput("fp1_pow_b","Power of FP1",value = -2),
                      actionButton("nma1_fp_run1b", label="Run FP1 NMA",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 2",
                      numericInput("fp2_pow1_b","Power of FP2_1",value = -2),
                      numericInput("fp2_pow2_b","Power of FP2_2",value = 1),
                      actionButton("nma1_fp_run2b", label="Run FP2 NMA",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                    checkboxInput("nma1_fp_jags_setting","Show advanced settings",value = FALSE),
                    conditionalPanel(
                      condition = "input.nma1_fp_jags_setting == true",
                      tags$hr(style="border-top: 1px solid black;"),
                      HTML("<b>Advanced settings for MCMC</b>"),
                      numericInput("fp_nchains","number of Markov chains",value = 3),
                      numericInput("fp_niter","number of total iterations per chain (including burn in",value = 20000),
                      numericInput("fp_nburnin","length of burn in",value = 10000),
                      numericInput("fp_nthin","thinning rate",value = 3),
                      HTML("<b>Advanced settings for priors</b>"),
                      numericInput("fp_pmean","priors of mu[]",value = 0),
                      numericInput("fp_pprec","priors of T[]",value = 0.0001)
                    )
                  )
                ),
                mainPanel(
                  conditionalPanel(
                    condition = "input.nma1_fp_run",
                    HTML("<b>AIC results for FP (Frequentist analysis:Fixed effect model)</b>"),
                    DTOutput("nma1_fp_fix_aic"),
                    hr(),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 1 & input.ran_freq == false",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_haz",
                               HTML("<b>Hazard plot for FP1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_haz"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_haz",
                                 actionButton("nma1_fp1_plot_haz_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_haz_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_haz_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_haz_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_haz", "", icon = icon("download"))
                                 )
                               ),
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_surv",
                               HTML("<b>Survival plot for FP1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_surv"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_surv",
                                 actionButton("nma1_fp1_plot_surv_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_surv_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_surv_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_surv_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_surv", "", icon = icon("download"))
                                 )
                               ),
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_hr",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for FP1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_hr"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_hr",
                                 actionButton("nma1_fp1_plot_hr_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_hr_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_hr_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_hr_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_hr", "", icon = icon("download"))
                                 )
                               ),
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 1",
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff",
                        conditionalPanel(
                          condition = "input.ran_freq == false",
                          HTML("<b>Coefficient Table for FP1 (Frequentist analysis:Fixed effect model)</b>")
                        ),
                        conditionalPanel(
                          condition = "input.ran_freq == true",
                          HTML("<b>Coefficient Table for FP1 (Frequentist analysis:Random effect model)</b>")
                        )
                      ),
                      DTOutput("nma1_fp1_fix_coeff"),
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff",
                        # uiOutput("copy_nma1_fp1_fix_coeff")
                        downloadButton("copy_nma1_fp1_fix_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                        )
                      ),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 2 & input.ran_freq == false",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_haz",
                               HTML("<b>Hazard plot for FP2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_haz"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_haz",
                                 actionButton("nma1_fp2_plot_haz_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_haz_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_haz_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_haz_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_haz", "", icon = icon("download"))
                                 )
                               )

                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_surv",
                               HTML("<b>Survival plot for FP2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_surv"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_surv",
                                 actionButton("nma1_fp2_plot_surv_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_surv_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_surv_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_surv_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_surv", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_hr",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for fp2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_hr"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_hr",
                                 actionButton("nma1_fp2_plot_hr_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_hr_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_hr_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_hr_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_hr", "", icon = icon("download"))
                                 )
                               ),
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2 == 2",
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff",
                        conditionalPanel(
                          condition = "input.ran_freq == false",
                          HTML("<b>Coefficient Table for FP2 (Frequentist analysis:Fixed effect model)</b>")
                        ),
                        conditionalPanel(
                          condition = "input.ran_freq == true",
                          HTML("<b>Coefficient Table for FP2 (Frequentist analysis:Random effect model)</b>")
                        )
                      ),
                      DTOutput("nma1_fp2_fix_coeff"),
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff",
                        # uiOutput("copy_nma1_fp2_fix_coeff")
                        downloadButton("copy_nma1_fp2_fix_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                        )
                      ),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 1 & input.ran_bay == false",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_haz_b",
                                 HTML("<b>Hazard plot for FP1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_haz_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_haz_b",
                                 actionButton("nma1_fp1_plot_haz_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_haz_b_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_haz_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_haz_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_haz_b", "", icon = icon("download"))
                                 )
                               )
                               
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_surv_b",
                                 HTML("<b>Survival plot for FP1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_surv_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_surv_b",
                                 actionButton("nma1_fp1_plot_surv_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_surv_b_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_surv_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_surv_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_surv_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_hr_b",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for FP1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp1_plot_hr_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp1_plot_hr_b",
                                 actionButton("nma1_fp1_plot_hr_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp1_plot_hr_b_dw",
                                   p(),
                                   uiOutput("nma1_fp1_plot_hr_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp1_plot_hr_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp1_plot_hr_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 1",
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff_b",
                        HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                        textOutput("nma1_fp1_fix_DIC_b")),
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff_b",
                        conditionalPanel(
                          condition = "input.ran_bay == true",
                          HTML("<b>Coefficient Table for FP1 (Bayesian analysis:Random effect model)</b>")
                        ),
                        conditionalPanel(
                          condition = "input.ran_bay == false",
                          HTML("<b>Coefficient Table for FP1 (Bayesian analysis:Fixed effect model)</b>")
                        )
                      ),
                      DTOutput("nma1_fp1_fix_coeff_b"),
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff_b",
                        # uiOutput("copy_nma1_fp1_fix_coeff_b")
                        downloadButton("copy_nma1_fp1_fix_coeff_b", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      ),
                      DTOutput("nma1_fp1_fix_coeff_b_mu"),
                      conditionalPanel(
                        condition = "output.nma1_fp1_fix_coeff_b_mu",
                        # uiOutput("copy_nma1_fp1_fix_coeff_b_mu")
                        downloadButton("copy_nma1_fp1_fix_coeff_b_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 2 & input.ran_bay == false",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_haz_b",
                                 HTML("<b>Hazard plot for FP2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_haz_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_haz_b",
                                 actionButton("nma1_fp2_plot_haz_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_haz_b_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_haz_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_haz_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_haz_b", "", icon = icon("download"))
                                 )
                               )
                               
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_surv_b",
                                 HTML("<b>Survival plot for FP2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_surv_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_surv_b",
                                 actionButton("nma1_fp2_plot_surv_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_surv_b_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_surv_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_surv_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_surv_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_hr_b",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for FP2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_fp2_plot_hr_b"),
                               conditionalPanel(
                                 condition = "output.nma1_fp2_plot_hr_b",
                                 actionButton("nma1_fp2_plot_hr_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_fp2_plot_hr_b_dw",
                                   p(),
                                   uiOutput("nma1_fp2_plot_hr_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_fp2_plot_hr_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_fp2_plot_hr_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.fp1_fp2_bay == 2",
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff_b",
                        HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                        textOutput("nma1_fp2_fix_DIC_b")),
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff_b",
                        conditionalPanel(
                          condition = "input.ran_bay == true",
                          HTML("<b>Coefficient Table for FP2 (Bayesian analysis:Random effect model)</b>")
                        ),
                        conditionalPanel(
                          condition = "input.ran_bay == false",
                          HTML("<b>Coefficient Table for FP2 (Bayesian analysis:Fixed effect model)</b>")
                        )
                      ),
                      DTOutput("nma1_fp2_fix_coeff_b"),
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff_b",
                        # uiOutput("copy_nma1_fp2_fix_coeff_b")
                        downloadButton("copy_nma1_fp2_fix_coeff_b", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      ),
                      DTOutput("nma1_fp2_fix_coeff_b_mu"),
                      conditionalPanel(
                        condition = "output.nma1_fp2_fix_coeff_b_mu",
                        # uiOutput("copy_nma1_fp2_fix_coeff_b_mu")
                        downloadButton("copy_nma1_fp2_fix_coeff_b_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    ),
                   ###Do not modify the below
                  )
                )
              )
              ),
      #----
      tabItem(tabName = "one3",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Piecewise Exponential (PWE)"))%>%
                    helper(type = "markdown", content = "table1"),
                  hr(),
                  div(style = "height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 8px;",
                      HTML("<b>Read first (Some notes for PWE NMA)</b>"),
                  p("The framework of PWE model is similar to that of FP. 
                  A PWE function can also be written in the form of ANOVA-like parameterisation, so that it can be fitted in the GLM framework.
                  Currently, only fixed effect models are available for PWE NMA. The choice of where to place cut points and how many cut points could result in 
                  many models being fitted before the best model can be selected. 
                  We recommend users to use Frequentist analysis to select suitable models before moving to Bayesian analysis."),
                  HTML("<u>Use the Difference in Beta and the Beta of reference treatment to calculate other Beta values. 
                  Then, hazard over time for each of the interventions can be calculated through the function with Beta. 
                  In addition, through d (trtf in Frequentist setting), Hazard Ratio between selected treatments can be calculated.
                  This process can also be realized easily through EXCEL.</u><br><br>"),
                  HTML("<b>For Example:</b> For reference treatment a and intervention b, we construct a PWE model with time point = 2.
                       We get d<sub>0ab</sub>, d<sub>1ab</sub>, Beta<sub>0a</sub>, Beta<sub>1a</sub>, 
                       Beta<sub>0b</sub>, Beta<sub>1b</sub>.<br>
                       Note:Beta<sub>0b</sub>=Beta<sub>0a</sub>+d<sub>0ab</sub>;Beta<sub>1b</sub>=Beta<sub>1a</sub>+d<sub>1ab</sub>-Beta<sub>0b</sub>.<br>
                       Thus, Log(HR<sub>ab</sub>(t))=d<sub>0ab</sub>(0<=t<2);d<sub>1ab</sub>(t>=2),<br>
                       Log(Hazard<sub>a</sub>(t))=Beta<sub>0a</sub>(0<=t<2);Beta<sub>1a</sub>(t>=2),<br>
                       Log(Hazard<sub>b</sub>(t))=Beta<sub>0b</sub>(0<=t<2);Beta<sub>1b</sub>(t>=2).<br>
                       Note:Beta<sub>0a</sub>, Beta<sub>0b</sub> are usually calculated as average from 
                       study specific estimates(μ0, μ1, μ2) of the reference treatment.
                       "
                  )),
                  p("For more information about methodology please refer to the following article:"),
                  p(""),
                  tags$a(href = "https://doi.org/10.1016/j.jval.2020.03.010", "[1]Nonproportional Hazards in Network Meta-Analysis: Efficient Strategies for Model Building and Analysis; Anna Wiksten et.al. (2020)", target = "_blank"),br(),
                  p(""), 
                  hr(),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                    textInput("nma1_pwe_refs","Reference Study",value = "Kim2008"),
                    textInput("nma1_pwe_reft","Reference Treatment",value = "Docetaxel"),
                    numericInput("nma1_pwe_ex","Extrapolation time (Year)",value = 5,min = 0),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                    p(""),
                    selectInput("pwe1_pwe2","Run specific PWE (Frequentist analysis)",choices = c(
                      "PWE with one cut point:Fixed effect model"=1,
                      "PWE with two cut points:Fixed effect model"=2,
                      "Do not show Frequentist analysis results"=3
                    ),selected = 3),
                    hr(),
                  ),
                  conditionalPanel(
                    condition = "input.pwe1_pwe2 == 1",
                    numericInput("pwe1_pow","Cutpoint of PWE",value = 2),
                    actionButton("nma1_pwe_run1", label="Run PWE NMA",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.pwe1_pwe2 == 2",
                    numericInput("pwe2_pow1","Cutpoint1 of PWE",value = 2),
                    numericInput("pwe2_pow2","Cutpoint2 of PWE",value = 12),
                    actionButton("nma1_pwe_run2", label="Run PWE NMA",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                  p(""),
                  tags$hr(style="border-top: 1px solid black;"),
                  selectInput("pwe1_pwe2_bay","Run specific PWE (Bayesian analysis)",choices = c(
                    "PWE with one cut point:Fixed effect model"=1,
                    "PWE with two cut points:Fixed effect model"=2,
                    "Do not show Bayesian analysis results"=3
                  ),selected = 3),
                  hr(),
                  ),
                  conditionalPanel(
                    condition = "input.pwe1_pwe2_bay == 1",
                    numericInput("pwe1_pow_b","Cutpoint of PWE",value = 2),
                    actionButton("nma1_pwe_run1b", label="Run PWE NMA",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "input.pwe1_pwe2_bay == 2",
                    numericInput("pwe2_pow1_b","Cutpoint1 of PWE",value = 2),
                    numericInput("pwe2_pow2_b","Cutpoint2 of PWE",value = 12),
                    actionButton("nma1_pwe_run2b", label="Run PWE NMA",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  p(""),
                  conditionalPanel(
                  condition = "output.df_nma_ad",
                  tags$hr(style="border-top: 1px solid black;"),
                  HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                  checkboxInput("nma1_pwe_jags_setting","Show advanced settings",value = FALSE),
                  conditionalPanel(
                    condition = "input.nma1_pwe_jags_setting == true",
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for MCMC</b>"),
                    numericInput("pwe_nchains","number of Markov chains",value = 3),
                    numericInput("pwe_niter","number of total iterations per chain (including burn in",value = 20000),
                    numericInput("pwe_nburnin","length of burn in",value = 10000),
                    numericInput("pwe_nthin","thinning rate",value = 3),
                    HTML("<b>Advanced settings for priors</b>"),
                    numericInput("pwe_pmean","priors of mu[]",value = 0),
                    numericInput("pwe_pprec","priors of T[]",value = 0.0001)
                  )
                )
                ),
                mainPanel(
                    conditionalPanel(
                      condition = "input.pwe1_pwe2 == 1",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_haz",
                                 HTML("<b>Hazard plot for PWE1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_haz"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_haz",
                                 actionButton("nma1_pwe1_plot_haz_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_haz_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_haz_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_haz_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_haz", "", icon = icon("download"))
                                 )
                               ),
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_surv",
                                 HTML("<b>Survival plot for PWE1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_surv"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_surv",
                                 actionButton("nma1_pwe1_plot_surv_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_surv_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_surv_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_surv_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_surv", "", icon = icon("download"))
                                 )
                               ),
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_hr",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for PWE1 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_hr"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_hr",
                                 actionButton("nma1_pwe1_plot_hr_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_hr_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_hr_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_hr_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_hr", "", icon = icon("download"))
                                 )
                               ),
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2 == 1",
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff",
                        HTML("<b>Coefficient Table for PWE1 (Frequentist analysis:Fixed effect model)</b>")
                      ),
                      DTOutput("nma1_pwe1_fix_coeff"),
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff",
                        # uiOutput("copy_nma1_pwe1_fix_coeff")
                       downloadButton("copy_nma1_pwe1_fix_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2 == 2",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_haz",
                                 HTML("<b>Hazard plot for PWE2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_haz"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_haz",
                                 actionButton("nma1_pwe2_plot_haz_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_haz_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_haz_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_haz_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_haz", "", icon = icon("download"))
                                 )
                               )
                               
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_surv",
                                 HTML("<b>Survival plot for PWE2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_surv"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_surv",
                                 actionButton("nma1_pwe2_plot_surv_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_surv_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_surv_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_surv_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_surv", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_hr",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for PWE2 (Frequentist analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_hr"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_hr",
                                 actionButton("nma1_pwe2_plot_hr_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_hr_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_hr_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_hr_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_hr", "", icon = icon("download"))
                                 )
                               ),
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2 == 2",
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff",
                        HTML("<b>Coefficient Table for PWE2 (Frequentist analysis:Fixed effect model)</b>")
                      ),
                      DTOutput("nma1_pwe2_fix_coeff"),
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff",
                        # uiOutput("copy_nma1_pwe2_fix_coeff")
                        downloadButton("copy_nma1_pwe2_fix_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2_bay == 1",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_haz_b",
                                 HTML("<b>Hazard plot for PWE1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_haz_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_haz_b",
                                 actionButton("nma1_pwe1_plot_haz_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_haz_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_haz_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_haz_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_haz_b", "", icon = icon("download"))
                                 )
                               )
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_surv_b",
                                 HTML("<b>Survival plot for PWE1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_surv_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_surv_b",
                                 actionButton("nma1_pwe1_plot_surv_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_surv_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_surv_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_surv_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_surv_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_hr_b",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for PWE1 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe1_plot_hr_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe1_plot_hr_b",
                                 actionButton("nma1_pwe1_plot_hr_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe1_plot_hr_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe1_plot_hr_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe1_plot_hr_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe1_plot_hr_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2_bay == 1",
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff_b",
                        HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                        textOutput("nma1_pwe1_fix_DIC_b")),
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff_b",
                        HTML("<b>Coefficient Table for PWE1 (Bayesian analysis:Fixed effect model)</b>")
                      ),
                      DTOutput("nma1_pwe1_fix_coeff_b"),
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff_b",
                        # uiOutput("copy_nma1_pwe1_fix_coeff_b")
                        downloadButton("copy_nma1_pwe1_fix_coeff_b", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      ),
                      DTOutput("nma1_pwe1_fix_coeff_b_mu"),
                      conditionalPanel(
                        condition = "output.nma1_pwe1_fix_coeff_b_mu",
                        # uiOutput("copy_nma1_pwe1_fix_coeff_b_mu")
                        downloadButton("copy_nma1_pwe1_fix_coeff_b_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2_bay == 2",
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_haz_b",
                                 HTML("<b>Hazard plot for PWE2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_haz_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_haz_b",
                                 actionButton("nma1_pwe2_plot_haz_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_haz_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_haz_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_haz_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_haz_b", "", icon = icon("download"))
                                 )
                               )
                               
                        ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_surv_b",
                                 HTML("<b>Survival plot for PWE2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_surv_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_surv_b",
                                 actionButton("nma1_pwe2_plot_surv_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_surv_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_surv_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_surv_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_surv_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_hr_b",
                                 HTML("<b>Hazard Ratio plot vs reference treatment for PWE2 (Bayesian analysis:Fixed effect model)</b>")),
                               plotOutput("nma1_pwe2_plot_hr_b"),
                               conditionalPanel(
                                 condition = "output.nma1_pwe2_plot_hr_b",
                                 actionButton("nma1_pwe2_plot_hr_b_dw", "Download plot", icon("download"),
                                              style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                 conditionalPanel(
                                   condition = "input.nma1_pwe2_plot_hr_b_dw",
                                   p(),
                                   uiOutput("nma1_pwe2_plot_hr_b_titlebut"),
                                   fluidRow(
                                     column(width = 5,
                                            uiOutput("nma1_pwe2_plot_hr_b_sizebut")
                                     )
                                   ),
                                   downloadButton("downloadPlot_nma1_pwe2_plot_hr_b", "", icon = icon("download"))
                                 )
                               )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pwe1_pwe2_bay == 2",
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff_b",
                        HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                        textOutput("nma1_pwe2_fix_DIC_b")),
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff_b",
                        HTML("<b>Coefficient Table for PWE2 (Bayesian analysis:Fixed effect model)</b>")
                      ),
                      DTOutput("nma1_pwe2_fix_coeff_b"),
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff_b",
                        # uiOutput("copy_nma1_pwe2_fix_coeff_b")
                        downloadButton("copy_nma1_pwe2_fix_coeff_b", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      ),
                      DTOutput("nma1_pwe2_fix_coeff_b_mu"),
                      conditionalPanel(
                        condition = "output.nma1_pwe2_fix_coeff_b_mu",
                        # uiOutput("copy_nma1_pwe2_fix_coeff_b_mu")
                        downloadButton("copy_nma1_pwe2_fix_coeff_b_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                      )
                    )
                  )
                )
      #----
      ),
      tabItem(tabName = "one4",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Parametric survival model (PSM)"))%>%
                    helper(type = "markdown", content = "table1"),
                  hr(),
                  div(style = "height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 8px;",
                      HTML("<b>Read first (Some notes for PSM NMA)</b>"),
                  p("In the part of Parametric survival model, we only include Bayesian analysis (Fixed effect model and random effect model).
                  Models including Weibull, Gompertz, log-logistic, log-normal are considered.
                    Please note that FP1 with power=0 is equal to Weibull, while FP1 with power=1 is equal to Gompertz."),
                  HTML("<u>Use the Difference in Beta and the Beta of reference treatment to calculate other Beta values. 
                  Then, hazard over time for each of the interventions can be calculated through the function with Beta. 
                  In addition, through d, Hazard Ratio between selected treatments can be calculated.
                  This process can also be realized easily through EXCEL.</u><br><br>"),
                  HTML("<b>For Example:</b> For reference treatment a and intervention b, we construct a PSM Weibull model.
                       We get d<sub>0ab</sub>, d<sub>1ab</sub>, Beta<sub>0a</sub>, Beta<sub>1a</sub>, 
                       Beta<sub>0b</sub>, Beta<sub>1b</sub>.<br>
                       Note:Beta<sub>0b</sub>=Beta<sub>0a</sub>+d<sub>0ab</sub>;Beta<sub>1b</sub>=Beta<sub>1a</sub>+d<sub>1ab</sub>.<br>
                       Thus, Log(HR<sub>ab</sub>(t))=d<sub>0ab</sub>+d<sub>1ab</sub>*log(t),<br>
                       Log(Hazard<sub>a</sub>(t))=Beta<sub>0a</sub>+Beta<sub>1a</sub>*log(t),<br>
                       Log(Hazard<sub>b</sub>(t))=Beta<sub>0b</sub>+Beta<sub>1b</sub>*log(t).<br>
                       Note:Beta<sub>0a</sub>, Beta<sub>0b</sub> are usually calculated as average from 
                       study specific estimates(μ0, μ1, μ2) of the reference treatment.<br>
                       "),
                  p(""),
                  HTML("<b>Formula for four distributions:</b> <br>
                      <b>Weibull:</b>f(t)=a+b</sub>*log(t)<br>
                      <b>Gompertz:</b>f(t)=a+b</sub>*t<br>
                      <b>Log-Logistic:</b>f(t)=log((exp(b)/exp(a))*((t/exp(a))^(exp(b)-1))/(1+(t/exp(a))^exp(b)))<br>
                      <b>Log-Normal:</b>f(t)=log(((2*pai)^(-0.5))*exp(-(((log(t)-a)/exp(b))^2)*0.5) / (exp(b)*t*pnorm(-(log(t)-a)/exp(b))))<br>"),
                  ),
                  p("For more information about methodology please refer to the following article:"),
                  p(""),
                  tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1002/jrsm.25", "[1]Network meta-analysis of parametric survival curves; Ouwens MJ et.al. (2010)", target = "_blank"),br(),
                  tags$a(href = "https://doi.org/10.1371/journal.pone.0184423", "[2]Cabozantinib versus everolimus, nivolumab, axitinib, sorafenib and best supportive care: A network meta-analysis of progression-free survival and overall survival in second line treatment of advanced renal cell carcinoma; Amzal B et.al. (2017)", target = "_blank"),br(),
                  tags$a(href = "https://doi.org/10.1371/journal.pone.0155389", "[3]Nivolumab versus Cabozantinib: Comparing Overall Survival in Metastatic Renal Cell Carcinoma; Wiecek W et.al. (2016)", target = "_blank"),br(),
                  
                  p(""), 
                  hr(),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                    textInput("nma1_psm_refs","Reference Study",value = "Kim2008"),
                    textInput("nma1_psm_reft","Reference Treatment",value = "Docetaxel"),
                    numericInput("nma1_psm_ex","Extrapolation time (Year)",value = 5,min = 0),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                    p(""),
                    selectInput("choose_psm","Choose specific parametric survival model",choices = c(
                      "Weibull"=1,
                      "Gompertz"=2,
                      "log-logistic"=3,
                      "log-normal"=4,
                      "Choose one model"=5
                    ),selected = 5),
                    selectInput("choose_fr","Choose fixed effect or random effect model",choices = c(
                      "Fixed effect model"=1,
                      "Random effect model"=2,
                      "Choose one model"=3
                    ),selected = 3),
                    hr(),
                    actionButton("nma1_psm_run", label="Run the model",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "output.df_nma_ad",
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                    checkboxInput("nma1_psm_jags_setting","Show advanced settings",value = FALSE),
                    conditionalPanel(
                      condition = "input.nma1_psm_jags_setting == true",
                      tags$hr(style="border-top: 1px solid black;"),
                      HTML("<b>Advanced settings for MCMC</b>"),
                      numericInput("psm_nchains","number of Markov chains",value = 3),
                      numericInput("psm_niter","number of total iterations per chain (including burn in",value = 20000),
                      numericInput("psm_nburnin","length of burn in",value = 10000),
                      numericInput("psm_nthin","thinning rate",value = 3),
                      HTML("<b>Advanced settings for priors</b>"),
                      numericInput("psm_pmean","priors of mu[]",value = 0),
                      numericInput("psm_pprec","priors of T[]",value = 0.0001),
                      numericInput("psm_pr","priors of R[]",value = 0.01)
                    )
                  )
                ),
                mainPanel(
                  conditionalPanel(
                    condition = "input.choose_psm == 1 & input.choose_fr == 1",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_1",
                               HTML("<b>Hazard plot for Weibull (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_1"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_1",
                               actionButton("nma1_psm1_plot_haz_1_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_1_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_1_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_1_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_1", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_1",
                               HTML("<b>Survival plot for Weibull (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_1"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_1",
                               actionButton("nma1_psm1_plot_surv_1_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_1_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_1_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_1_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_1", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_1",
                               HTML("<b>Hazard Ratio plot vs reference treatment for Weibull (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_1"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_1",
                               actionButton("nma1_psm1_plot_hr_1_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_1_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_1_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_1_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_1", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_1",
                      HTML("<b>Goodness-of-fit estimates (Weibull (Fixed effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_1")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_1",
                      HTML("<b>Coefficient Table for Weibull (Fixed effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_1"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_1",
                      # uiOutput("copy_nma1_psm1_fix_coeff_1")
                      downloadButton("copy_nma1_psm1_fix_coeff_1", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_1_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_1_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_1_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_1_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 1 & input.choose_fr == 2",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_2",
                               HTML("<b>Hazard plot for Weibull (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_2"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_2",
                               actionButton("nma1_psm1_plot_haz_2_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_2_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_2_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_2_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_2", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_2",
                               HTML("<b>Survival plot for Weibull (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_2"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_2",
                               actionButton("nma1_psm1_plot_surv_2_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_2_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_2_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_2_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_2", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_2",
                               HTML("<b>Hazard Ratio plot vs reference treatment for Weibull (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_2"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_2",
                               actionButton("nma1_psm1_plot_hr_2_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_2_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_2_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_2_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_2", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_2",
                      HTML("<b>Goodness-of-fit estimates (Weibull (Random effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_2")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_2",
                      HTML("<b>Coefficient Table for Weibull (Random effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_2"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_2",
                      # uiOutput("copy_nma1_psm1_fix_coeff_2")
                      downloadButton("copy_nma1_psm1_fix_coeff_2", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_2_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_2_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_2_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_2_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 2 & input.choose_fr == 1",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_3",
                               HTML("<b>Hazard plot for Gompertz (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_3"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_3",
                               actionButton("nma1_psm1_plot_haz_3_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_3_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_3_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_3_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_3", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_3",
                               HTML("<b>Survival plot for Gompertz (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_3"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_3",
                               actionButton("nma1_psm1_plot_surv_3_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_3_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_3_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_3_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_3", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_3",
                               HTML("<b>Hazard Ratio plot vs reference treatment for Gompertz (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_3"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_3",
                               actionButton("nma1_psm1_plot_hr_3_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_3_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_3_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_3_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_3", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_3",
                      HTML("<b>Goodness-of-fit estimates (Gompertz (Fixed effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_3")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_3",
                      HTML("<b>Coefficient Table for Gompertz (Fixed effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_3"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_3",
                      # uiOutput("copy_nma1_psm1_fix_coeff_3")
                      downloadButton("copy_nma1_psm1_fix_coeff_3", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_3_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_3_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_3_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_3_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 2 & input.choose_fr == 2",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_4",
                               HTML("<b>Hazard plot for Gompertz (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_4"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_4",
                               actionButton("nma1_psm1_plot_haz_4_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_4_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_4_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_4_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_4", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_4",
                               HTML("<b>Survival plot for Gompertz (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_4"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_4",
                               actionButton("nma1_psm1_plot_surv_4_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_4_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_4_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_4_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_4", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_4",
                               HTML("<b>Hazard Ratio plot vs reference treatment for Gompertz (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_4"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_4",
                               actionButton("nma1_psm1_plot_hr_4_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_4_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_4_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_4_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_4", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_4",
                      HTML("<b>Goodness-of-fit estimates (Gompertz (Random effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_4")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_4",
                      HTML("<b>Coefficient Table for Gompertz (Random effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_4"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_4",
                      # uiOutput("copy_nma1_psm1_fix_coeff_4")
                      downloadButton("copy_nma1_psm1_fix_coeff_4", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_4_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_4_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_4_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_4_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 3 & input.choose_fr == 1",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_5",
                               HTML("<b>Hazard plot for loglogistic (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_5"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_5",
                               actionButton("nma1_psm1_plot_haz_5_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_5_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_5_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_5_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_5", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_5",
                               HTML("<b>Survival plot for loglogistic (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_5"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_5",
                               actionButton("nma1_psm1_plot_surv_5_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_5_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_5_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_5_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_5", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_5",
                               HTML("<b>Hazard Ratio plot vs reference treatment for loglogistic (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_5"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_5",
                               actionButton("nma1_psm1_plot_hr_5_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_5_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_5_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_5_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_5", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_5",
                      HTML("<b>Goodness-of-fit estimates (loglogistic (Fixed effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_5")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_5",
                      HTML("<b>Coefficient Table for loglogistic (Fixed effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_5"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_5",
                      # uiOutput("copy_nma1_psm1_fix_coeff_5")
                      downloadButton("copy_nma1_psm1_fix_coeff_5", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_5_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_5_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_5_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_5_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 3 & input.choose_fr == 2",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_6",
                               HTML("<b>Hazard plot for loglogistic (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_6"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_6",
                               actionButton("nma1_psm1_plot_haz_6_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_6_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_6_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_6_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_6", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_6",
                               HTML("<b>Survival plot for loglogistic (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_6"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_6",
                               actionButton("nma1_psm1_plot_surv_6_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_6_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_6_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_6_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_6", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_6",
                               HTML("<b>Hazard Ratio plot vs reference treatment for loglogistic (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_6"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_6",
                               actionButton("nma1_psm1_plot_hr_6_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_6_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_6_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_6_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_6", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_6",
                      HTML("<b>Goodness-of-fit estimates (loglogistic (Random effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_6")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_6",
                      HTML("<b>Coefficient Table for loglogistic (Random effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_6"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_6",
                      # uiOutput("copy_nma1_psm1_fix_coeff_6")
                      downloadButton("copy_nma1_psm1_fix_coeff_6", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_6_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_6_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_6_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_6_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 4 & input.choose_fr == 1",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_7",
                               HTML("<b>Hazard plot for lognormal (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_7"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_7",
                               actionButton("nma1_psm1_plot_haz_7_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_7_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_7_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_7_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_7", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_7",
                               HTML("<b>Survival plot for lognormal (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_7"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_7",
                               actionButton("nma1_psm1_plot_surv_7_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_7_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_7_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_7_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_7", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_7",
                               HTML("<b>Hazard Ratio plot vs reference treatment for lognormal (Fixed effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_7"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_7",
                               actionButton("nma1_psm1_plot_hr_7_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_7_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_7_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_7_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_7", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_7",
                      HTML("<b>Goodness-of-fit estimates (lognormal (Fixed effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_7")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_7",
                      HTML("<b>Coefficient Table for lognormal (Fixed effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_7"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_7",
                      # uiOutput("copy_nma1_psm1_fix_coeff_7")
                      downloadButton("copy_nma1_psm1_fix_coeff_7", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_7_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_7_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_7_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_7_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.choose_psm == 4 & input.choose_fr == 2",
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_8",
                               HTML("<b>Hazard plot for lognormal (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_haz_8"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_haz_8",
                               actionButton("nma1_psm1_plot_haz_8_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_haz_8_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_haz_8_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_haz_8_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_haz_8", "", icon = icon("download"))
                               )
                             ),
                      ),
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_8",
                               HTML("<b>Survival plot for lognormal (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_surv_8"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_surv_8",
                               actionButton("nma1_psm1_plot_surv_8_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_surv_8_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_surv_8_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_surv_8_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_surv_8", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_8",
                               HTML("<b>Hazard Ratio plot vs reference treatment for lognormal (Random effect model)</b>")),
                             plotOutput("nma1_psm1_plot_hr_8"),
                             conditionalPanel(
                               condition = "output.nma1_psm1_plot_hr_8",
                               actionButton("nma1_psm1_plot_hr_8_dw", "Download plot", icon("download"),
                                            style="color: black;
                       background-color: white; border-color: #2e6da4"),
                               conditionalPanel(
                                 condition = "input.nma1_psm1_plot_hr_8_dw",
                                 p(),
                                 uiOutput("nma1_psm1_plot_hr_8_titlebut"),
                                 fluidRow(
                                   column(width = 5,
                                          uiOutput("nma1_psm1_plot_hr_8_sizebut")
                                   )
                                 ),
                                 downloadButton("downloadPlot_nma1_psm1_plot_hr_8", "", icon = icon("download"))
                               )
                             ),
                      )
                    ),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_8",
                      HTML("<b>Goodness-of-fit estimates (lognormal (Random effect model))</b>"),
                      textOutput("nma1_psm1_fix_DIC_8")),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_8",
                      HTML("<b>Coefficient Table for lognormal (Random effect model)</b>")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_8"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_8",
                      # uiOutput("copy_nma1_psm1_fix_coeff_8")
                      downloadButton("copy_nma1_psm1_fix_coeff_8", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    DTOutput("nma1_psm1_fix_coeff_8_mu"),
                    conditionalPanel(
                      condition = "output.nma1_psm1_fix_coeff_8_mu",
                      # uiOutput("copy_nma1_psm1_fix_coeff_8_mu")
                      downloadButton("copy_nma1_psm1_fix_coeff_8_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    )
                  )
                )
              )
              #----
      ),
      tabItem(tabName = "two1", 
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Import Data (IPD-based NMA)")),
                  p(""),
                  fileInput("upload2", "Load Excel file (.xlsx)", accept = c(".xlsx")) %>%
                    helper(type = "markdown", content = "upload"),
                  p(""),
                  actionButton("downloadtemplate2", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                               onclick = "location.href='files/example2.xlsx'"),
                  p(""),
                  actionButton("FCexample2", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                  p(""),
                  hr(),
                  p("The example IPD here is obtained from Freeman SC et.al. (2022). This melanoma network included 13 treatments and 13 studies. 
                    Treatment 1 is the reference treatment.", strong("In this APP, the reference treatment should be set as treatment 1 (code 1)."),
                    "The IPD input here can be obtained from published survival curves through Guyot.et.al (2012)'s method.
                    For more information please refer to the following two articles:"),
                  p(""),
                  tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/09622802211070253?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", "[1]Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network; Freeman SC et.al. (2022)", target = "_blank"),br(),
                  tags$a(href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-9", "[2]Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves; Guyot.et.al (2012)", target = "_blank"),br(),
                  conditionalPanel(
                    condition = "output.df_nma2",
                    tags$hr(style="border-top: 1px solid black;"),
                    actionButton("ready_rmi1", label="Move to COX PH model",icon("play"),style="color: black;
                       background-color: white; border-color: #2e6da4"),br(),
                    p(),
                    actionButton("ready_rmi2", label="Move to Generalised Gamma Model",icon("play"),style="color: black;
                       background-color: white; border-color: #2e6da4"),br()
                  )
                ),
                mainPanel(
                  HTML("<b>IPD data used in NMA</b>"),
                  DTOutput("df_nma2"),
                  HTML("<b>Included treatments and studies, and reference numbers</b>"),
                  fluidRow(
                    column(width = 6,DTOutput("df_nma_ipd_t")),
                    column(width = 6,DTOutput("df_nma_ipd_s")))
                  )
              )
      ),
              #----
      tabItem(tabName = "two2",
              #----
              sidebarLayout(
                sidebarPanel(
                  h3(em("COX PH model"))%>%
                    helper(type = "markdown", content = "table1"),
                  hr(),
                  p("In the part of COX PH model, we only include Bayesian analysis (Fixed effect model).
                  Treatment 1 (Code 1) is considered as the reference treatment. 
                  Changing the reference treatment can be realized through modifying the data.
                  The Cox PH model was fitted using a two-stage approach according to the method of Freeman SC et.al. (2022). 
                  In the first stage a Cox PH model was fitted individually to each trial to obtain an estimate of the log HR for the 
                  treatment effect and its corresponding standard error.
                  In the second stage, the treatment effect estimate was synthesised through a standard fixed effect NMA model."),
                  p("This APP will provide the results of HR (compared with the reference treatment).
                  When HR is obtained, the user can calculate the cumulative hazard for reference treatment (written as cumhaz_ref).
                  Then, multiplying HR with the cumhaz_ref, the cumulative hazard for each treatment (written as cumhaz) can be obtained.
                  Finally, through formula 'exp(-cumhaz)', the survival rate can be calculated.
                    For more information about methodology please refer to the following article:"),
                  p(""),
                  tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/09622802211070253?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", "[1]Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network; Freeman SC et.al. (2022)", target = "_blank"),br(),
                  tags$a(href = "https://link.springer.com/book/10.1007/978-1-4757-3294-8", "[2]Modeling Survival Data: Extending the Cox Model; Terry M et.al. (2000)", target = "_blank"),br(),
                  p(""), 
                  # hr(),
                  conditionalPanel(
                    condition = "output.df_nma2",
                    # textInput("nma1_cph_bugs_d","Bugs Directory",value = "D:/WinBUGS14"),
                    # textInput("nma1_cph_work_d","Code Directory",value = "C:/Users/Administrator/Desktop/APPtest-download/FE_model_cph.txt"),
                    # p("Copy the following code to a txt file named 'FE_model_cph.txt' and put it in the Code Directory you have just input."),
                    # uiOutput("download_cox_ph"),
                    # downloadButton("download_cox_ph", "Copy Winbugs Code",icon("download"),style="color: black;
                    #    background-color: white; border-color: #2e6da4"),
                    # numericInput("nma1_cph_ex","Extrapolation time (Year)",value = 5,min = 0),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                    p(""),
                    # hr(),
                    actionButton("run_nma_coxph", label="Run the model",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "output.df_nma2",
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for JAGS (Bayesian analysis)</b>"),
                    checkboxInput("nma1_cph_jags_setting","Show advanced settings",value = FALSE),
                    conditionalPanel(
                      condition = "input.nma1_cph_jags_setting == true",
                      tags$hr(style="border-top: 1px solid black;"),
                      HTML("<b>Advanced settings for MCMC</b>"),
                      p("Number of Markov chains was set 3 in this model."),
                      # numericInput("cph_nchains","number of Markov chains",value = 3),
                      numericInput("cph_sim","simulation size",value = 6000),
                      numericInput("cph_nburnin","length of burn in",value = 6000),
                      HTML("<b>Advanced settings for priors</b>"),
                      numericInput("cph_d1","priors of d1[]",value = 0),
                      numericInput("cph_d2","priors of d2[]",value = 0.1),
                      numericInput("cph_d3","priors of d3[]",value = -0.1)
                    )
                  )
                ),
                mainPanel(
                    conditionalPanel(
                      condition = "output.df_nma2_res_hr",
                      HTML("<b>HR for COX PH model (Fixed effect model, compared to treatment 1)</b>")
                    ),
                    DTOutput("df_nma2_res_hr"),
                    conditionalPanel(
                      condition = "output.df_nma2_res_hr",
                      # uiOutput("copy_output.df_nma2_res_hr")
                      downloadButton("copy_output.df_nma2_res_hr", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                    ),
                    # conditionalPanel(
                    #   condition = "output.df_nma2_res_rank",
                    #   HTML("<b>Treatment effect for COX PH model (Fixed effect model, compared to treatment 1)</b>")
                    # ),
                    # DTOutput("df_nma2_res_rank"),
                    # conditionalPanel(
                    #   condition = "output.df_nma2_res_rank",
                    #   # uiOutput("copy_output.df_nma2_res_rank")
                    #   downloadButton("copy_output.df_nma2_res_rank", label="Copy this table",style="color: black;
                    #    background-color: white; border-color: #2e6da4")
                    # )
                )
              )
              #----
      ),
      tabItem(tabName = "two3",
              # ----
              sidebarLayout(
                sidebarPanel(
                  h3(em("Generalised Gamma Model"))%>%
                    helper(type = "markdown", content = "table1"),
                  hr(),
                  p("Similar to COX PH Model, in the part of Generalised Gamma Model, we only include Bayesian analysis (Fixed effect model).
                  Treatment 1 (Code 1) is considered as the reference treatment.
                  Changing the reference treatment can be realized through modifying the data.
                  The generalised gamma model was fitted using a two-stage process according to the method of Freeman SC et.al. (2022). 
                  In the first stage, each trial was analysed separately using the generalised gamma model to obtain estimates of the log hazard ratio for the treatment effect and the corresponding standard error. 
                  Initially, each trial was independently evaluated by employing the generalized gamma model. 
                  This approach facilitated the derivation of log hazard ratio estimates pertinent to the treatment effect, alongside the corresponding standard errors.
                  In the second stage, the treatment effect estimate of the baseline treatment compared to treatment i in trial j was synthesised and its variability was esitmated within a standard fixed effect NMA model."),
                  p("This APP will provide the results of Treatment Effects (TE) (compared with the reference treatment).
                  The user can use 'flexsurv' package to calculate the coefficicents (including 'mu', 'sigma' and 'q') of applying Generalised Gamma Model on the reference treatment.
                  When TE is obtained, the user can calculate the survival rate (S(t)) through the coefficicents and TE.
                  Formula can be written as : S(t)n = 1 - pgengamma(Time, mu = mu + TEn, sigma = sigma, Q=q, ...);
                  Similarly, hazard (h(t)) can be written as : h(t) = hgengamma(Time, mu = mu + TEn, sigma = sigma, Q=q).
                  For more information about methodology please refer to the following article:"),
                  p(""),
                  tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/09622802211070253?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", "[1]Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network; Freeman SC et.al. (2022)", target = "_blank"),br(),
                  tags$a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.2375", "[2]Generalized gamma frailty model; Balakrishnan N et.al. (2006)", target = "_blank"),br(),
                  tags$a(href = "https://www.jstatsoft.org/article/view/v070i08", "[3]flexsurv: A Platform for Parametric Survival Modeling in R; Jackson CH (2016)", target = "_blank"),br(),
                  p(""), 
                  # hr(),
                  conditionalPanel(
                    condition = "output.df_nma2",
                    # textInput("nma1_geng_bugs_d","Bugs Directory",value = "D:/WinBUGS14"),
                    # textInput("nma1_geng_work_d","Working Directory",value = "C:/Users/Administrator/Desktop/APPtest-download/FE_model_gg.txt"),
                    # p("Copy the following code to a txt file named 'FE_model_gg.txt' and put it in the Code Directory you have just input."),
                    # uiOutput("download_gengamma"),
                    # actionButton("download_gengamma", "Copy Winbugs Code",icon("download"),style="color: black;
                    #    background-color: white; border-color: #2e6da4"),
                    # numericInput("nma1_geng_ex","Extrapolation time (Year)",value = 5,min = 0),
                    p(""),
                    tags$hr(style="border-top: 1px solid black;"),
                    p(""),
                    # hr(),
                    actionButton("run_nma_gengamma", label="Run the model",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4")
                  ),
                  conditionalPanel(
                    condition = "output.df_nma2",
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for JAGS (Bayesian analysis)</b>"),
                    checkboxInput("nma1_geng_jags_setting","Show advanced settings",value = FALSE),
                    conditionalPanel(
                      condition = "input.nma1_geng_jags_setting == true",
                      tags$hr(style="border-top: 1px solid black;"),
                      HTML("<b>Advanced settings for MCMC</b>"),
                      p("Number of Markov chains was set 3 in this model."),
                      # numericInput("geng_nchains","number of Markov chains",value = 3),
                      numericInput("geng_sim","simulation size",value = 6000),
                      numericInput("geng_nburnin","length of burn in",value = 6000),
                      HTML("<b>Advanced settings for priors</b>"),
                      numericInput("geng_d1","priors of init1[]",value = -0.5),
                      numericInput("geng_d2","priors of init2[]",value = 0.5),
                      numericInput("geng_d3","priors of init3[]",value = 0.1)
                    )
                  )
                ),
                mainPanel(
                  conditionalPanel(
                    condition = "output.df_nma3_res_hr",
                    HTML("<b>Treatment effect for Generalized Gamma Model (Fixed effect model, compared to treatment 1)</b>")
                  ),
                  DTOutput("df_nma3_res_hr"),
                  conditionalPanel(
                    condition = "output.df_nma3_res_hr",
                    # uiOutput("copy_output.df_nma3_res_hr")
                    downloadButton("copy_output.df_nma3_res_hr", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                  ),
                  # conditionalPanel(
                  #   condition = "output.df_nma3_res_rank",
                  #   HTML("<b>The probability to be the best</b>")
                  # ),
                  # DTOutput("df_nma3_res_rank"),
                  # conditionalPanel(
                  #   condition = "output.df_nma3_res_rank",
                  #   # uiOutput("copy_output.df_nma3_res_rank")
                  #   downloadButton("copy_output.df_nma3_res_rank", label="Copy this table",style="color: black;
                  #      background-color: white; border-color: #2e6da4")
                  # )
                )
              )
              # ----
      )
      
      
      
# Do not modify the below      
    )
  )
)

