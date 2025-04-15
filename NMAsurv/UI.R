##### NMAsurv #####

cat("\n Welcome to this APP! \n")

# Interface
dashboardPage(
  # skin = "blue",
  dashboardHeader(dropdownMenu(type = "notifications", badgeStatus = "warning",headerText="If the content is not displayed well, 
                               you can adjust the zoom ratio of the web page display.
                               Taking developers' computer as an example, 
                               the monitor size is 27 inches and the resolution is 2560*1440, 
                               a suitable zoom ratio will be 100% to 125%."
                    )
                  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    rclipboardSetup(),
    withMathJax(),
    useShinyjs(),
    useShinyalert(force = TRUE),
    
    #### Tags Panel ####
    tags$head(
      tags$style(HTML("
      .scrollable-panel {
        max-height: 200px;
        overflow-y: auto;
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
      .shiny-split-layout>div {
        overflow: hidden;
      }
        
      .tabbable > .nav > li > a {
        background-color: #006699;
        color:white
      }
      .tabbable > .nav > li[class=active] > a {
        background-color: #FF9966;
        font-weight: 900;
        font-style: italic;
        text-decoration: underline;
      }
      .tabbable > .nav > li > a[data-value='datapage1'],
      .tabbable > .nav > li > a[data-value='tab_show_ad'],
      .tabbable > .nav > li > a[data-value='datapage3'],
      .tabbable > .nav > li > a[data-value='show_data_transform_panel'],
      .tabbable > .nav > li > a[data-value='fppage21'],
      .tabbable > .nav > li > a[data-value='fppage22'],
      .tabbable > .nav > li > a[data-value='fppage23'],
      .tabbable > .nav > li > a[data-value='fppage24'],
      .tabbable > .nav > li > a[data-value='pwepage21'],
      .tabbable > .nav > li > a[data-value='pwepage22'],
      .tabbable > .nav > li > a[data-value='pwepage23'],
      .tabbable > .nav > li > a[data-value='pwepage24'],
      .tabbable > .nav > li > a[data-value='pwepage25'],
      .tabbable > .nav > li > a[data-value='pwepage26'],
      .tabbable > .nav > li > a[data-value='psmpage21'],
      .tabbable > .nav > li > a[data-value='psmpage22'],
      .tabbable > .nav > li > a[data-value='psmpage23'],
      .tabbable > .nav > li > a[data-value='psmpage24'],
      .tabbable > .nav > li > a[data-value='psmpage25'],
      .tabbable > .nav > li > a[data-value='psmpage26'],
      .tabbable > .nav > li > a[data-value='psmpage27'],
      .tabbable > .nav > li > a[data-value='psmpage28'],
      .tabbable > .nav > li > a[data-value='psmpage29'],
      .tabbable > .nav > li > a[data-value='psmpage210'],
      .tabbable > .nav > li > a[data-value='psmpage211'],
      .tabbable > .nav > li > a[data-value='psmpage212'],
      .tabbable > .nav > li > a[data-value='outputpage21'],
      .tabbable > .nav > li > a[data-value='outputpage22'] {
        background-color: #009999;
        color: white;
      }

      .tabbable > .nav > li > a[data-value='fppage31'],
      .tabbable > .nav > li > a[data-value='fppage32'],
      .tabbable > .nav > li > a[data-value='fppage33'],
      .tabbable > .nav > li > a[data-value='fppage34'],
      .tabbable > .nav > li > a[data-value='fppage35'],
      .tabbable > .nav > li > a[data-value='fppage36'],
      .tabbable > .nav > li > a[data-value='fppage37'],
      .tabbable > .nav > li > a[data-value='fppage38'],
      .tabbable > .nav > li > a[data-value='fppage39'],
      .tabbable > .nav > li > a[data-value='fppage310'],
      .tabbable > .nav > li > a[data-value='fppage311'],
      .tabbable > .nav > li > a[data-value='fppage312'] {
        background-color: #0099cc;
        color: white;
      }
    ")),
      tags$style(HTML("
      #baseline table.dataTable {
        border: 1px solid black; 
      }
    ")),
      tags$style(HTML("
      .custom-h1 {
        font-size: 2em; 
        font-weight: bold;
        font-family: 'Lucida Sans Unicode', sans-serif, 'Arial';
        color: black
      }
      .custom-h3 {
        font-size: 1.6em; 
        font-weight: bold;
        font-family: 'Lucida Sans Unicode', sans-serif, 'Arial';
        color: black
      }
      .custom-h4 {
        font-size: 1.3em; 
        font-weight: bold;
        font-family: 'Lucida Sans Unicode', sans-serif, 'Arial';
        text-decoration: underline; 
        color: black
      }
      .custom-h5 {
        font-size: 1.2em; 
        font-family: 'Calibri','Lucida Sans Unicode', sans-serif, 'Arial';
        text-decoration: underline; 
        color: black
      }
    ")),
      tags$style(HTML("
      .dashboard-title {
        background-color: white; 
        border-radius: 5px;
        padding: 10px;
      }
    ")),
      tags$style(HTML("
      .text-op {
        color: red;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
        body {
            font-family: 'Calibri','Lucida Sans Unicode', sans-serif, 'Arial';
        }
      ")),
      tags$style(HTML("
            .disabled-tab {
                color: #aaa !important; 
                pointer-events: none !important; 
            }
        ")),
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('controlTab1', function(message) {
                var tabLink = $('.navbar-nav a[data-value=\"IPD-based NMA\"]');
                if (message.enable) {
                    tabLink.removeClass('disabled-tab');
                } else {
                    tabLink.addClass('disabled-tab');
                }
            });"
      )),
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('controlTab2', function(message) {
                var tabLink = $('.navbar-nav a[data-value=\"AD-based NMA\"]');
                if (message.enable) {
                    tabLink.removeClass('disabled-tab');
                } else {
                    tabLink.addClass('disabled-tab');
                }
            });"
      )),
      tags$style(HTML("
            .disabled {
                pointer-events: none !important;
                color: #aaa !important;
            }
        ")),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"tab_show_data\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"tab_show_data\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"tab_show_data\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"tab_show_ad\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab1', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"tab_show_ad\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"tab_show_ad\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"data_transform_panel\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab2', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"data_transform_panel\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"data_transform_panel\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"show_data_transform_panel\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab3', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"show_data_transform_panel\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"show_data_transform_panel\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"ph_test_panel\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab4', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"ph_test_panel\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"ph_test_panel\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"1.2 FP Step One\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab5', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"1.2 FP Step One\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"1.2 FP Step One\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"1.3 FP Step Two (FP1)\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab6', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"1.3 FP Step Two (FP1)\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"1.3 FP Step Two (FP1)\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"1.4 FP Step Two (FP2)\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab7', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"1.4 FP Step Two (FP2)\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"1.4 FP Step Two (FP2)\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"1.2 Show the COX PH results\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab8', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"1.2 Show the COX PH results\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"Show the COX PH results\"]').addClass('disabled');
                    }
                });
            ")
      ),
      tags$script(
        HTML("
                $(document).on('shiny:connected', function(event) {
                    // Initially disable the tab
                    $('a[data-value=\"2.2 Show the Gen-Gamma results\"]').addClass('disabled');
                });

                Shiny.addCustomMessageHandler('enableTab9', function(message) {
                    if(message.enable) {
                        $('a[data-value=\"2.2 Show the Gen-Gamma results\"]').removeClass('disabled');
                    } else {
                        $('a[data-value=\"2.2 Show the Gen-Gamma results\"]').addClass('disabled');
                    }
                });
            ")
      ),
      # tags$script(
      #   HTML("
      #           $(document).on('shiny:connected', function(event) {
      #               // Initially disable the tab
      #               $('a[data-value=\"3. Output report\"]').addClass('disabled');
      #           });
      # 
      #           Shiny.addCustomMessageHandler('enableTab10', function(message) {
      #               if(message.enable) {
      #                   $('a[data-value=\"3. Output report\"]').removeClass('disabled');
      #               } else {
      #                   $('a[data-value=\"3. Output report\"]').addClass('disabled');
      #               }
      #           });
      #       ")
      # )
    ),
    
    #### End of Tags Panel ####
    
    navbarPage(
      id = "top_bar",
      title = "NMAsurv",
      
      #### Dashboard tab ####
      tabPanel(title = "Home Page",
              column(width = 12,
                     wellPanel(
                       class = "dashboard-title",
                       h1("Welcome to the tool for Network Meta-Analysis of survival data (NMAsurv)",class = "custom-h1"),
                      tags$h4(style = "font-size: 1.3em;font-family: 'Calibri','Lucida Sans Unicode', sans-serif, 'Arial';",
                      "This tool provides an open-access tool powered by R that can be used to conduct network meta-analysis (NMA) based on reconstructed survival data. 
                       In this APP, we provided several methods including Aggregated data(AD)-based NMA and Individual patient data(IPD)-based NMA. 
                       Please note that not all existing NMA methods are included in this app, so please be careful when using this APP (You can find more in the references).
                     Under many real-world situations, researchers do not have the IPD. Therefore,
                     this tool focused on reconstructed survival data only, covariates can not be included in this APP.
                     If you have survival data with covariates, we recommend you to conduct further analyses like multilevel network meta-regression or considering treatment-by-covariates interactions.
                     Please refer to the references in this app for relevant methodology and codes.
                      ")
                     ),
                     p("")
              ),
              column(width = 12,
                       h3("ABOUT",class = "custom-h3"),
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h5>BugReports: travis_shao@outlook.com</h4>"),
                       HTML("<h5>This tool was developed by:<br></h4>"),
                       HTML("<h5>Taihang Shao<sup>[1][2]</sup>, Mingye Zhao<sup>[1]</sup>, Fenghao Shi<sup>[3]</sup>, Mingjun Rui<sup>[4]</sup>, Wenxi Tang<sup>[1]</sup><br></h5>"),
                       HTML("<h5>[1] Center for Pharmacoeconomics and Outcome Research, China Pharmaceutical University </em>
                       <br> [2] School of Public Health and Primary Care, The Chinese University of Hong Kong </em>
                       <br> [3] International Research Center for Medicinal Administration, Peking University </em>
                       <br> [4] School of Pharmacy, The Chinese University of Hong Kong </em>
                          </h5>")
                     ),
              ),
              column(width = 12,
                       h3("LOG",class = "custom-h3")
              ),
              column(width = 12,
                     wellPanel(
                       class = "scrollable-panel",
                       HTML("<h5><u> Jan 21, 2025 : Version 1.5 The user interface has been greatly improved and some features have been added.</u></h5>"),
                       HTML("<div style='line-height: 1;'> 1.The user interface is more integrated, and instructions are added for each section to 
                       facilitate users to use it step by step; <br>
                       2.Color-coding is added for each section to make it easier for users to understand their usage; <br>
                            3.An additional Application Output panel is now available and users can batch export their results;<br>
                              4.Downloading extrapolated HR data is now available.<br></div>"),
                       HTML("<h5><u> June 28, 2024 : Version 1.4 PH assumption tests are available in this APP now.</u></h5>"),
                       HTML("<h5><u> May 4, 2024 : Version 1.3 Some improvements are made in this APP.</u></h5>"),
                       HTML("<div style='line-height: 1;'> 1.DIC of Parametric survival model is available; <br>
                       2.All tables can be downloaded; <br>
                            3.Winbugs is replaced by JAGS in IPD-based NMA; <br>
                            4.Results of Rank in IPD-based NMA are removed.<br></div>"),
                       HTML("<h5><u> April 30, 2024 : Version 1.2 Some improvements are made in this APP. More results are available now.</u></h5>"),
                       HTML("<div style='line-height: 1;'> 1.Advanced setting of JAGS is improved; <br>
                       2.Rhat results of Bayesian Analysis are available; <br> 
                            3.All estimates of parameters are available.<br></div>"),
                       HTML("<h5><u> April 6, 2024 : Version 1.1 Gengamma model is available now.</u></h5>"),
                       HTML("<h5><u> December 17, 2023 : Version 1.0 NMA is constructed and 4 methods are provided.</u></h5>"),
                       HTML("<div style='line-height: 1;'> 1. Fractional polynomial, Piecewise exponential model, Parametric survival model and COX-PH model are now available.<br></div>"),
                     ),
              ),
              column(width = 12,
                       h3("USEFUL REFERENCE",class = "custom-h3"),
                       HTML("<h4>If you use this tool, these references will be helpful to you:</h4>")
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
      #### End of dashboard ####

      #### Data Page ####
      tabPanel(
        title = "Data Page",
        tabsetPanel(
          
          #### Load Data Panel ####
          tabPanel(
            title = "1. Load Data",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                h4("Load (Reconstructed) Individual Patient Data",class = "custom-h4"),
                p(""),
                fileInput("upload2", "Load Excel file (.xlsx)", accept = c(".xlsx")),
                p(""),
                actionButton("downloadtemplate2", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                             onclick = "location.href='files/example2.xlsx'"),
                p(""),
                actionButton("FCexample2", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                tags$div(style = "height: 3px; background-color: gray; margin-top: 12px; margin-bottom: 12px;"),
                bsCollapse(
                  id = "collapse_ad",
                  bsCollapsePanel(
                    title = "Load AD Panel (Click to open / hide this panel)", 
                    h4("Load Aggregated Data", class = "custom-h4"),
                    p("Users can load their own aggreated data here."),
                    p(""),
                    fileInput("upload1", "Load Excel file (.xlsx)", accept = c(".xlsx")),
                    p(""),
                    actionButton("downloadtemplate1", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                                 onclick = "location.href='files/example1.xlsx'"),
                    p(""),
                    actionButton("FCexample1", label="Load example data",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                    style = "warning"
                  )
                ),
                tags$div(style = "height: 3px; background-color: gray; margin-top: 12px; margin-bottom: 12px;"),
                h5(strong("Record:")),
                textOutput("ipd_text"),
                textOutput("ad_text")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel(
                    title = "1.1 Instruction",
                    value = "instruction_data_page",
                    div(
                      wellPanel(
                        h4("Get started with importing data",class = "custom-h4"),
                        HTML("To start with this application, the users are recommended to follow the following steps:<br>
                             (1) Users should load the (reconstructed) IPD first, and they can check the input data in the <b>'Show the Input Data'</b> panel;<br>
                             <div style='color: red;'> <b>&nbsp;&nbsp; ! Users are recommended to read the notes on data format before 
                             moving on. (Below this section)</b></div>
                             <b>&nbsp;&nbsp; ! If the data is not imported, many functions will temporarily not be available for users.</b><br>
                             (2) Then, user should either transform the IPD to AD through the <b>'Data Transform'</b> panel or 
                             load their own AD directly through the <b>'Load AD'</b> panel;<br>
                             (3) Test the PH assumption through the <b>'PH assumption Test'</b> panel.
                             "),br(),
                        p(),
                        HTML("<b>A flow chart for Data Page</b>"),br(),
                        tags$img(src = "instruction_flow_chart.png", 
                                 style = "max-width: 100%; height: auto; display: block; margin: auto;"
                                 # height = "400px", width = "auto"
                                 ),br(),
                        p(),
                        HTML("Some Tips:<br>
                             (1) If users only load the AD only, they can not run the IPD-based NMA, but they can still run the AD-based NMA;<br>
                             (2) If users do not load the IPD, they can not run the PH assumption tests;<br>
                             (3) To draw the network plot, users should move to the <b>Network Plot</b> panel;<br>
                             (4) Data for network plot is different from the AD and IPD.
                             "),
                        br(),
                        hr(),
                        h4("Notes on data format (Important)",class = "custom-h4"),
                        HTML("<b>For the input IPD</b>, it should include <b>seven</b> columns: 
                        n, patid, time, event, arm, study and treatment.
                        <b>n [num]</b> is the ID of record; 
                        <b>patid [num]</b> is the ID of a patient in a trial; 
                             <b>time [num]</b> is the survival time of a patient (in months); 
                             <b>event [num]</b> refers to whether the patient is survival or dead; 
                             <b>arm [num]</b> refers to the arm of the trial; 
                             <b>study [chr]</b> refers to the study Name; 
                             <b>treatment [chr]</b> is the name of treatment."),br(),p(),
                        HTML("<b>For the input AD</b>, it should include <b>eight</b> columns: 
                        studyn, trtn, time, timeDelta, nevents, natrisk, study and treatment.
                        <b>studyn [num]</b> is the reference number of specific study; 
                             <b>trtn [num]</b> is the reference number of specific treatment;
                             <b>time [num]</b> refers the time of one specific observation (in months); 
                             <b>timeDelta [num]</b> refers to the duration of time between two consecutive observations; 
                             <b>nevents [num]</b> represents number of events; 
                             <b>natrisk [num]</b> refers to number at risk; 
                             <b>study [chr]</b> refers to the study name; 
                             <b>treatment [chr]</b> is the name of treatment. 
                             "),br(),p(),
                        HTML("<b>For the data to draw network plot</b>, it should include <b>nine</b> columns: 
                        studyCode, Study, t1, t2, treat1, treat2, hr, lower, upper.
                        <b>studyCode [num]</b> refers to the study code;
                        <b>Study [chr]</b> is the study name;
                             <b>t1,t2 [num]</b> refers to the treatment code; 
                             <b>treat1,treat2 [chr]</b> refers to the treatment name; 
                             <b>hr with lower and upper [num]</b> refers to the hazard ratio with its upper and lower limits."),
                        br(),
                        hr(),
                        h4("Notes on example data used in this APP",class = "custom-h4"),
                        HTML("The example data (both IPD and AD) used in this application is extracted from: 
                             <i>Zhao M, Shao T, Shao H, Zhou C, Tang W. Identifying optimal ALK inhibitors in 
                             first- and second-line treatment of  patients with advanced ALK-positive non-small-cell 
                             lung cancer: a systematic  review and network meta-analysis. BMC Cancer. 2024;24(1):186</i>.
                             The example data is reconstructed based on the overall survival of all the 
                             included studies in the ALK-NSCLC network. This ALK-NSCLC network included six RCTs 
                             (ALEX, CROWN, ALTA-1L, eXalt 3, PROFILE 1014, ASCEND-4) with seven treatments 
                             (crizotinib, alectinib, lorlatinib, brigatinib, ensartinib, chemotherapy, ceritinib)."), 
                        p(),
                        HTML("<b>Baseline information for ALK-NSCLC network</b>"),br(),
                        DTOutput("baseline"),
                        hr(),
                        h4("Additional example data for reference",class = "custom-h4"),
                        HTML("<b>Data of the Melanoma Network (Individual Patient Data)</b>"),br(),
                        actionButton("downloadtemplatea1", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                                     onclick = "location.href='files/Melanoma.xlsx'"),br(),
                        HTML("Reference: 
                             <i>Freeman SC, Cooper NJ, Sutton AJ, Crowther MJ, Carpenter JR, Hawkins N. 
                             Challenges of modelling approaches for network meta-analysis of time-to-event outcomes 
                             in the presence of non-proportional hazards to aid decision making: 
                             Application to a melanoma network. Stat Methods Med Res. 2022;31(5):839-861. 
                             doi:10.1177/09622802211070253</i>."),br(),p(),
                        HTML("<b>Data of the NSCLC-Che Network (Aggregated Data)</b>"),br(),
                        actionButton("downloadtemplatea2", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                                     onclick = "location.href='files/NSCLC-Che.xlsx'"),br(),
                        HTML("Reference: 
                             <i>Jansen JP. Network meta-analysis of survival data with fractional polynomials. 
                             BMC Med Res Methodol. 2011;11:61. Published 2011 May 6. 
                             doi:10.1186/1471-2288-11-61</i>."),br(),p(),
                        HTML("<b>Data of the NSCLC-PDX Network (Network Plot)</b>"),br(),
                        actionButton("downloadtemplatea3", "Download example data",icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4",
                                     onclick = "location.href='files/NSCLC-PDX.xlsx'"),br(),
                        HTML("Reference: 
                             <i>Shao T, Zhao M, Liang L, Tang W. A systematic review and network meta-analysis of 
                             first-line immune checkpoint inhibitor combination therapies in patients with 
                             advanced non-squamous non-small cell lung cancer. Front Immunol. 
                             2022;13:948597. Published 2022 Oct 26. doi:10.3389/fimmu.2022.948597</i>."),
                        hr(),
                        h4("A summary table for this APP",class = "custom-h4"),
                        HTML("<b>A summary of the five implemented methods and their data/model assumptions</b>"),br(),
                        tags$img(src = "figure_data_0.png", 
                                 style = "max-width: 100%; height: auto; display: block; margin: auto;"
                                 # height = "400px", width = "auto"
                        ),br()
                      )
                    )
                  ),
                  tabPanel(
                    title = "1.2 Show the Input Data",
                    value = "tab_show_data",
                    tabsetPanel(
                      tabPanel(
                        title = "1.21 Show the IPD",
                        value = "datapage1",
                        div(
                          wellPanel(              
                            bsCollapse(
                              id = "collapse1",open = "panel1",
                              bsCollapsePanel(
                                title = "Raw IPD Panel (Click to open / hide this panel)", 
                                DTOutput("df_nma2"),
                                style = "warning",value = "panel1"
                              )
                            ),
                          ),
                          wellPanel(
                            fluidRow(
                            column(width = 6,
                                   wellPanel(
                                     bsCollapse(
                                     id = "collapse2",
                                     bsCollapsePanel(
                                       title = "Treatment Name Panel (Click to open / hide this panel)", 
                                       DTOutput("df_nma_ipd_t"),
                                       style = "warning"
                                               )
                                             ))
                            ),
                            column(width = 6,
                                   wellPanel(bsCollapse(
                                     id = "collapse3",
                                     bsCollapsePanel(
                                       title = "Study Name Panel (Click to open / hide this panel)", 
                                       DTOutput("df_nma_ipd_s"),
                                       style = "warning"
                                     )
                                   ))
                            ))
                          )
                        )
                      ),
                      tabPanel(
                        title = "1.22 Show the AD",
                        value = "tab_show_ad",
                        div(
                          wellPanel(              
                            bsCollapse(
                              id = "collapse1",open = "panel1",
                              bsCollapsePanel(
                                title = "Raw AD Panel (Click to open / hide this panel)", 
                                DTOutput("df_nma_ad"),
                                style = "warning",value = "panel1"
                              )
                            ),
                          ),
                          wellPanel(
                            fluidRow(
                              column(width = 6,
                                     wellPanel(
                                       bsCollapse(
                                       id = "collapse2",
                                       bsCollapsePanel(
                                         title = "Treatment Name Panel (Click to open / hide this panel)", 
                                         DTOutput("df_nma_ad_t"),
                                         style = "warning"
                                       )
                                     )
                                    )
                              ),
                              column(width = 6,
                                     wellPanel(bsCollapse(
                                       id = "collapse3",
                                       bsCollapsePanel(
                                         title = "Study Name Panel (Click to open / hide this panel)", 
                                         DTOutput("df_nma_ad_s"),
                                         style = "warning"
                                       )
                                     ))
                              ))
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = "1.3 Data Transform",
                    value = "data_transform_panel",
                    tabsetPanel(
                      id = "data_transform_subpanel",
                      tabPanel(
                        title = "1.31 Control Panel",
                        value = "datapage3",
                        wellPanel(
                          div(
                            p(""),
                            uiOutput("tran_length1"),
                            p(""),
                            uiOutput("tran_step1"),
                            p(""),
                            hr(),
                            p(""),
                            uiOutput("tran_ready")
                          )
                        )
                      ),
                      tabPanel(
                        title = "1.32 Show the Transformed AD",
                        value = "show_data_transform_panel",
                        div(
                          p(""),
                          actionButton("trans_ad_analysis", label="Use Transformed AD to Analysis",icon("gear"),style="color: black;
                          background-color: white; border-color: #2e6da4"),
                          p(""),
                          wellPanel(              
                            bsCollapse(
                              id = "collapse1",open = "panel1",
                              bsCollapsePanel(
                                title = "Transformed AD Panel (Click to open / hide this panel)", 
                                DTOutput("tran_agg"),
                                conditionalPanel(
                                  condition = "output.tran_agg",
                                  downloadButton("downloadtable_tran_agg", "Download Aggregated Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                                ),
                                style = "warning",value = "panel1"
                              )
                            ),
                          )
                        )
                      )
                    )
                  ),
                  #### PH Test Page ####
                  tabPanel(title = "1.4 PH assumption Test",
                           value = "ph_test_panel",
                           wellPanel(
                             div(
                               p(""),
                               uiOutput("selectStudy"),
                               uiOutput("selectTreatment1"),
                               uiOutput("selectTreatment2"),
                               p(""),
                               hr(),
                               selectInput("select_pht", 
                                           label = "Select a method for PH assumptions test",
                                           choices = list("Schoenfeld residual plot" = "pht1", 
                                                          "Log-Log plot" = "pht2", 
                                                          "Grambsch-Therneau test" = "pht3"),
                                           selected = "pht1")%>%
                                 helper(type = "markdown", content = "pht"),
                               p(""),
                               hr(),
                               uiOutput("pht_ready"),
                               p(""),
                               hr(),
                               HTML("<b>Selected PH assumption test results</b>"),
                               conditionalPanel(
                                 condition = "input.select_pht == 'pht1'",
                                 htmlOutput("text_pht_3"),
                                 verbatimTextOutput("text_pht_2"),
                                 p(),
                                 plotOutput("plot_pht_1",width = "800px",height  = "600px"),
                                 p(),
                                 conditionalPanel(
                                   condition = "output.plot_pht_1",
                                   actionButton("ph_1_dw", "Download plot panel", icon("circle-down"),
                                                style="color: black;background-color: white; border-color: #2e6da4"),
                                   conditionalPanel(
                                     condition = "input.ph_1_dw",
                                     uiOutput("pht_1_sizebut"),
                                     downloadButton("downloadPlot_pht_1", "Download", icon = icon("download"))
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.select_pht == 'pht2'",
                                 plotOutput("plot_pht_2",width = "800px",height  = "600px"),
                                 p(),
                                 conditionalPanel(
                                   condition = "output.plot_pht_2",
                                   actionButton("ph_2_dw", "Download plot panel", icon("circle-down"),
                                                style="color: black;background-color: white; border-color: #2e6da4"),
                                   conditionalPanel(
                                     condition = "input.ph_2_dw",
                                     uiOutput("pht_2_sizebut"),
                                     downloadButton("downloadPlot_pht_2", "Download", icon = icon("download"))
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.select_pht == 'pht3'",
                                 htmlOutput("text_pht_4"),
                                 verbatimTextOutput("text_pht_1"),
                               )
                             )
                           )
                  ),
                  #### End of PH Test Page ####

                )
              )
            )
          ),
          #### End of Load Data Panel ####
          
          #### Network Plot Page ####
          tabPanel(title = "2. Network Plot",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Network Plot",class = "custom-h4"),
                       p(""),
                       fileInput("uploadnp", "Load Excel file (.xlsx)", accept = c(".xlsx")),
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
                       uiOutput("np_ready")
                     ),
                     mainPanel( 
                       br(),
                       bsCollapse(
                         id = "collapse_npd",
                         bsCollapsePanel(
                           title = "Raw Data (Click to open / hide this panel)", 
                           DTOutput("df_nma_np"),
                           style = "warning"
                         )
                       ),
                       conditionalPanel(
                         condition = "output.plot_np",
                         tags$h4("Network Plot", class = "custom-h4"),
                       ),
                       plotOutput("plot_np",width = "800px",height  = "600px"),
                       conditionalPanel(
                         condition = "output.plot_np",
                         numericInput("heightnp", "Height (px)", value = 600),
                         numericInput("widthnp", "Width (px)", value = 800),
                         downloadButton("downloadPlot", "Download", icon = icon("download"))
                       )
                     )
                   ) 
          ),
          #### End of Network Plot Page ####

        )
      ),
      #### End of Data Page ####
      
      ##### AD NMA Page #####
      #NMA1
      tabPanel(
        title = "AD-based NMA",
        tabsetPanel(

          #### FP ####
          tabPanel(title = "1. Fractional Polynomials",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Fractional Polynomials (FP)",class = "custom-h4")%>%
                         helper(type = "markdown", content = "table1"),
                       p(""), 
                       textInput("nma1_fp_refs","Reference Study",value = "ALEX"),
                       textInput("nma1_fp_reft","Reference Treatment",value = "crizotinib"),
                       numericInput("nma1_fp_ex","Extrapolation time (Year)",value = 10,min = 0),
                       actionButton("nma1_fp_run", label="Run FP NMA (Frequentist analysis)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                       p(""),
                       hr(),
                       HTML("<h6>Adjusting the Y axis (FP)</h6>"),
                       HTML("<h6>Slider for Hazard plot</h6>"),
                       checkboxInput("y_axis_haz_fp", HTML("<h6>Activate axis control (Hazard)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_haz_fp==true",
                         sliderInput("y_min_haz_fp", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_haz_fp", "Y-axis Max", min = 1, max = 100, value = 10, step = 1)),
                       HTML("<h6>Slider for HR plot</h6>"),
                       checkboxInput("y_axis_hr_fp", HTML("<h6>Activate axis control (HR)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_hr_fp==true",
                         sliderInput("y_min_hr_fp", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_hr_fp", "Y-axis Max", min = 1, max = 100, value = 10, step = 1))
                     ),
                     mainPanel(
                       tabsetPanel(
                         id = "subtabs_fp_main",
                         tabPanel(
                           title = "1.1 Instruction",
                           div(
                             wellPanel(
                               h4("Fractional Polynomials (FP)",class = "custom-h4"),
                             hr(),
                             div(
                                 HTML("<b>Read first (Some notes for FP NMA)</b>"),
                                 HTML("In this FP NMA section, we use the framework of Wiksten et al.(2020). We propose a two-step process for fitting FP models.
                                               In the first step, an ANOVA-like parameterisation is used to express and fit the models as GLM
                                               with time-varying covariates in a frequentist framework (<i>Run FP NMA (Frequentist analysis)</i> in sidebar).
                                               The fit of the models in terms of the AIC is compared (in <b>FP Step One</b> panel).
                                               The model with the lowest AIC can be selected to fit in the Bayesian setting (Frequentist setting) in the second step
                                               (in <b>FP Step TWO (FP1)</b> panel and <b>FP Step TWO (FP2)</b> panel).
                                               "),br(),br(),
                                 HTML("For the fixed-effect model, we provide the parameter estimates. Hazard plot and survival plot are also drawn based on these parameters.
                                 Use the Difference in Beta and the Beta of reference treatment to calculate other Beta values.
                                               Then, hazard over time for each of the interventions can be calculated through the function with Beta.
                                               In addition, through d (trtf in Frequentist setting), Hazard Ratio between selected treatments can be calculated.
                                               This process can also be realized easily through EXCEL.<br><br>"),
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
                                 p("For the random-effects model, we provide the parameter estimates only. Plots can be drawn through the same methods.
                                                 In addition, we only consider the model with a heterogeneity parameter for d0 in the random-effects model.
                                                 We do not consider all heterogeneity parameters (d0, d1, d2 in FP2) since we believe that the methodology still need further development.
                                                 (Bayesian analysis using the random-effects model would require careful specification of the prior for between-study heterogeneity.
                                                 Also, maximum likelihood estimation can be problematic at this time, in which the number of parameters increases with the number of studies.)
                                                 An example of Bugs codes for the random-effects FP models with all heterogeneity parameters can be found in Jansen JP (2011).
                                                 In Jansen's study, two kinds of the random-effects models showed similar fit results.
                                                 ")),
                             p("For more information about FP, please refer to the following two articles:"),
                             p(""),
                             tags$a(href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-11-61", "[1]Network meta-analysis of survival data with fractional polynomials; Jansen JP (2011)", target = "_blank"),br(),
                             tags$a(href = "https://doi.org/10.1016/j.jval.2020.03.010", "[2]Nonproportional Hazards in Network Meta-Analysis: Efficient Strategies for Model Building and Analysis; Anna Wiksten et.al. (2020)", target = "_blank"),br(),
                             p(""),
                             HTML("For more information about the methodologies and codes of running FP in this APP, please refer to the <b>User Manual</b> panel"),
                             p("")
                           ))
                         ),
                         tabPanel(
                           title = "1.2 FP Step One",
                           div(
                             wellPanel(
                               conditionalPanel(
                                 condition = "input.nma1_fp_run",
                                 HTML("<b>AIC results for FP (Frequentist analysis:Fixed-effect model)</b>"),
                                 DTOutput("nma1_fp_fix_aic"),
                                 p(""),
                                 hr(),
                                 uiOutput("gotofpstep21"),
                                 p(""),
                                 uiOutput("gotofpstep22")
                               )
                             )
                           )
                         ),
                         ### FP1
                         tabPanel(
                           title = "1.3 FP Step Two (FP1)",
                           tabsetPanel(
                             tabPanel(
                               title = "1.31 Frequentist analysis",
                               value = "fppage21",
                               tabsetPanel(
                                 id = "fp1_freq_sub",
                                 tabPanel(
                                   title = "1.31(a) Input Panel",
                                   value = "fppage31",
                                   div(
                                     wellPanel(
                                       h4(strong("Fractional polynomials 1, Frequentist analysis")),
                                       br(),
                                       p(),
                                       numericInput("fp1_freq_pow","Power of FP1",value = 1),
                                       actionButton("nma1_fp1_freq_run1", label="Run FP1 NMA (Fixed-effect model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       br(),
                                       p(""),
                                       actionButton("nma1_fp1_freq_run1r", label="Run FP1 NMA (Random-effects model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4")
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.31(b) Fixed-effect model",
                                   value = "fppage32",
                                   div(
                                     p(),
                                     uiOutput("output_fp1ff"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp1ff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1ff_coeff",
                                         HTML("<b>Coefficient Table for FP1 (Frequentist analysis:Fixed-effect model)</b>")
                                       ),
                                       DTOutput("nma1_fp1ff_coeff"),
                                       p(""),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1ff_coeff",
                                         downloadButton("copy_nma1_fp1ff_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_haz",
                                                  HTML("<b>Hazard plot for FP1 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1ff_plot_haz"),
                                                p(""),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_haz",
                                                  actionButton("nma1_fp1ff_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1ff_plot_haz_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1ff_plot_haz_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1ff_plot_haz_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1ff_plot_haz", "Download", icon = icon("download"))
                                                  )
                                                ),
                                         ),
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_surv",
                                                  HTML("<b>Survival plot for FP1 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1ff_plot_surv"),
                                                p(""),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_surv",
                                                  actionButton("nma1_fp1ff_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1ff_plot_surv_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1ff_plot_surv_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1ff_plot_surv_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1ff_plot_surv", "Download", icon = icon("download"))
                                                  )
                                                ),
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_hr",
                                                  HTML("<b>Hazard Ratio plot vs reference treatment for FP1 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1ff_plot_hr"),
                                                p(""),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1ff_plot_hr",
                                                  downloadButton("nma1_fp1ff_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  p(),
                                                  actionButton("nma1_fp1ff_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1ff_plot_hr_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1ff_plot_hr_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1ff_plot_hr_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1ff_plot_hr", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.31(c) Random-effects model",                                   
                                   value = "fppage33",
                                   div(
                                     p(),
                                     uiOutput("output_fp1fr"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp1fr"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1fr_coeff",
                                         HTML("<b>Coefficient Table for FP1 (Frequentist analysis:Random-effects model)</b>")
                                       ),
                                       DTOutput("nma1_fp1fr_coeff"),
                                       p(""),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1fr_coeff",
                                         downloadButton("copy_nma1_fpfr_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       )
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "1.32 Bayesian analysis",
                               value = "fppage22",
                               tabsetPanel(
                                 id = "fp1_bay_sub",
                                 tabPanel(
                                   title = "1.32(a) Input Panel",
                                   value = "fppage34",
                                   div(
                                     wellPanel(
                                       h4(strong("Fractional polynomials 1, Bayesian analysis")),
                                       br(),
                                       p(),
                                       numericInput("fp1_bay_pow","Power of FP1",value = 1),
                                       actionButton("nma1_fp1_bay_run1", label="Run FP1 NMA (Fixed-effect model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       br(),
                                       p(""),
                                       actionButton("nma1_fp1_bay_run1r", label="Run FP1 NMA (Random-effects model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       p(""),
                                       hr(),
                                       HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                       checkboxInput("nma1_fp1_jags_setting","Show advanced settings",value = FALSE),
                                       conditionalPanel(
                                         condition = "input.nma1_fp1_jags_setting == true",
                                         tags$hr(style="border-top: 1px solid black;"),
                                         HTML("<b>Advanced settings for MCMC</b>"),
                                         numericInput("fp1_nchains","number of Markov chains",value = 3),
                                         numericInput("fp1_niter","number of total iterations per chain (including burn in",value = 20000),
                                         numericInput("fp1_nburnin","length of burn in",value = 10000),
                                         numericInput("fp1_nthin","thinning rate",value = 3),
                                         HTML("<b>Advanced settings for priors</b>"),
                                         numericInput("fp1_pmean","priors of mu[]",value = 0),
                                         numericInput("fp1_pprec","priors of T[]",value = 0.0001)
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.32(b) Fixed-effect model",
                                   value = "fppage35",
                                   div(
                                     p(),
                                     uiOutput("output_fp1bf"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp1bf"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1bf_coeff",
                                         HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                         textOutput("nma1_fp1bf_DIC"),
                                         p(""),
                                         HTML("<b>Coefficient Table for FP1 (Bayesian analysis:Fixed-effect model)</b>")
                                       ),
                                       DTOutput("nma1_fp1bf_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1bf_coeff",
                                         downloadButton("copy_nma1_fp1bf_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       DTOutput("nma1_fp1bf_coeff_mu"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1bf_coeff_mu",
                                         downloadButton("copy_nma1_fp1bf_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_haz",
                                                  HTML("<b>Hazard plot for FP1 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1bf_plot_haz"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_haz",
                                                  actionButton("nma1_fp1bf_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1bf_plot_haz_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1bf_plot_haz_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1bf_plot_haz_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1bf_plot_haz", "Download", icon = icon("download"))
                                                  )
                                                )
                                                
                                         ),
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_surv",
                                                  HTML("<b>Survival plot for FP1 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1bf_plot_surv"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_surv",
                                                  actionButton("nma1_fp1bf_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1bf_plot_surv_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1bf_plot_surv_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1bf_plot_surv_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1bf_plot_surv", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_hr",
                                                  HTML("<b>Hazard Ratio plot vs reference treatment for FP1 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp1bf_plot_hr"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp1bf_plot_hr",
                                                  downloadButton("nma1_fp1bf_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  p(),
                                                  actionButton("nma1_fp1bf_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp1bf_plot_hr_dw",
                                                    p(),
                                                    uiOutput("nma1_fp1bf_plot_hr_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp1bf_plot_hr_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp1bf_plot_hr", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.32(c) Random-effects model",
                                   value = "fppage36",
                                   div(
                                     p(),
                                     uiOutput("output_fp1br"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp1br"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1br_coeff",
                                         HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                         textOutput("nma1_fp1br_DIC"),
                                         p(""),
                                         HTML("<b>Coefficient Table for FP1 (Bayesian analysis:Random-effects model)</b>")
                                       ),
                                       DTOutput("nma1_fp1br_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1br_coeff",
                                         downloadButton("copy_nma1_fp1br_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       DTOutput("nma1_fp1br_coeff_mu"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp1br_coeff_mu",
                                         downloadButton("copy_nma1_fp1br_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           )                           
                         ),
                         ### FP2
                         tabPanel(
                           title = "1.4 FP Step Two (FP2)",
                           tabsetPanel(
                             tabPanel(
                               title = "1.41 Frequentist analysis",
                               value = "fppage23",
                               tabsetPanel(
                                 id = "fp2_freq_sub",
                                 tabPanel(
                                   title = "1.41(a) Input Panel",
                                   value = "fppage37",
                                   div(
                                     wellPanel(
                                       h4(strong("Fractional polynomials 2, Frequentist analysis")),
                                       br(),
                                       p(),
                                       numericInput("fp2_freq_pow1","Power of FP2_1",value = -0.5),
                                       numericInput("fp2_freq_pow2","Power of FP2_2",value = 1),
                                       actionButton("nma1_fp2_freq_run2", label="Run FP2 NMA (Fixed-effect model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       br(),
                                       p(""),
                                       actionButton("nma1_fp2_freq_run2r", label="Run FP2 NMA (Random-effects model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       p(""),
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.41(b) Fixed-effect model",
                                   value = "fppage38",
                                   div(
                                     p(),
                                     uiOutput("output_fp2ff"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp2ff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2ff_coeff",
                                         HTML("<b>Coefficient Table for FP2 (Frequentist analysis:Fixed-effect model)</b>")
                                       ),
                                       DTOutput("nma1_fp2ff_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2ff_coeff",
                                         downloadButton("copy_nma1_fp2ff_coeff", label="Copy this table",style="color: black;
                     background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_haz",
                                                  HTML("<b>Hazard plot for FP2 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2ff_plot_haz"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_haz",
                                                  actionButton("nma1_fp2ff_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2ff_plot_haz_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2ff_plot_haz_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2ff_plot_haz_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2ff_plot_haz", "Download", icon = icon("download"))
                                                  )
                                                )
                                                
                                         ),
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_surv",
                                                  HTML("<b>Survival plot for FP2 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2ff_plot_surv"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_surv",
                                                  actionButton("nma1_fp2ff_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2ff_plot_surv_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2ff_plot_surv_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2ff_plot_surv_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2ff_plot_surv", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_hr",
                                                  HTML("<b>Hazard Ratio plot vs reference treatment for fp2 (Frequentist analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2ff_plot_hr"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2ff_plot_hr",
                                                  downloadButton("nma1_fp2ff_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  p(),
                                                  actionButton("nma1_fp2ff_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2ff_plot_hr_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2ff_plot_hr_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2ff_plot_hr_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2ff_plot_hr", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.41(c) Random-effects model",
                                   value = "fppage39",
                                   div(
                                     p(),
                                     uiOutput("output_fp2fr"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp2fr"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2fr_coeff",
                                         HTML("<b>Coefficient Table for FP2 (Frequentist analysis:Random-effects model)</b>")
                                       ),
                                       DTOutput("nma1_fp2fr_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2fr_coeff",
                                         downloadButton("copy_nma1_fp2fr_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       )
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "1.42 Bayesian analysis",
                               value = "fppage24",
                               tabsetPanel(
                                 id = "fp2_bay_sub",
                                 tabPanel(
                                   title = "1.42(a) Input Panel",
                                   value = "fppage310",
                                   div(
                                     wellPanel(
                                       h4(strong("Fractional polynomials 2, Bayesian analysis")),
                                       br(),
                                       p(),
                                       numericInput("fp2_bay_pow1","Power of FP2_1",value = -0.5),
                                       numericInput("fp2_bay_pow2","Power of FP2_2",value = 1),
                                       actionButton("nma1_fp2_bay_run2", label="Run FP2 NMA (Fixed-effect model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       br(),
                                       p(""),
                                       actionButton("nma1_fp2_bay_run2r", label="Run FP2 NMA (Random-effects model)",icon("gear"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                       p(),
                                       hr(),
                                       HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                       checkboxInput("nma1_fp2_jags_setting","Show advanced settings",value = FALSE),
                                       conditionalPanel(
                                         condition = "input.nma1_fp2_jags_setting == true",
                                         tags$hr(style="border-top: 1px solid black;"),
                                         HTML("<b>Advanced settings for MCMC</b>"),
                                         numericInput("fp2_nchains","number of Markov chains",value = 3),
                                         numericInput("fp2_niter","number of total iterations per chain (including burn in",value = 20000),
                                         numericInput("fp2_nburnin","length of burn in",value = 10000),
                                         numericInput("fp2_nthin","thinning rate",value = 3),
                                         HTML("<b>Advanced settings for priors</b>"),
                                         numericInput("fp2_pmean","priors of mu[]",value = 0),
                                         numericInput("fp2_pprec","priors of T[]",value = 0.0001)
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.42(b) Fixed-effect model",
                                   value = "fppage311",
                                   div(
                                     p(),
                                     uiOutput("output_fp2bf"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp2bf"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2bf_coeff",
                                         HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                         textOutput("nma1_fp2bf_DIC"),
                                         p(""),
                                         HTML("<b>Coefficient Table for FP2 (Bayesian analysis:Fixed-effect model)</b>")
                                       ),
                                       DTOutput("nma1_fp2bf_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2bf_coeff",
                                         downloadButton("copy_nma1_fp2bf_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       DTOutput("nma1_fp2bf_coeff_mu"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2bf_coeff_mu",
                                         downloadButton("copy_nma1_fp2bf_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_haz",
                                                  HTML("<b>Hazard plot for FP2 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2bf_plot_haz"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_haz",
                                                  actionButton("nma1_fp2bf_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2bf_plot_haz_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2bf_plot_haz_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2bf_plot_haz_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2bf_plot_haz", "Download", icon = icon("download"))
                                                  )
                                                )
                                                
                                         ),
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_surv",
                                                  HTML("<b>Survival plot for FP2 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2bf_plot_surv"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_surv",
                                                  actionButton("nma1_fp2bf_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2bf_plot_surv_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2bf_plot_surv_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2bf_plot_surv_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2bf_plot_surv", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 6,
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_hr",
                                                  HTML("<b>Hazard Ratio plot vs reference treatment for FP2 (Bayesian analysis:Fixed-effect model)</b>")),
                                                plotOutput("nma1_fp2bf_plot_hr"),
                                                p(),
                                                conditionalPanel(
                                                  condition = "output.nma1_fp2bf_plot_hr",
                                                  downloadButton("nma1_fp2bf_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  p(),
                                                  actionButton("nma1_fp2bf_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                               style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                                  conditionalPanel(
                                                    condition = "input.nma1_fp2bf_plot_hr_dw",
                                                    p(),
                                                    uiOutput("nma1_fp2bf_plot_hr_titlebut"),
                                                    fluidRow(
                                                      column(width = 5,
                                                             uiOutput("nma1_fp2bf_plot_hr_sizebut")
                                                      )
                                                    ),
                                                    downloadButton("downloadPlot_nma1_fp2bf_plot_hr", "Download", icon = icon("download"))
                                                  )
                                                )
                                         )
                                       )
                                     )
                                   )
                                 ),
                                 tabPanel(
                                   title = "1.42(c) Random-effects model",
                                   value = "fppage312",
                                   div(
                                     p(),
                                     uiOutput("output_fp2br"),
                                     p(),
                                     wellPanel(
                                       uiOutput("text_fp2br"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2br_coeff",
                                         HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                         textOutput("nma1_fp2br_DIC"),
                                         p(""),
                                         HTML("<b>Coefficient Table for FP2 (Bayesian analysis:Random-effects model)</b>")
                                       ),
                                       DTOutput("nma1_fp2br_coeff"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2br_coeff",
                                         downloadButton("copy_nma1_fp2br_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       ),
                                       br(),
                                       DTOutput("nma1_fp2br_coeff_mu"),
                                       p(),
                                       conditionalPanel(
                                         condition = "output.nma1_fp2br_coeff_mu",
                                         downloadButton("copy_nma1_fp2br_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           )                           
                         )
                       )
                     )
                   )
          ),
          #### End of FP ####
          
          #### PWE ####
          tabPanel(title = "2. Piecewise Exponential",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Piecewise Exponential (PWE)",class = "custom-h4")%>%
                         helper(type = "markdown", content = "table1"),
                       p(""), 
                       textInput("nma1_pwe_refs","Reference Study",value = "ALEX"),
                       textInput("nma1_pwe_reft","Reference Treatment",value = "crizotinib"),
                       numericInput("nma1_pwe_ex","Extrapolation time (Year)",value = 10,min = 0),
                       p(""),
                       tags$hr(style="border-top: 1px solid black;"),
                       p(""),
                       hr(),
                       HTML("<h6>Adjusting the Y axis (PWE)</h6>"),
                       HTML("<h6>Slider for Hazard plot</h6>"),
                       checkboxInput("y_axis_haz_pwe", HTML("<h6>Activate axis control (Hazard)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_haz_pwe==true",
                         sliderInput("y_min_haz_pwe", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_haz_pwe", "Y-axis Max", min = 1, max = 100, value = 10, step = 1)),
                       HTML("<h6>Slider for HR plot</h6>"),
                       checkboxInput("y_axis_hr_pwe", HTML("<h6>Activate axis control (HR)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_hr_pwe==true",
                         sliderInput("y_min_hr_pwe", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_hr_pwe", "Y-axis Max", min = 1, max = 100, value = 10, step = 1))
                     ),
                     mainPanel(
                       tabsetPanel(
                         id = "subtabs_pwe_main",
                         tabPanel(
                           title = "2.1 Instruction",
                           div(
                             wellPanel(
                               h4("Piecewise Exponential (PWE)",class = "custom-h4"),
                               hr(),
                               div(
                                   HTML("<b>Read first (Some notes for PWE NMA)</b>"),
                                   p("The framework of PWE model is similar to that of FP.
                  A PWE function can also be written in the form of ANOVA-like parameterisation, so that it can be fitted in the GLM framework.
                  Currently, only the fixed-effect models are available for PWE NMA. The choice of where to place cut points and how many cut points could result in
                  many models being fitted before the best model can be selected.
                  We recommend users to use Frequentist analysis to select suitable models before moving to Bayesian analysis."),
                                   HTML("Use the Difference in Beta and the Beta of reference treatment to calculate other Beta values.
                  Then, hazard over time for each of the interventions can be calculated through the function with Beta.
                  In addition, through d (trtf in Frequentist setting), Hazard Ratio between selected treatments can be calculated.
                  This process can also be realized easily through EXCEL.<br><br>"),
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
                               HTML("For more information about the methodologies and codes of running PWE in this APP, please refer to the <b>User Manual</b> panel"),
                               p("")
                             )
                           )
                         ),
                         ### PWE1
                         tabPanel(
                           title = "2.2 PWE 1",
                           tabsetPanel(
                             id = "subtabs_pwe1",
                             tabPanel(
                               title = "2.21 Input Panel",
                               value = "pwepage21",
                               div(
                                 wellPanel(
                                   h4(strong("Piecewise Exponential with One Cutpoint")),
                                   br(),
                                   p(),
                                   numericInput("pwe1_pow","Cutpoint of PWE",value = 12),
                                   actionButton("nma1_pwe1f_run1", label="Run PWE 1 NMA (Frequentist analysis)",icon("gear"),style="color: black;
                               background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_pwe1b_run1", label="Run PWE 1 NMA (Bayesian analysis)",icon("gear"),style="color: black;
                               background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_pwe1_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_pwe1_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("pwe1_nchains","number of Markov chains",value = 3),
                                     numericInput("pwe1_niter","number of total iterations per chain (including burn in)",value = 20000),
                                     numericInput("pwe1_nburnin","length of burn in",value = 10000),
                                     numericInput("pwe1_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("pwe1_pmean","priors of mu[]",value = 0),
                                     numericInput("pwe1_pprec","priors of T[]",value = 0.0001)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "2.22 Frequentist analysis:Fixed-effect model",
                               value = "pwepage22",
                               div(
                                 p(),
                                 uiOutput("output_pwe1f"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1f_coeff",
                                     HTML("<b>Coefficient Table for PWE1 (Frequentist analysis:Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_pwe1f_coeff"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1f_coeff",
                                     downloadButton("copy_nma1_pwe1f_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_haz",
                                            HTML("<b>Hazard plot for PWE1 (Frequentist analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe1f_plot_haz"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_haz",
                                            actionButton("nma1_pwe1f_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe1f_plot_haz_dw",
                                              p(),
                                              uiOutput("nma1_pwe1f_plot_haz_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe1f_plot_haz_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe1f_plot_haz", "Download", icon = icon("download"))
                                            )
                                          ),
                                   ),
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_surv",
                                            HTML("<b>Survival plot for PWE1 (Frequentist analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe1f_plot_surv"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_surv",
                                            actionButton("nma1_pwe1f_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe1f_plot_surv_dw",
                                              p(),
                                              uiOutput("nma1_pwe1f_plot_surv_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe1f_plot_surv_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe1f_plot_surv", "Download", icon = icon("download"))
                                            )
                                          ),
                                   )
                                 ),
                                 br(),
                                 fluidRow(
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_hr",
                                            HTML("<b>Hazard Ratio plot vs reference treatment for PWE1 (Frequentist analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe1f_plot_hr"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe1f_plot_hr",
                                            downloadButton("nma1_pwe1f_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            p(),
                                            actionButton("nma1_pwe1f_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe1f_plot_hr_dw",
                                              p(),
                                              uiOutput("nma1_pwe1f_plot_hr_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe1f_plot_hr_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe1f_plot_hr", "Download", icon = icon("download"))
                                            )
                                          )
                                   )
                                 )
                               )
                             )
                          ),
                              tabPanel(
                               title = "2.23 Bayesian analysis:Fixed-effect model",
                               value = "pwepage23",
                               div(
                                 p(),
                                 uiOutput("output_pwe1b"),
                                 p(),
                                 wellPanel( 
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1b_coeff",
                                     HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                     textOutput("nma1_pwe1b_DIC")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1b_coeff",
                                     HTML("<b>Coefficient Table for PWE1 (Bayesian analysis:Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_pwe1b_coeff"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1b_coeff",
                                     downloadButton("copy_nma1_pwe1b_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_pwe1b_coeff_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_pwe1b_coeff_mu",
                                     downloadButton("copy_nma1_pwe1b_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_haz",
                                              HTML("<b>Hazard plot for PWE1 (Bayesian analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe1b_plot_haz"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_haz",
                                              actionButton("nma1_pwe1b_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe1b_plot_haz_dw",
                                                p(),
                                                uiOutput("nma1_pwe1b_plot_haz_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe1b_plot_haz_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1f_pwe1b_plot_haz", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_surv",
                                              HTML("<b>Survival plot for PWE1 (Bayesian analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe1b_plot_surv"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_surv",
                                              actionButton("nma1_pwe1b_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe1b_plot_surv_dw",
                                                p(),
                                                uiOutput("nma1_pwe1b_plot_surv_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe1b_plot_surv_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1_pwe1b_plot_surv", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_hr",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for PWE1 (Bayesian analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe1b_plot_hr"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe1b_plot_hr",
                                              downloadButton("nma1_pwe1b_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_pwe1b_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe1b_plot_hr_dw",
                                                p(),
                                                uiOutput("nma1_pwe1b_plot_hr_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe1b_plot_hr_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1_pwe1b_plot_hr", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         ),
                       ### PWE2
                         tabPanel(
                           title = "2.3 PWE 2",
                           tabsetPanel(
                             id = "subtabs_pwe2",
                             tabPanel(
                               title = "2.31 Input Panel",
                               value = "pwepage24",
                               div(
                                 wellPanel(
                                   h4(strong("Piecewise Exponential with Two Cutpoints")),
                                   br(),
                                   p(),
                                   numericInput("pwe2_pow1","Cutpoint1 of PWE",value = 6),
                                   numericInput("pwe2_pow2","Cutpoint2 of PWE",value = 12),
                                   actionButton("nma1_pwe2f_run2", label="Run PWE 2 NMA (Frequentist analysis)",icon("gear"),style="color: black;
                               background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_pwe2b_run2", label="Run PWE 2 NMA (Bayesian analysis)",icon("gear"),style="color: black;
                               background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_pwe2_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_pwe2_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("pwe2_nchains","number of Markov chains",value = 3),
                                     numericInput("pwe2_niter","number of total iterations per chain (including burn in)",value = 20000),
                                     numericInput("pwe2_nburnin","length of burn in",value = 10000),
                                     numericInput("pwe2_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("pwe2_pmean","priors of mu[]",value = 0),
                                     numericInput("pwe2_pprec","priors of T[]",value = 0.0001)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "2.32 Frequentist analysis:Fixed-effect model",
                               value = "pwepage25",
                               div(
                                 p(),
                                 uiOutput("output_pwe2f"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_pwe2f_coeff",
                                     HTML("<b>Coefficient Table for PWE2 (Frequentist analysis:Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_pwe2f_coeff"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_pwe2f_coeff",
                                     downloadButton("copy_nma1_pwe2f_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_haz",
                                              HTML("<b>Hazard plot for PWE2 (Frequentist analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe2f_plot_haz"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_haz",
                                              actionButton("nma1_pwe2f_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe2f_plot_haz_dw",
                                                p(),
                                                uiOutput("nma1_pwe2f_plot_haz_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe2f_plot_haz_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1_pwe2f_plot_haz", "Download", icon = icon("download"))
                                              )
                                            )
                                            
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_surv",
                                              HTML("<b>Survival plot for PWE2 (Frequentist analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe2f_plot_surv"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_surv",
                                              actionButton("nma1_pwe2f_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe2f_plot_surv_dw",
                                                p(),
                                                uiOutput("nma1_pwe2f_plot_surv_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe2f_plot_surv_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1_pwe2f_plot_surv", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_hr",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for PWE2 (Frequentist analysis:Fixed-effect model)</b>")),
                                            plotOutput("nma1_pwe2f_plot_hr"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_pwe2f_plot_hr",
                                              downloadButton("nma1_pwe2f_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_pwe2f_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                           style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              conditionalPanel(
                                                condition = "input.nma1_pwe2f_plot_hr_dw",
                                                p(),
                                                uiOutput("nma1_pwe2f_plot_hr_titlebut"),
                                                fluidRow(
                                                  column(width = 5,
                                                         uiOutput("nma1_pwe2f_plot_hr_sizebut")
                                                  )
                                                ),
                                                downloadButton("downloadPlot_nma1_pwe2f_plot_hr", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   )
                                 )
                               )
                           ),
                           tabPanel(
                             title = "2.33 Bayesian analysis:Fixed-effect model",
                             value = "pwepage26",
                             div(
                               p(),
                               uiOutput("output_pwe2b"),
                               p(),
                               wellPanel(
                                 conditionalPanel(
                                   condition = "output.nma1_pwe2b_coeff",
                                   HTML("<b>Goodness-of-fit estimates (Bayesian analysis)</b>"),
                                   textOutput("nma1_pwe2b_DIC")),
                                 p(),
                                 conditionalPanel(
                                   condition = "output.nma1_pwe2b_coeff",
                                   HTML("<b>Coefficient Table for PWE2 (Bayesian analysis:Fixed-effect model)</b>")
                                 ),
                                 DTOutput("nma1_pwe2b_coeff"),
                                 p(),
                                 conditionalPanel(
                                   condition = "output.nma1_pwe2b_coeff",
                                   downloadButton("copy_nma1_pwe2b_coeff", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                 ),
                                 br(),
                                 DTOutput("nma1_pwe2b_coeff_mu"),
                                 p(),
                                 conditionalPanel(
                                   condition = "output.nma1_pwe2b_coeff_mu",
                                   downloadButton("copy_nma1_pwe2b_coeff_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                 ),
                                 br(),
                                 fluidRow(
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_haz",
                                            HTML("<b>Hazard plot for PWE2 (Bayesian analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe2b_plot_haz"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_haz",
                                            actionButton("nma1_pwe2b_plot_haz_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe2b_plot_haz_dw",
                                              p(),
                                              uiOutput("nma1_pwe2b_plot_haz_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe2b_plot_haz_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe2b_plot_haz", "Download", icon = icon("download"))
                                            )
                                          )
                                          
                                   ),
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_surv",
                                            HTML("<b>Survival plot for PWE2 (Bayesian analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe2b_plot_surv"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_surv",
                                            actionButton("nma1_pwe2b_plot_surv_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe2b_plot_surv_dw",
                                              p(),
                                              uiOutput("nma1_pwe2b_plot_surv_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe2b_plot_surv_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe2b_plot_surv", "Download", icon = icon("download"))
                                            )
                                          )
                                   )
                                 ),
                                 br(),
                                 fluidRow(
                                   column(width = 6,
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_hr",
                                            HTML("<b>Hazard Ratio plot vs reference treatment for PWE2 (Bayesian analysis:Fixed-effect model)</b>")),
                                          plotOutput("nma1_pwe2b_plot_hr"),
                                          p(),
                                          conditionalPanel(
                                            condition = "output.nma1_pwe2b_plot_hr",
                                            downloadButton("nma1_pwe2b_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            p(),
                                            actionButton("nma1_pwe2b_plot_hr_dw", "Download plot panel", icon("circle-down"),
                                                         style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                            conditionalPanel(
                                              condition = "input.nma1_pwe2b_plot_hr_dw",
                                              p(),
                                              uiOutput("nma1_pwe2b_plot_hr_titlebut"),
                                              fluidRow(
                                                column(width = 5,
                                                       uiOutput("nma1_pwe2b_plot_hr_sizebut")
                                                )
                                              ),
                                              downloadButton("downloadPlot_nma1_pwe2b_plot_hr", "Download", icon = icon("download"))
                                            )
                                          )
                                   )
                                 )
                               )
                             )
                           )
                         )   
                       )
                     )
                   )
                )
          ),
          #### End of PWE ####
          
          #### PSM ####
          tabPanel(title = "3. Parametric Survival Model",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       h4("Parametric Survival Model (PSM)",class = "custom-h4")%>%
                         helper(type = "markdown", content = "table1"),
                       p(""), 
                       textInput("nma1_psm_refs","Reference Study",value = "ALEX"),
                       textInput("nma1_psm_reft","Reference Treatment",value = "crizotinib"),
                       numericInput("nma1_psm_ex","Extrapolation time (Year)",value = 10,min = 0),
                       p(""),
                       tags$hr(style="border-top: 1px solid black;"),
                       p(""),
                       hr(),
                       HTML("<h6>Adjusting the Y axis (PSM)</h6>"),
                       HTML("<h6>Slider for Hazard plot</h6>"),
                       checkboxInput("y_axis_haz_psm", HTML("<h6>Activate axis control (Hazard)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_haz_psm==true",
                         sliderInput("y_min_haz_psm", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_haz_psm", "Y-axis Max", min = 1, max = 100, value = 10, step = 1)),
                       HTML("<h6>Slider for HR plot</h6>"),
                       checkboxInput("y_axis_hr_psm", HTML("<h6>Activate axis control (HR)</h6>"), value = FALSE),
                       conditionalPanel(
                         condition="input.y_axis_hr_psm==true",
                         sliderInput("y_min_hr_psm", "Y-axis Min", min = 0, max = 1, value = 0.1, step = 0.01),
                         sliderInput("y_max_hr_psm", "Y-axis Max", min = 1, max = 100, value = 10, step = 1))
                     ),
                     mainPanel(
                       tabsetPanel(
                         id = "subtabs_psm_main",
                         tabPanel(
                           title = "3.1 Instruction",
                           div(
                             wellPanel(
                               h4("Parametric Survival Model (PSM)",class = "custom-h4"),
                               hr(),
                               div(
                                   HTML("<b>Read first (Some notes for PSM NMA)</b>"),
                                   p("In the part of Parametric survival model, we only include Bayesian analysis (Fixed-effect model and random-effects model).
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
                               p(),
                               HTML("For more information about the methodologies and codes of running PSM in this APP, please refer to the <b>User Manual</b> panel"),
                               p("")
                             )
                           )
                         ),
                         ### weibull
                         tabPanel(
                           title = "3.2 Weibull",
                           tabsetPanel(
                             id = "subtabs_wei",
                             tabPanel(
                               title = "3.21 Input Panel",
                               value = "psmpage21",
                               div(
                                 wellPanel(
                                   h4(strong("Weibull distribution")),
                                   br(),
                                   p(),
                                   actionButton("nma1_psm1_run", label="Run Weibull (Fixed-effect model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_psm1_runr", label="Run Weibull (Random-effects model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_psm1_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_psm1_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("psm1_nchains","number of Markov chains",value = 3),
                                     numericInput("psm1_niter","number of total iterations per chain (including burn in",value = 20000),
                                     numericInput("psm1_nburnin","length of burn in",value = 10000),
                                     numericInput("psm1_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("psm1_pmean","priors of mu[]",value = 0),
                                     numericInput("psm1_pprec","priors of T[]",value = 0.0001),
                                     numericInput("psm1_pr","priors of R[]",value = 0.01)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.22 Fixed-effect model",
                               value = "psmpage22",
                               div(
                                 p(),
                                 uiOutput("output_psm11"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_1",
                                     HTML("<b>Goodness-of-fit estimates (Weibull (Fixed-effect model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_1")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_1",
                                     HTML("<b>Coefficient Table for Weibull (Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_1"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_1",
                                     downloadButton("copy_nma1_psm1_fix_coeff_1", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_1_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_1_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_1_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_1",
                                              HTML("<b>Hazard plot for Weibull (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_1"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_1",
                                              actionButton("nma1_psm1_plot_haz_1_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_1", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_1",
                                              HTML("<b>Survival plot for Weibull (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_1"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_1",
                                              actionButton("nma1_psm1_plot_surv_1_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_1", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_1",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for Weibull (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_1"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_1",
                                              downloadButton("nma1_psm11_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_1_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_1", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.23 Random-effects model",
                               value = "psmpage23",
                               div(
                                 p(),
                                 uiOutput("output_psm12"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_2",
                                     HTML("<b>Goodness-of-fit estimates (Weibull (Random-effects model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_2")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_2",
                                     HTML("<b>Coefficient Table for Weibull (Random-effects model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_2"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_2",
                                     downloadButton("copy_nma1_psm1_fix_coeff_2", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_2_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_2_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_2_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_2",
                                              HTML("<b>Hazard plot for Weibull (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_2"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_2",
                                              actionButton("nma1_psm1_plot_haz_2_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_2", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_2",
                                              HTML("<b>Survival plot for Weibull (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_2"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_2",
                                              actionButton("nma1_psm1_plot_surv_2_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_2", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_2",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for Weibull (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_2"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_2",
                                              downloadButton("nma1_psm12_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_2_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_2", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         ),
                         ### Gompertz
                         tabPanel(
                           title = "3.3 Gompertz",
                           tabsetPanel(
                             id = "subtabs_gom",
                             tabPanel(
                               title = "3.31 Input Panel",
                               value = "psmpage24",
                               div(
                                 wellPanel(
                                   h4(strong("Gompertz distribution")),
                                   br(),
                                   p(),
                                   actionButton("nma1_psm2_run", label="Run Gompertz (Fixed-effect model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_psm2_runr", label="Run Gompertz (Random-effects model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_psm2_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_psm2_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("psm2_nchains","number of Markov chains",value = 3),
                                     numericInput("psm2_niter","number of total iterations per chain (including burn in",value = 20000),
                                     numericInput("psm2_nburnin","length of burn in",value = 10000),
                                     numericInput("psm2_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("psm2_pmean","priors of mu[]",value = 0),
                                     numericInput("psm2_pprec","priors of T[]",value = 0.0001),
                                     numericInput("psm2_pr","priors of R[]",value = 0.01)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.32 Fixed-effect model",
                               value = "psmpage25",
                               div(
                                 p(),
                                 uiOutput("output_psm13"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_3",
                                     HTML("<b>Goodness-of-fit estimates (Gompertz (Fixed-effect model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_3")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_3",
                                     HTML("<b>Coefficient Table for Gompertz (Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_3"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_3",
                                     downloadButton("copy_nma1_psm1_fix_coeff_3", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_3_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_3_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_3_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_3",
                                              HTML("<b>Hazard plot for Gompertz (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_3"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_3",
                                              actionButton("nma1_psm1_plot_haz_3_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_3", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_3",
                                              HTML("<b>Survival plot for Gompertz (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_3"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_3",
                                              actionButton("nma1_psm1_plot_surv_3_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_3", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_3",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for Gompertz (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_3"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_3",
                                              downloadButton("nma1_psm13_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_3_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_3", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.33 Random-effects model",
                               value = "psmpage26",
                               div(
                                 p(),
                                 uiOutput("output_psm14"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_4",
                                     HTML("<b>Goodness-of-fit estimates (Gompertz (Random-effects model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_4")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_4",
                                     HTML("<b>Coefficient Table for Gompertz (Random-effects model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_4"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_4",
                                     downloadButton("copy_nma1_psm1_fix_coeff_4", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_4_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_4_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_4_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_4",
                                              HTML("<b>Hazard plot for Gompertz (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_4"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_4",
                                              actionButton("nma1_psm1_plot_haz_4_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_4", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_4",
                                              HTML("<b>Survival plot for Gompertz (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_4"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_4",
                                              actionButton("nma1_psm1_plot_surv_4_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_4", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_4",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for Gompertz (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_4"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_4",
                                              downloadButton("nma1_psm14_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_4_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_4", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         ),
                         ### log-logistic
                         tabPanel(
                           title = "3.4 Log-logistic",
                           tabsetPanel(
                             id = "subtabs_loglog",
                             tabPanel(
                               title = "3.41 Input Panel",
                               value = "psmpage27",
                               div(
                                 wellPanel(
                                   h4(strong("Log-logistic distribution")),
                                   br(),
                                   p(),
                                   actionButton("nma1_psm3_run", label="Run Log-logistic (Fixed-effect model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_psm3_runr", label="Run Log-logistic (Random-effects model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_psm3_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_psm3_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("psm3_nchains","number of Markov chains",value = 3),
                                     numericInput("psm3_niter","number of total iterations per chain (including burn in",value = 20000),
                                     numericInput("psm3_nburnin","length of burn in",value = 10000),
                                     numericInput("psm3_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("psm3_pmean","priors of mu[]",value = 0),
                                     numericInput("psm3_pprec","priors of T[]",value = 0.0001),
                                     numericInput("psm3_pr","priors of R[]",value = 0.01)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.42 Fixed-effect model",
                               value = "psmpage28",
                               div(
                                 p(),
                                 uiOutput("output_psm15"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_5",
                                     HTML("<b>Goodness-of-fit estimates (loglogistic (Fixed-effect model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_5")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_5",
                                     HTML("<b>Coefficient Table for loglogistic (Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_5"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_5",
                                     downloadButton("copy_nma1_psm1_fix_coeff_5", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_5_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_5_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_5_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_5",
                                              HTML("<b>Hazard plot for loglogistic (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_5"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_5",
                                              actionButton("nma1_psm1_plot_haz_5_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_5", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_5",
                                              HTML("<b>Survival plot for loglogistic (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_5"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_5",
                                              actionButton("nma1_psm1_plot_surv_5_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_5", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_5",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for loglogistic (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_5"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_5",
                                              downloadButton("nma1_psm15_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_5_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_5", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.43 Random-effects model",
                               value = "psmpage29",
                               div(
                                 p(),
                                 uiOutput("output_psm16"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_6",
                                     HTML("<b>Goodness-of-fit estimates (loglogistic (Random-effects model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_6")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_6",
                                     HTML("<b>Coefficient Table for loglogistic (Random-effects model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_6"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_6",
                                     downloadButton("copy_nma1_psm1_fix_coeff_6", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_6_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_6_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_6_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_6",
                                              HTML("<b>Hazard plot for loglogistic (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_6"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_6",
                                              actionButton("nma1_psm1_plot_haz_6_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_6", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_6",
                                              HTML("<b>Survival plot for loglogistic (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_6"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_6",
                                              actionButton("nma1_psm1_plot_surv_6_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_6", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_6",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for loglogistic (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_6"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_6",
                                              downloadButton("nma1_psm16_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_6_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_6", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         ),
                         ### Log-normal
                         tabPanel(
                           title = "3.5 Log-normal",
                           tabsetPanel(
                             id = "subtabs_lognor",
                             tabPanel(
                               title = "3.51 Input Panel",
                               value = "psmpage210",
                               div(
                                 wellPanel(
                                   h4(strong("Log-normal distribution")),
                                   br(),
                                   p(),
                                   actionButton("nma1_psm4_run", label="Run Log-normal (Fixed-effect model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   actionButton("nma1_psm4_runr", label="Run Log-normal (Random-effects model)",icon("gear"),style="color: black;
                     background-color: white; border-color: #2e6da4"),
                                   p(),
                                   hr(),
                                   HTML("<b>Advanced settings for Rjags(Bayesian analysis)</b>"),
                                   checkboxInput("nma1_psm4_jags_setting","Show advanced settings",value = FALSE),
                                   conditionalPanel(
                                     condition = "input.nma1_psm4_jags_setting == true",
                                     tags$hr(style="border-top: 1px solid black;"),
                                     HTML("<b>Advanced settings for MCMC</b>"),
                                     numericInput("psm4_nchains","number of Markov chains",value = 3),
                                     numericInput("psm4_niter","number of total iterations per chain (including burn in",value = 20000),
                                     numericInput("psm4_nburnin","length of burn in",value = 10000),
                                     numericInput("psm4_nthin","thinning rate",value = 3),
                                     HTML("<b>Advanced settings for priors</b>"),
                                     numericInput("psm4_pmean","priors of mu[]",value = 0),
                                     numericInput("psm4_pprec","priors of T[]",value = 0.0001),
                                     numericInput("psm4_pr","priors of R[]",value = 0.01)
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.52 Fixed-effect model",
                               value = "psmpage211",
                               div(
                                 p(),
                                 uiOutput("output_psm17"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_7",
                                     HTML("<b>Goodness-of-fit estimates (lognormal (Fixed-effect model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_7")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_7",
                                     HTML("<b>Coefficient Table for lognormal (Fixed-effect model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_7"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_7",
                                     downloadButton("copy_nma1_psm1_fix_coeff_7", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_7_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_7_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_7_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_7",
                                              HTML("<b>Hazard plot for lognormal (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_7"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_7",
                                              actionButton("nma1_psm1_plot_haz_7_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_7", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_7",
                                              HTML("<b>Survival plot for lognormal (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_7"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_7",
                                              actionButton("nma1_psm1_plot_surv_7_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_7", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_7",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for lognormal (Fixed-effect model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_7"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_7",
                                              downloadButton("nma1_psm17_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_7_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_7", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               title = "3.53 Random-effects model",
                               value = "psmpage212",
                               div(
                                 p(),
                                 uiOutput("output_psm18"),
                                 p(),
                                 wellPanel(
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_8",
                                     HTML("<b>Goodness-of-fit estimates (lognormal (Random-effects model))</b>"),
                                     textOutput("nma1_psm1_fix_DIC_8")),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_8",
                                     HTML("<b>Coefficient Table for lognormal (Random-effects model)</b>")
                                   ),
                                   DTOutput("nma1_psm1_fix_coeff_8"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_8",
                                     downloadButton("copy_nma1_psm1_fix_coeff_8", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   DTOutput("nma1_psm1_fix_coeff_8_mu"),
                                   p(),
                                   conditionalPanel(
                                     condition = "output.nma1_psm1_fix_coeff_8_mu",
                                     downloadButton("copy_nma1_psm1_fix_coeff_8_mu", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_8",
                                              HTML("<b>Hazard plot for lognormal (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_haz_8"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_haz_8",
                                              actionButton("nma1_psm1_plot_haz_8_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_haz_8", "Download", icon = icon("download"))
                                              )
                                            ),
                                     ),
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_8",
                                              HTML("<b>Survival plot for lognormal (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_surv_8"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_surv_8",
                                              actionButton("nma1_psm1_plot_surv_8_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_surv_8", "Download", icon = icon("download"))
                                              )
                                            ),
                                     )
                                   ),
                                   br(),
                                   fluidRow(
                                     column(width = 6,
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_8",
                                              HTML("<b>Hazard Ratio plot vs reference treatment for lognormal (Random-effects model)</b>")),
                                            plotOutput("nma1_psm1_plot_hr_8"),
                                            p(),
                                            conditionalPanel(
                                              condition = "output.nma1_psm1_plot_hr_8",
                                              downloadButton("nma1_psm18_hrdata_dw", "Download extrapolated HR Data", icon = icon("download"),style="color: black;
                       background-color: white; border-color: #2e6da4"),
                                              p(),
                                              actionButton("nma1_psm1_plot_hr_8_dw", "Download plot panel", icon("circle-down"),
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
                                                downloadButton("downloadPlot_nma1_psm1_plot_hr_8", "Download", icon = icon("download"))
                                              )
                                            )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
          ),
          #### End of PSM ####          
          
        )
      ),
      ##### End of AD NMA Page #####

      #### IPD NMA Page ####
      tabPanel(
        title = "IPD-based NMA",
        tabsetPanel(
      
      #### cox ph ####
      tabPanel(title = "1. COX PH Model",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     h4("COX PH Model",class = "custom-h4")%>%
                       helper(type = "markdown", content = "table1"),
                     p(""),
                     br(),
                     p(""),
                     actionButton("run_nma_coxph", label="Run the model",icon("gear"),style="color: black;
                   background-color: white; border-color: #2e6da4"),
                     hr(),
                     HTML("<b>Advanced settings for JAGS (Bayesian analysis)</b>"),
                     checkboxInput("nma1_cph_jags_setting","Show advanced settings",value = FALSE),
                     conditionalPanel(
                       condition = "input.nma1_cph_jags_setting == true",
                       tags$hr(style="border-top: 1px solid black;"),
                       HTML("<b>Advanced settings for MCMC</b>"),
                       p("Number of Markov chains was set 3 in this model."),
                       numericInput("cph_sim","simulation size",value = 6000),
                       numericInput("cph_nburnin","length of burn in",value = 6000),
                       HTML("<b>Advanced settings for priors</b>"),
                       numericInput("cph_d1","priors of d1[]",value = 0),
                       numericInput("cph_d2","priors of d2[]",value = 0.1),
                       numericInput("cph_d3","priors of d3[]",value = -0.1)
                     )
                   ),
                   mainPanel(
                     tabsetPanel(
                       id="subset_cph_panel",
                       tabPanel(
                         title = "1.1 Instruction",
                         div(
                           wellPanel(
                             h4("COX PH Model",class = "custom-h4"),
                             hr(),
                             p("In the part of COX PH model, we only include Bayesian analysis (Fixed-effect model).
                  Treatment 1 (Code 1) is considered as the reference treatment.
                  Changing the reference treatment can be realized through modifying the data.
                  The Cox PH model was fitted using a two-stage approach according to the method of Freeman SC et.al. (2022).
                  In the first stage a Cox PH model was fitted individually to each trial to obtain an estimate of the log HR for the
                  treatment effect and its corresponding standard error.
                  In the second stage, the treatment effect estimate was synthesised through a standard fixed-effect NMA model."),
                             p("This APP will provide the results of HR (compared with the reference treatment).
                  When HR is obtained, the user can calculate the cumulative hazard for reference treatment (written as cumhaz_ref).
                  Then, multiplying HR with the cumhaz_ref, the cumulative hazard for each treatment (written as cumhaz) can be obtained.
                  Finally, through formula 'exp(-cumhaz)', the survival rate can be calculated.
                    For more information about methodology please refer to the following article:"),
                             p(""),
                             tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/09622802211070253?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", "[1]Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network; Freeman SC et.al. (2022)", target = "_blank"),br(),
                             tags$a(href = "https://link.springer.com/book/10.1007/978-1-4757-3294-8", "[2]Modeling Survival Data: Extending the Cox Model; Terry M et.al. (2000)", target = "_blank"),br(),
                             p(),
                             HTML("For more information about the methodologies and codes of running COX PH model in this APP, please refer to the <b>User Manual</b> panel"),
                             p("")
                            )
                         )
                       ),
                       tabPanel(
                         title = "1.2 Show the COX PH results",
                         div(
                           p(),
                           uiOutput("output_cph"),
                           p(),
                           wellPanel(
                             conditionalPanel(
                               condition = "output.df_nma2_res_hr",
                               HTML("<b>HR for COX PH model (Fixed-effect model, compared to treatment 1)</b>")
                             ),
                             DTOutput("df_nma2_res_hr"),
                             conditionalPanel(
                               condition = "output.df_nma2_res_hr",
                               downloadButton("copy_output.df_nma2_res_hr", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                             )
                           )
                         )
                       )
                     )
                   )
                 )
      ),
      #### end of cox ph ####

      #### gen gamma ####
      tabPanel(title = "2. Generalized Gamma Model",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  h4("Generalized Gamma Model",class = "custom-h4")%>%
                    helper(type = "markdown", content = "table1"),
                  p(""),
                  br(),
                  p(""),
                  numericInput("nma1_gengamma_ex","Extrapolation time for estimated HR(Year)",value = 10,min = 0),
                  p(),
                  actionButton("run_nma_gengamma", label="Run the model",icon("gear"),style="color: black;
                   background-color: white; border-color: #2e6da4"),
                  hr(),
                  HTML("<b>Advanced settings for JAGS (Bayesian analysis)</b>"),
                  checkboxInput("nma1_geng_jags_setting","Show advanced settings",value = FALSE),
                  conditionalPanel(
                    condition = "input.nma1_geng_jags_setting == true",
                    tags$hr(style="border-top: 1px solid black;"),
                    HTML("<b>Advanced settings for MCMC</b>"),
                    p("Number of Markov chains was set 3 in this model."),
                    numericInput("geng_sim","simulation size",value = 6000),
                    numericInput("geng_nburnin","length of burn in",value = 6000),
                    HTML("<b>Advanced settings for priors</b>"),
                    numericInput("geng_d1","priors of init1[]",value = -0.5),
                    numericInput("geng_d2","priors of init2[]",value = 0.5),
                    numericInput("geng_d3","priors of init3[]",value = 0.1)
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    id = "subset_gengamma_panel",
                    tabPanel(
                      title = "2.1 Instruction",
                      div(
                        wellPanel(
                          h4("Generalized Gamma Model",class = "custom-h4"),
                          hr(),
                          p("Similar to COX PH Model, in the part of Generalized Gamma Model, we only include Bayesian analysis (Fixed-effect model).
                  Treatment 1 (Code 1) is considered as the reference treatment.
                  Changing the reference treatment can be realized through modifying the data.
                  The Generalized gamma model was fitted using a two-stage process according to the method of Freeman SC et.al. (2022).
                  In the first stage, each trial was analysed separately using the Generalized gamma model to obtain estimates of the log hazard ratio for the treatment effect and the corresponding standard error.
                  Initially, each trial was independently evaluated by employing the generalized gamma model.
                  This approach facilitated the derivation of log hazard ratio estimates pertinent to the treatment effect, alongside the corresponding standard errors.
                  In the second stage, the treatment effect estimate of the baseline treatment compared to treatment i in trial j was synthesised and its variability was esitmated within a standard fixed-effect NMA model."),
                          HTML("This APP will provide the results of Treatment Effects (TE) (compared with the reference treatment).
                  The user can use 'flexsurv' package to calculate the coefficicents (including 'mu', 'sigma' and 'q') of applying Generalized Gamma Model on the reference treatment.
                  When TE is obtained, the user can calculate the survival rate (S(t)) through the coefficicents and TE.
                  Formula can be written as : S(t)n = 1 - pgengamma(Time, mu = mu + TEn, sigma = sigma, Q=q, ...);
                  Similarly, hazard (h(t)) can be written as : h(t) = hgengamma(Time, mu = mu + TEn, sigma = sigma, Q=q)."),
                          br(),p(),
                  HTML("In this APP, HR will be estimated based on the calculated treatment effects and the hazard of the reference treatment.
                  To be specific, in the example ALK-NSCLC network, Crizotinib in ALEX will be fitted through a gengamma distribution 
                  and the 'mu', 'sigma' and 'q' will be obtained. 
                  Through hazard function and calculated treatment effects, hazards of all the treatments can be calculated.
                  Finally, the HR<sub>12</sub> can be calculated by h<sub>1</sub>(t)/h<sub>2</sub>(t).
                  For more information about methodology please refer to the following article:"),
                          p(""),
                          tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/09622802211070253?rfr_dat=cr_pub++0pubmed&url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org", "[1]Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network; Freeman SC et.al. (2022)", target = "_blank"),br(),
                          tags$a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.2375", "[2]Generalized gamma frailty model; Balakrishnan N et.al. (2006)", target = "_blank"),br(),
                          tags$a(href = "https://www.jstatsoft.org/article/view/v070i08", "[3]flexsurv: A Platform for Parametric Survival Modeling in R; Jackson CH (2016)", target = "_blank"),br(),
                          p(),
                          HTML("For more information about the methodologies and codes of running Generalized Gamma Model in this APP, please refer to the <b>User Manual</b> panel"),
                          p("")
                        )
                      )
                    ),
                    tabPanel(
                      title = "2.2 Show the Gen-Gamma results",
                      div(
                        p(),
                        uiOutput("output_gg"),
                        p(),
                        wellPanel(
                          conditionalPanel(
                            condition = "output.df_nma3_res_hr",
                            HTML("<b>Treatment effect for Generalized Gamma Model (Fixed-effect model, compared to treatment 1)</b>")
                          ),
                          DTOutput("df_nma3_res_hr"),
                          p(),
                          conditionalPanel(
                            condition = "output.df_nma3_res_hr",
                            downloadButton("copy_output.df_nma3_res_hr", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                          ),
                          br(),
                          conditionalPanel(
                            condition = "output.df_gengamma_hr",
                            HTML("<b>HR for Generalized Gamma Model (Fixed-effect model, compared to treatment 1)</b>")
                          ),
                          DTOutput("df_gengamma_hr"),
                          p(),
                          conditionalPanel(
                            condition = "output.df_gengamma_hr",
                            downloadButton("copy_df_gengamma_hr", label="Copy this table",style="color: black;
                       background-color: white; border-color: #2e6da4")
                          )
                        )
                      )
                    )
                  )
                )
              )
        )
      #### end of gen gamma ####
      
        )
      ),
      #### End of IPD NMA Page ####
      
      #### output Page ####
      tabPanel(
        title = "Application output",
        tabsetPanel(
          id = "app_output_page",
          #### Notes ####
          tabPanel(title = "1. Notes on model selection",
                   tabsetPanel(
                     tabPanel(
                       title = "1.1 Within Models",
                       value = "outputpage21",
                       div(
                         wellPanel(
                           h4("Model selection for specific NMA methods",class = "custom-h4"),
                           HTML("<b>For FP model</b>, through the step one frequentist analysis, 
                            the best models with the lowest AIC can be obtained. 
                            But be aware that additional step two analysis should be conducted because overfit may exist
                            in models with the lowest AIC. A check for hazard, survival and hr plots is needed before
                            making a conclusion on the final model.  For example, see the example data (ALK-NSCLC network),
                            for FP, the best models are FP1 with power = 2,3,1. However, for FP1 with power = 2,3, 
                            by visually observing the survival curves, we all found different degrees of overfitting.
                            Thus, FP1 with power = 1 was selected at last."),
                           p(),
                           fluidRow(
                             column(
                               width =6,
                               HTML("<b>Survival plot of FP1, power = 2, example data of ALK-NSCLC network</b>"),
                               tags$img(src = "between_model_selection_fp1.png", style = "max-width: 100%; height: auto; display: block; margin: auto;"),br(),
                               
                             ),
                             column(
                               width =6,
                               HTML("<b>Survival plot of FP1, power = 3, example data of ALK-NSCLC network</b>"),
                               tags$img(src = "between_model_selection_fp2.png", style = "max-width: 100%; height: auto; display: block; margin: auto;"),br(),
                             )
                           ),
                           p(),
                           HTML("<b>For PWE model</b>, the most important part is to select the most suitable cutpoints. 
                            Here are some suggestions on the selection of potential cutpoints: 
                            (1) the time that most survival curves show a plateau period; 
                            (2) the time that there are turning points on hazard plots. In this APP, the AIC
                            of PWE can be compared with FP. That is, through comparing AIC, users can select the best
                            model among all FP and PWE models."),p(),
                           HTML("<b>For PSM</b>, a selection of the best model can be conducted based on the comparison of DIC.
                            However, users should pay attention that not all parametric survival distribution may be
                            suitable for the given data.  For example, see the example data (ALK-NSCLC network), 
                            Log-normal distribution had a poor fit on the data."),
                           p(),
                           fluidRow(
                             column(
                               width =6,
                               HTML("<b>Survival plot of PSM, Log-normal, example data of ALK-NSCLC network</b>"),
                               tags$img(src = "between_model_selection_psm.png", style = "max-width: 100%; height: auto; display: block; margin: auto;"),br(),
                             )
                           ),
                           p(),
                           HTML("<b>For COX PH Model and Gen Gamma Model</b>, there is no need for model selection within models since
                      there will be only one model generated for these two methods.
                      "),p()
                         )
                       )
                     ),
                     tabPanel(
                       title = "1.2 Between Models",
                       value = "outputpage22",
                       div(
                         wellPanel(
                           h4("Model selection among different NMA methods",class = "custom-h4"),
                           HTML("There is currently no authoritative standard for how to select the best model among different NMA models.
                           Comparisons between NMA models are often difficult to achieve due to different methodological compositions.
                           Using different research data may also lead to large changes in choices for models.
                           However, there are many published studies including frameworks and suggestions on selecting NMA models in this field. 
                           Researchers can benefit a lot from these published studies.
                           "),
                           HTML("Some useful references can be found below:<br>
                           <I>[1] Heeg B, Garcia A, Beekhuizen SV, et al. Novel and existing flexible survival methods for network meta-analyses. J Comp Eff Res. Published online September 12, 2022. doi:10.2217/cer-2022-0044 </I><br>
                           <I>[2] Cope S, Chan K, Campbell H, et al. A Comparison of Alternative Network Meta-Analysis Methods in the Presence of Nonproportional Hazards: A Case Study in First-Line Advanced or Metastatic Renal Cell Carcinoma. Value Health. 2023;26(4):465-476. doi:10.1016/j.jval.2022.11.017</I><br>
                           <I>[3] Freeman SC, Cooper NJ, Sutton AJ, Crowther MJ, Carpenter JR, Hawkins N. Challenges of modelling approaches for network meta-analysis of time-to-event outcomes in the presence of non-proportional hazards to aid decision making: Application to a melanoma network. Stat Methods Med Res. 2022;31(5):839-861. doi:10.1177/09622802211070253</I><br>"),
                           p(),
                           HTML("In this app, users can batch export the results (images and tables) of their selected models into a Word document 
                           to facilitate their comparison of results or report writing.
                           Although the results or conclusions on which model performs best will not be provided in this APP, 
                           developers can provide some tricks to help the researchers to make the model selection more rigorous and comprehensive here:"),br(),
                           HTML("<u><b>(1) Although the choice between different NMA models may be difficult at times, it is a good idea
                           to make the entire model selection process transparent and justifiable with the help of existing model selection frameworks.<br>
                           (2) In addition to statistical indicators, visual inspection through plots is also very important. 
                           Some estimates that are obviously counterintuitive can be easily discovered through visual inspection.<br>
                           (3) External evidence and clinical validity will be good evidence to support model choice if available.<br>
                           (4) Sensitivity analyses are highly suggested due to the inherent uncertainty, especially for economic evaluation. Considering 
                           different models in sensitivity analyses should be given priority instead of using only one NMA model in the study.
                           </b></u>")
                         )
                       )
                     )
                   )
          ),
          #### end of notes ####
          
          #### hr output ####
          tabPanel(title = "2. Notes on HR output",
                   div(
                     wellPanel(
                       h4("Notes on the use of extrapolated time-varing HRs",class = "custom-h4"),
                       HTML("For users who want to do the cost-effectiveness analysis of multiple treatments, 
                       they have to conduct the NMA to get the relative treatment effects. Typically, the relative treatment
                       effects can be the HRs. For time-varing HRs, they should have the same follow-up time as the 
                       cost-effectiveness analysis. This means that extrapolation on HRs is needed.
                       In this APP, for each model except COX PH model (time-constant HRs), we provide the output of 
                       estimated HR with the time duration determined by the users. 
                            <b><u>However, users should note that sometimes the extrapolation of time-varing HRs is not reliable, 
                            especially when the data used to run the NMA model is immature.</u></b>
                            Under this condition, continuing to use time-varing HRs to extrapolate may lead to greater biases than using constant HRs 
                            (Based on the results of one unpublished article). Some CEA studies have tried to solve this by using an alternative extrapolation method. 
                            The details of this method can be found as follows:<br>
                            (1) Calculate the (average) time duration with mature survival data for each treatment based on the method of Val Gebski et.al. (2018).<br>
                            (2) For each treatment, for time with mature survival data, use the model estimated HRs as the results; 
                            for time with immature survival data, use the HR of the last time point of the mature data as the result, and extrapolate
                            the HR for the remaining time by this constant HR value.
                            "),br(),p(),
                       HTML("Here is a simple example to illustrate this method: For treatment A, the average time to have mature data is 12 months, 
                       but we want to get the extrapolated HRs of 36 months. 
                       Thus, we set the time point as 12 months. Before the 12th month, we used the model estimated time-varing HRs. 
                            After the 12th month, we used the HR of the 12th month to extrapolate the HRs to the 36th month. That is, 
                            between the 13th month and the 36th month, the HRs are constant."),
                       br(),p(),
                       HTML("Some useful references can be found below:<br>
                            <I>[1] Gebski V, Garès V, Gibbs E, Byth K. Data maturity and follow-up in time-to-event analyses. Int J Epidemiol. 2018;47(3):850-859. doi:10.1093/ije/dyy013</I><br>
                            <I>[2] Wang L, Hong H, Alexander GC, Brawley OW, Paller CJ, Ballreich J. Cost-Effectiveness of Systemic Treatments for Metastatic Castration-Sensitive Prostate Cancer: An Economic Evaluation Based on Network Meta-Analysis. Value Health. 2022;25(5):796-802. doi:10.1016/j.jval.2021.10.016</I><br>
                            <I>[3] Pei R, Shi Y, Lv S, et al. Nivolumab vs Pembrolizumab for Treatment of US Patients With Platinum-Refractory Recurrent or Metastatic Head and Neck Squamous Cell Carcinoma: A Network Meta-analysis and Cost-effectiveness Analysis. JAMA Netw Open. 2021;4(5):e218065. Published 2021 May 3. doi:10.1001/jamanetworkopen.2021.8065</I><br>")
                       
                       
                     )
                   )
          ),
          #### end of hr output ####
          
          #### output ####
          tabPanel(title = "3. Output report",
                   div(
                     wellPanel(
                       h4("Model Run Log",class = "custom-h4"),
                       p(""),
                       HTML("In 'Output Report', exported outcomes including plots and tables will be shown here.
                       All selected outcomes can be merged into one word document.
                       Users can select which outcome to report in their final word report through the checkboxes below. 
                       Extrapolated HR data will not be provided here.
                            Also please note that only successfully exported model outcomes will be shown here. 
                            (Pressing the export data button 
                            and getting a success message is considered a successful attempt) 
                            "),
                       p(),
                       hr(),
                       bsCollapse(
                         id = "output_success_step",
                         bsCollapsePanel(
                           title = "Example of exporting data to this panel successfully  (Click to open / hide this panel)", 
                           HTML("<b>Step One</b>"),br(),
                           tags$img(src = "output_step1.png", style = "max-width: 100%; height: auto; display: block; margin: auto;"),br(),
                           HTML("<b>Step Two</b>"),br(),
                           tags$img(src = "output_step2.png", style = "max-width: 100%; height: auto; display: block; margin: auto;"),br(),
                           style = "info"
                         )
                       ),
                       p(""),
                       uiOutput("model_checkboxes"),
                       p(""),
                       hr(),
                       downloadButton("downloadWord", label="Download the Word File",style="color: black;
                       background-color: white; border-color: #2e6da4")
                     )
                   )
          )
          #### end of output ####
          
        )
      ),
      #### End of IPD NMA Page ####

      #### User Manual ####

      tabPanel(title = "User Manual",div(style = "text-align: center;",tags$iframe(src = "NMAsurv_user_manual.pdf", width = "100%", height = "1440px")),
      )

      #### End of User Manual ####

# Do not modify the below
    )
  )
)

