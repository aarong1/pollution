# https://www.color-hex.com/color-palette/1039605
# https://colordrop.io/palette/63568
# earth greens



library(htmltools)
library(shiny)
library(echarts4r)
library(bslib)
library(readxl)
# library(leaflet)
library(reactable)
library(fst)
library(tidyverse)
# library(sf)

library(rsconnect)
library(sparkline)
# load(".RData")

# rm(past_populations,
#    instantiate_base_pop,
#    default_fracture4_female,
#    th,
#    test_poopulation,
#    teset_population,stroke_incidence,
#    population_w_established_prevalence,new_year_pop,wrapping_examples_in_function)
# rm(list=ls())

system("ls -lh")        # list files
system("echo Hello!")   # print message

source('./components/footer.R')
source('./components/legend.R')
source('./components/disabled_risk_to_intervene_on.R')
source('./components/hatched_sub_title.R')
source('./components/scenarios_div.R')



source('modules/startup_overlay/startup_overlay_div.R')
# source('modules/specificInterventionModule_3.R')
# source('modules/chart_update_module_3/chart_update_module.R')
# source('modules/intervention_module2/intervention_module.R')
source('modules/slide_panel_module/slide_panel_module.R')

# source('load_graphs_sppg.R')
# source('chtgpt_banner_metrics_suggestion2.R')
# source('sppg_div.R')

# x <- stroke_age20[[2]]
# x$dependencies = NULL
# x <- x |> e_hide_grid_lines()

print('#################################')
print(system.file(package = "sparkline"))
print(dir(system.file("htmlwidgets/lib", package = "sparkline")))
print(file.exists(system.file("htmlwidgets/lib/jquery.sparkline/jquery.sparkline.min.js", package = "sparkline")))

print('#################################')

graph_wrapper <- function(..., header =NULL){
  
  div(class = "grid-item grid-item--graph theme-green",
      div(class = "grid-item-content",
          div(class = "chart-card",
              if(!is.null(header)){
                div(class = "card-header",# style = "font-size: 0.5em;",
                    header
                )},
              ...
          )
      )
  )
}

# theme_x <- readLines('theme.json')

ui <- page_fluid(
  theme = bs_theme(version = 5,
                   bootswatch = 'litera',
                   font_scale = 0.9,
                   primary = 'black'),
  
  startup_overlay_div(5000,7000),
  slide_panel_ui('slide',panel_title = 'Hello, Write text and collect graphs here by drag and dropping'),
 
  div(id = 'main-content', #class = "visually-hidden",
      
      #style = "opacity:0;",
      
#       tags$head(
#         tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/css/ion.rangeSlider.min.css"),
#         # Custom CSS styling
#         tags$style("
# 
# .irs-line {
#     /* background: linear-gradient(0.25turn, #3cb3716e, #ffa5007a, #ff00006b); */ 
#     background: linear-gradient(0.25turn, #3cb371e8, white, #00b0ff) !important;
#   }
#   
#  .irs-bar {
#     background: #ffffffbf !important;
#   }
#                     
# .selectize-input{
#     max-height:75px;
#     overflow-y:auto;
# }
# 
# accordion-button.collapsed {
#     transition:background 3s ease-in-out;
#     background:white; 
#     }
# 
# .accordion-button:not(.collapsed) {
#     transition:background 3s ease-in-out;
#     background: linear-gradient(140deg, var(--bs-success), 5%, white);
#     border-radius:12px
# }
# 
# .accordion-button{
#     transition:background 3s ease-in-out;
#     background: linear-gradient(10deg, var(--bs-success), 25%, white);
#     border-radius:12px
# }
# 
# .accordion,.accordion-item{
#     border: solid white 2px;
#     }
# 
#  /* * { box-sizing: border-box; } */
# 
# html{font-size:70%}
# ")),
      
      tags$head(
        # Include external dependencies
        # Custom CSS styling
        includeCSS('./www/styles.css'),
        HTML("<link rel='stylesheet' href='progress_bar.css' />"),
        
      ),
      
      # Main layout container
      
      # Control Panel
      
      div(class = 'w-100 navbar p-0 fixed-top border-0 d-flex gap-0 flex-column flex-wrap glass-card',
          shiny::tags$nav(
            #style='position:fixed;top:0px;left:0px',
            class = "navbar w-100 glass-card p-3 rounded", # bg-light mt-5
            shiny::h4(  "Population Health", span(class = 'lead','Public Health Agency'),
                        
                        HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-success ">Environmental Risk </span></h7>'),
                        HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-success ">Air Quality </span></h7>'),
                        span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">PHA', '' ,'</span></h7>')),
                        span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">DEARA', '' ,'</span></h7>')),
                        HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">Population Health Model </span></h7>'),
                        HTML('<h7 id = "move" style = "float:right;">
                       <!-- <a href="https://apply-for-innovation-funding.service.gov.uk/competition/2186/overview/e51c18bc-21b3-450d-bdbc-2f43dad3b268">
                       <span class="badge rounded-pill bg-light">
                        <i class="fa-solid fa-arrow-up-right-from-square"></i>
                       OPIP bid</span>
                       </a> -->
                       </h7>'),
                        class = "navbar-brand mb-0 w-100"
            )
          ),
          
          # div(class='mt-5 pt-5'),##position:fixed;top:0px;
          HTML('<nav id = "top-top"
                                  style = "width:100%;
                                  margin-inline:-12px;backdrop-filter: blur(5px);
                                  /*background: #00D4FF;
                                  background: linear-gradient(55deg, orange 55%, #FFD700 87%); */
                                  background: linear-gradient(55deg, #7e9b7f 15%, #c9e4c5 8%);",

                                  class=" w-100 bg-gradient-blue-cyan bg-opacity-50">
                      <div class="text-white">
                      <div id ="top-nav-content" class="d-flex gap-5 justify-content-center align-items-center">
                      <a href="#overview-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-tachometer-alt"></i>
                      <span class="fs-6">Overview</span>
                      </a>
          <i class="fa-solid fa-chevron-right"></i>
          <a href="#evidence-section" class="nav-item bg-opacity-100 nav-iten p-1 m-3 px-4 rounded-pill text-white">
          <i class="fas fa-exclamation-triangle"></i>
          <span class="fs-6">Research</span>
          </a>
                      <i class="fa-solid fa-chevron-right"></i>
                      <a href="#prevalence-section" class="nav-item bg-opacity-100 nav-iten p-1 m-3 px-4 rounded-pill text-white">
                      <i class="fas fa-tachometer-alt"></i>
                      <span class="fs-6">Reports</span>
                      </a>
                      <!-- <a href="#incidence-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-map-marked-alt"></i>
                      <span class="fs-6">Incidence</span>
                      </a>
                      <a href="#downstream-section" class="nav-item bg-opacity-100 nav-iten p-1 m-3 px-4 rounded-pill text-white">
                      <i class="fas fa-users"></i>
                      <span class="fs-6" > Dashboard</span> -->
                      </a>
                      </div>
                      </div>
                      </nav>')
          
      ),
      
      
      
      div(style='top:12vh;width:12vw;z-index:1000',class = ' ms-2  p-3 d-flex flex-column display-absolute position-fixed left-0 ',#shadow-sm glass-card
          div(class = "nav-section",
              h6(class = "bg-opacity-100 border-5  p-2 rounded-3  text-bg-dark text-light", "Pollution and Air Quality on Health "), #text-body-secondary
              div(class = "nav-item",
                  span(class = "nav-icon"),
                  "More pages exist. Scroll for more of this one"
              )
          )
          
        
          
      ),
      


      div(class = "main-container", `data-bs-spy` = "scroll", `data-bs-target` = "#top-nav-content", `data-bs-offset` = "70",
          # div(class='mt-5 pt-5')
          # Main content area
          div(class = "content-area",
              # div( class="tab-pane active", id="overview", role="tabpanel", `aria-labelledby`="overview",
              
              div(class = "tab-content",
                  
                  # Dashboard Tab Content
                  
                  div(id = "dashboard-tab", class = "tab-pane show active", #
                      
                      div(class = "container-fluid",
                          style = "padding-left: 0%;padding-top:8%;padding-right:3%",
                          
                          div(
                            tags$head(
                              tags$style(HTML("

      #scrollspy-nav i {
  /*
  color: grey;
  Change icon color */
      }

      #scrollspy-content h5 {
  color: grey;         /* Change icon color */
  margin-left:1rem;
  margin-top:10px;

      }

            #scrollspy-content span {
  font-size: 90%;;
/*
  margin-left:2rem;
  padding-bottom:1.5rem; 
*/
  color:rgba(0,0,0,0.8);

            }

            /* #intro span, */
            #intro p,
            .nav-card-description {
              display: block;
              margin-right: 8%;
            }

                        #scrollspy-content div {
/*
  margin-left:0.5rem;
  padding-bottom:0.5rem;
*/

      }

           #intro h5 >h5 {
  color: grey;         /* Change icon color */
  margin-left:2rem;

           }

                 #intro p {
  color: dimgrey;         /* Change icon color */
  margin-left:2rem;

                            }

      #scrollspy-nav {

        border-radius:15px;
        /*
        position: sticky;
        top: 22vh;
        right: 10px;
        width: 80px;
        */
      }

      #scrollspy-content {
        margin-left: 50px;
        /*height: 6000px;*/
        overflow-y: scroll;
        position: relative;
      }

     #scrollspy-nav > a.nav-link.active{
      background-color:dar;
      color:white;
      border-radius:4px;
      }

      #scrollspy-nav a.nav-link.active{
      background-color:rgb(177,227,199);
      color:white;
      border-radius:4px;
      }

      .section {
        height: 400px;
        padding-top: 60px;
      }
    "))
                            ),
                            

                            tags$head(
                              tags$script(
                                "
document.addEventListener('shown.bs.tab', function (event) {
                                          const scrollSpy = bootstrap.ScrollSpy.getInstance(document.body);
                                          if (scrollSpy) {
                                            //scrollSpy.refresh();
                                          } else {
                                          console.log('1');
                                            new bootstrap.ScrollSpy(document.body, {
                                              target: '#scrollspy-nav',
                                              offset: 0
                                            });
                                          }
                                        });

                                          // Re-initialize ScrollSpy every time a tab is shown
                                      document.addEventListener('DOMContentLoaded', function () {
                                          const scrollSpy = bootstrap.ScrollSpy.getInstance(document.body);
                                          if (scrollSpy) {
                                            //scrollSpy.refresh();
                                          } else {
                                          console.log('1');
                                            new bootstrap.ScrollSpy(document.body, {
                                              target: '#scrollspy-nav',
                                              offset: 0
                                            });
                                          }
                                        });"
                              )),
                            
                            # Scrollspy Nav
                            
                            # div( class="row",
                            #   div( class="col-2",
                            #div(style='',class = ' ms-2 p-3 d-flex flex-row',#shadow-sm glass-card
                            
                            div(class = 'bg-white rounded-3',style = 'margin-left:12%;padding-left:2%;min-height:100vh;',
                                id = "scrollspy-content",
                                `data-bs-spy` = "scroll",
                                `data-bs-target` = "#scrollspy-nav",
                                #`data-bs-offset` = "0",
                                #tabindex = "0",
                                style = "position: relative; overflow-y: auto; scroll-behavior: smooth;", # height: 100vh;
                                
                                
                                tags$div( id = "intro",
                                          h2('Pollution',class = 'm-5 p-3 border-bottom border-info'),
                                          tags$span(class ='',"Air Quality in Northern Ireland is not significantly different than the rest of the UK. The exception is larger cities and areas with a 
                                                    more substantial manufacturing base. There is a gradient between higher concentration in urban areas and lower concentration in rural regions.
                                                     There are different types of pollution that affect the physiology in different ways, but affects mostly the ", tags$b('Respiratory'), 'and', tags$b('Cardiovascular'), " systems."),
                                          
                                          
                                          h4('Types',class = 'm-5 p-3 border-bottom border-top border-success'),
                                          
                                          tags$div(id = "item-1",class = 'pb-3',
                                                   tags$h4("Particulate Matter (PM2.5)"),
                                                   tags$span(class = 'ps-2', "Only three sites in Northern Ireland have been
measuring PM2.5 concentrations for at least
five years (this being considered the minimum
            for estimation of trends). Annual mean
concentrations at these sites showed a decrease
between 2019 and 2020, likely due to the COVID-19
restrictions, but since then have remained fairly
stable: 7-9 µg m-3 at urban sites, 4 µg m-3 at the
rural Lough Navar. ")
                                          ),
                                          
                                          tags$div(id = "item-11",class = 'pb-3',
                                                   tags$h4(class = 'd-inline', 'Particulate Matter (PM10)'),
                                                   tags$span(class = 'd-inline text-secondary',"")
                                          ),
                                          
                                          tags$div(id = "item-2",class = 'pb-3',
                                                   tags$h4(class = 'd-inline', 'Sulphur dioxide'),
                                                   tags$span(class = 'd-inline text-secondary',"NSO₂")
                                          ),

                                          tags$div(id = "item-4",class = 'pb-3',
                                                   tags$h4(class = 'd-inline','Nitrogen oxides'),
                                                   tags$span(class = 'd-inline',"NOₓ")
                                          ),
                                          
                                          tags$div(id = "item-5",class = 'pb-3',
                                                   tags$h4(class = 'd-inline','Ammonia'),
                                                   tags$span(class = 'd-inline',"NH₃"),
                                                   tags$div(id = "item-5-1",
                                                            # tags$h5("Note 1"),
                                                            tags$span("")
                                                   ),
                                                   tags$div(id = "item-5-2",
                                                            # tags$h5("Note 2"),
                                                            tags$span(style='font-weight:bold;',"")
                                                   ),
                                                   tags$div(id = "item-5-3",
                                                            #tags$h5("Note 3"),
                                                            tags$span(style='font-weight:bold;',"")
                                                   ),
                                                   # tags$div(id = "item-5-4",
                                                   #          #tags$h5("Note 4"),
                                                   #          tags$span("Number of patients aged 18 or over with CKD with classification of categories G3a to G5 (previously stage 3 to 5).")
                                                   # ),
                                                   tags$div(id = "item-5-5",
                                                            #tags$h5("Note 5"),
                                                            tags$span("")
                                                   ),
                                                   tags$div(id = "item-5-6",
                                                            #tags$h5("Note 6"),
                                                            tags$span("")
                                                   )
                                          ),
                                          
                                          tags$div(id = "item-6",class = 'pb-3',
                                                   tags$h4(class = 'd-inline',"Non-methane volatile organic compounds"),
                                                   tags$span(class = 'd-inline',"NMVOCs"),
                                                   tags$div(id = "item-6-1",
                                                            #tags$h5("Cancer - Note 1"),
                                                            # tags$span(style='font-weight:bold;',"Because of the date cut-off in the definition of this register, prevalence trends are obscured by the increase in the size of the register due to the cumulative accrual of new cancer cases onto practice registers with each passing year.")
                                                   )
                                          ),
                                          
                                         
                                          
                                          tags$div(id = "item-9",
                                                  #  tags$h5("Sources of Environmental Risk"),
                                                     h4('Sources',class = 'm-5 p-3 border-bottom border-top border-success'),
                                                   tags$span(class ='lead',"The contributing sources of differing types of pollution"),br(),br(),
                                                   tags$div(id = "item-9-1",
                                                            
                                                            div(class ='d-flex flex-row gap-5 justify-content-center',
                                                                div(class = ' shadow-sm rounded-3 p-3 bg-white w-50', sources),
                                                                img(class = ' rounded-4 p-3 bg-white', src ='pollution_sources.jpg', style='width:35%;')
                                                            ),
                                                            div(class = 'visually-hidden',
                                                                pm25_chart
                                                                ),
                                                            
                                                          h4('PM2.5',class = 'm-5 p-3 border-bottom border-top border-success'),
                                                            tags$span(class ='lead',"Particulate Matter, or dust, that is 2.5 microns across is the most impactful to non-Communicable Disease Health."),
                                                            HTML(" <div id='main' style='width:100%;height:400px'></div>
<script>

document.addEventListener('DOMContentLoaded', function () {

var chartDom = document.getElementById('main');
var myChart = echarts.init(chartDom);
var option;

const Icons = {
  Car: './car_van_2_icon_png.png', //ROOT_PATH + '/data/asset/img/weather/sunny_128.png',
  Overcrowding: './25694.png',
  Job: './briefcase_2_icon_png.png',
  Ownership: 'key.png',
  Energy : './icons/bolt-solid.png',
  Manufacturing:   './icons/oil-well-solid.png',
  Industrial : './icons/industry-solid.png',
  Residential :   './25694.png',
  Agriculture  : './icons/tractor-solid.png',
  Road : './car_icon_png.png',
  Fugitive  :  './icons/smog-solid.png',
  NonRoad :'./icons/ship-solid.png',
};

const seriesLabel = {
  show: true
};
option = {
  title: {
    //text: 'Townsend Deprivation'
  },
  tooltip: {
    trigger: 'axis',
    axisPointer: {
      type: 'shadow'
    }
  },
  legend: {
    //data: ['City Alpha', 'City Beta', 'City Gamma', 'City Delta']
  },
  grid: {
    left: 100
  },
  toolbox: {
    show: true,
    feature: {
      saveAsImage: {}
    }
  },
  xAxis: {
    type: 'value',
    //name: '%',
   // axisLabel: {
   //   formatter: '{value}',
   //   textStyle: {
   //     align: 'right'
   //   }
   // },

    splitLine:{ show: false },
    axisLine: { show: false },
    axisTick: { show: false },
    axisLabel: { show: false }, 
  },
  yAxis: {
    splitLine:{ show: false },
    axisLine: { show: false },
    axisTick: { show: false },
    type: 'category',
    inverse: false,
    data: [
    'Energy',
    'Manufacturing',
    'Industrial',
    'Residential',
    'Agriculture',
    'Road',
    'Fugitive',
    'NonRoad'
],
    axisLabel: {
           align: 'right',
            textStyle: {
        align: 'right'
      },
      formatter: function (value) {
        return '{' + value + '| }';
      },
      margin: 20,
      rich: {
Manufacturing: {  
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Manufacturing
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Energy: {  
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Energy
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Industrial: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Industrial
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Residential: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Residential
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Road: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Road
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Agriculture: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Agriculture
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
Fugitive: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.Fugitive
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
},
NonRoad: {
  height: 30,
  width: 30,
  align: 'right',
  backgroundColor: {
    image: Icons.NonRoad
  },
  borderRadius: 10,
  borderWidth: 0,
  borderColor: '#ff8811'
}
}
}
},
series: [
  {
    name: 'PM2.5',
    type: 'bar',
    itemStyle: {
      borderRadius: 5,
      color: '#000000'
    },
    data: [3.3, 16.1, 12.9, 43.1, 4.0, 12.4, 1.1, 3.6]
    
  }
  
]
};

option && myChart.setOption(option);

}); // DOMContentLoaded
</script>")
                                                   )
                                          ),
                                          tags$div(id = "item-10",
                                                  
                                          div(id = "Links",class='min-vh-25 p-5 m-5 w-75',
                                              div(class="text-dark bg-white fs-5 p-3 rounded-2 my-5 opacity-100 shadow-sm", h5(class = 'd-inline','Links'), span(class='d-inline text-secondary','Resources that prove useful')),
                                              div(class='justify-content-evenly min-vh-50 flex-wrap',#grid
                                                  # d-flex flex-row gap-2 py-5
                                                  
                                                  div(class = "grid-item nav-card float-left analytics bg-opacity-50",
                                                      div(onclick = "window.open('https://www.airqualityni.co.uk/air-quality/health-effects','_blank')",
                                                          class = "nav-card-icon",
                                                          icon("arrow-up-right-from-square")),
                                                      div(class = "nav-card-title", "Air Quality NI"),
                                                      div(class = "nav-card-description text-wrap ", "Northern Ireland 2023 Air Quality Health Impact Report"),
                                                  ),
                                                  div(class = "grid-item nav-card float-left settings bg-opacity-50",
                                                      div(onclick = "window.open('https://www.gov.uk/government/publications/health-matters-air-pollution/health-matters-air-pollution','_blank')",
                                                          class = "nav-card-icon",
                                                          icon("arrow-up-right-from-square")),
                                                      div(class = "nav-card-title", " UK Gov"),
                                                      div(class = "nav-card-description", "Guidance on Health impact of Air pollution"),
                                                  ),
                                                  

                                                  # tags$h5("Resources"),
                                                  # tags$span("Resources"),
                                                  tags$div(id = "item-10-1",
                                                           # tags$h5("Note 1"),
                                                           # tags$span("Resources"),
                                                           
                                                           lapply(
                                                             list(
                                                               list('NI Air Quality', 'https://www.airqualityni.co.uk/uploads/reports/66fe5c057bf6f-air-pollution-in-northern-ireland-2023-issue1-screen-optimised-672399cdd209a620008745.pdf'),
                                                               # list(2, 'https://www.airqualityni.co.uk/air-quality/health-effects'),
                                                               list('Microsimulation', 'https://www.sciencedirect.com/science/article/pii/S0048969719340823'),
                                                               # list(4, 'https://www.gov.uk/government/publications/health-matters-air-pollution/health-matters-air-pollution'),
                                                               # list(5, 'https://static-content.springer.com/esm/art%3A10.1038%2Fs41366-021-00849-8/MediaObjects/'),
                                                               list('London Risk Modelling', 'https://erg.ic.ac.uk/research/home/resources/ERG_ImperialCollegeLondon_HIA_AQ_LDN_11012021.pdf'),
                                                               list('Polluton and the National Health Service', 'https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002602'),
                                                               list('Costs of Pollution', 'https://assets.publishing.service.gov.uk/media/5afef7a0e5274a4be3231bd0/Estimation_of_costs_to_the_NHS_and_social_care_due_to_the_health_impacts_of_air_pollution_-_summary_report.pdf'),
                                                               list('Romanian Modelling', 'https://www.sciencedirect.com/science/article/pii/S2214750022000506'),
                                                               list('Pollution and Stroke', 'https://www.ahajournals.org/doi/10.1161/STROKEAHA.122.035498')
                                                             ) |> (\(x) Map(c, x, seq_along(x)))(),
                                                             function(r) {
                                                               div(class = paste("grid-item nav-card float-left bg-opacity-50",
                                                                                 if (r[[3]] %% 2 == 1) "analytics" else "settings"),
                                                                   div(onclick = sprintf("window.open('%s','_blank')", r[[2]]),
                                                                       class = "nav-card-icon",
                                                                       icon("arrow-up-right-from-square")),
                                                                   div(class = "nav-card-title",  r[[1]]),
                                                                   div(class = "nav-card-description", sub("^https?://([^/]+).*", "\\1", r[[2]]))
                                                               )
                                                             }
                                                           )
                                              )
                                              )
                                                  ),
                                                  
                                                  div(class = "grid-item nav-card float-bottom w-25 reports bg-opacity-50",
                                                      div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'research-tab').addClass('active show');",
                                                          class = "nav-card-icon",
                                                          icon("arrow-up-right-from-square")),
                                                      div(class = "nav-card-title", "Go to Scenarios"),
                                                      div(class = "nav-card-description", " Intervene on population risk"),
                                                  ),
                                                  # 
                                                  div(class = "grid-item nav-card float-bottom w-25 reports bg-opacity-50",
                                                      div(onclick = "  $('.tab-pane').removeClass('active show');$('#' + 'reports-tab').addClass('active show');",
                                                          class = "nav-card-icon",
                                                          icon("arrow-up-right-from-square")),
                                                      div(class = "nav-card-title", "Go to intervene tab"),
                                                      div(class = "nav-card-description text-wrap ", "Rudimentary estimates of devolved nations, adult and children adhd prevalence"),

                                                  ),
                                                  # 
                                                  # div(class = "grid-item nav-card float-left settings bg-opacity-50",
                                                  #     div(onclick = "window.open('https://digital.nhs.uk/data-and-information/publications/statistical/mi-adhd/may-2025','_blank')",
                                                  #         class = "nav-card-icon",
                                                  #         icon("arrow-up-right-from-square")),
                                                  #     div(class = "nav-card-title", "NHS England"),
                                                  #     div(class = "nav-card-description", "Experimental Mental Health Statistics"),
                                                  # )
                                              
                                          ),
                                ),
                            ),
                                          div(id = "outro", class = "pt-5", h2("Notes")),
                            
                      )
                      )
                  ),# End of dashboard grid
                  
   #       div(id = "research-tab", class = "tab-pane ", #
              
  #             #div(class = 'col-9',
  #             
  #             # Scrollspy content container — requires height and overflow
  #             div(style = 'padding-left:12%;padding-top:5%;margin-left:3%;',
  #                 id = "scrollspy-content",
  #                 `data-bs-spy` = "scroll",
  #                 `data-bs-target` = "#scrollspy-nav",
  #                 #`data-bs-offset` = "0",
  #                 #tabindex = "0",
  #                 style = "position: relative; overflow-y: auto; scroll-behavior: smooth;", # height: 100vh;
  #                 
  #                 # Stroke Section,
  #                 
  #                 div(id = "stroke", class = "pt-5", h2("Select Intervention")),
  #                 
  #                 ################################################################
  #                 interventionSelectorUI("people_intervention_selector"),
  #                 div(class = "row g-2 align-items-start",
  #                     div(class = "col-6",
  #                         intervention_module_ui("intensity_intervention_selector", chart_height = "220px")
  #                     ),
  #                     div(class = "col-6",
  #                         chartUpdateModuleUI("loading_scenarios_chart")
  #                     )
  #                 ),
  #                 
  #                 h2("Risk to modulate"),
  #                 disabled_risk_to_intervene,
  #                 
  #                 actionButton(inputId = 'reset', label = 'Reset ', icon = icon('rotate-left'), class = 'float-right'),
  #                 actionButton(class='float-right pb-2 mb-3 ',
  #                              inputId = 'run_intervention_w_selectors','Run Intervention',
  #                              icon = icon('play')),
  #                 
  #                 
  #                 ################################################################
  #                 
  #                 
  #                 
  #                 
  #                 
  #                 
  #                 
  #                 div(id = "Intervention", class = "pt-5", h2("Risk to intervene on")),
  #                 selectizeInput(
  #                   inputId = "disease",
  #                   label = "Disease",
  #                   choices = character(0),
  #                   selected = NULL,
  #                   options = list(
  #                     valueField = "email",
  #                     labelField = "name",
  #                     render = I("{
  #   item: function(item, escape) {
  #     console.log(item);
  #     var name = item.email ? '<span class=\"name\">' + item.email + '</span>' : '';
  #     return '<div class =  m-2 p-2>' + '<span class=\"email m-2\">' + item.name + '</span></div>';
  #   },
  #   option: function(item, escape) {
  #     var label = item.name || item.email;
  #     var caption = item.name ? item.email : null;
  #     return '<div class =  \"m-2 p-2 rounded-3 \">' +
  #      (caption ? '<span class=\"label\">' + item.email + '</span>' : '') 
  #       
  #     '</div>';
  #   }
  # }") 
  #                   )),
  #                 
  #                 
  #                 
  #             )
  #             
  #             
  #         
  #         
  #         
  #  ),
                  div(id = "reports-tab", class = "tab-pane ", #
                  
                      div(class = 'm-5 pt-5', 
                          style = 'padding-left:15%;width:800px;',
                        sm_hatched_subtitle('Scenarios')
                        ),
                      scenarios_div()
                      )
                  
              ),#, # End of dashboard tab content
              
      
              
          ) # End of tab-content-container
          
      ), # Close content-area
      
      # Initialize Packery with Click and Expand JavaScript
      tags$script(HTML("
$(document).ready(function() {

    // Tab Navigation Functionality
    $('.nav-item').on('click', function() {
      // Remove active class from all nav items
      $('.nav-item').removeClass('active');
      // Add active class to clicked item
      $(this).addClass('active');

      // Hide all tab content
      $('.tab-pane').removeClass('active show')

      // Show corresponding tab content based on nav item text
      var navText = $(this).text().trim().toLowerCase();
      var tabId = '';

      switch(navText) {
        case 'dashboard':
          tabId = 'dashboard-tab';
          break;
        case 'research':
          tabId = 'research-tab';
          break;
        case 'reports':
          tabId = 'reports-tab';
          break;
        case 'geography':
          tabId = 'geography-tab';
          break;
        case 'deprivation':
          tabId = 'deprivation-tab';
          break;
        case 'population':
          tabId = 'population-tab';
          break;
        case 'intervention':
          tabId = 'intervention-tab';
          break;
        case 'specify':
          tabId = 'specify-tab';
          break;
        case 'scenarios':
          tabId = 'scenarios-tab';
          break;
        case 'obesity risk':
          tabId = 'ObesityRisk-tab';
          break;
        case 'northern trust':
          tabId = 'NorthernTrust-tab';
          break;
        case 'lifestyle':
          tabId = 'lifestyle-tab';
          break;
         case 'society':
          tabId = 'society-tab';
          break;
        default:
          tabId = 'dashboard-tab';
      }

      $('#' + tabId).addClass('active show')

      // Re-initialize packery if dashboard tab is shown
      if (tabId === 'dashboard-tab') {
        setTimeout(function() {
          $('.grid').packery();
        }, 100);
      }

      // Initialize pivot module if reports tab is shown
      if (tabId === 'reports-tab') {
        setTimeout(function() {
          // Trigger pivot module column update
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('pivot_reports_tab_shown', Math.random());
          }

          // Re-trigger any drag-drop initialization if needed
          if (typeof window.setupDropzone === 'function') {
            console.log('Re-initializing pivot drag-drop zones');
            window.setupDropzone('#column-pool');
            window.setupDropzoneCat('#groups-drop');
            window.setupDropzoneCat('#wide-by-drop');
            window.setupDropzone('#values-drop');
            window.setupDropzoneValueFunc('#value-func-drop');
          }
        }, 200);
      }
    });

  // Initialize Bootstrap Scrollspy
  var scrollspyEl = document.querySelector('[data-bs-spy=\"scroll\"]');
  if (scrollspyEl) {
    var scrollspy = new bootstrap.ScrollSpy(scrollspyEl, {
      target: '#top-nav-content',
      offset: 80
    });
  }

});
"
               
      ), # End tags$script
      ), # End main-container
      
      footer()
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
#   runButton <- reactiveVal(NULL)
#   
#   observeEvent(input$run_intervention_w_selectors,ignoreInit = T,ignoreNULL=T,{
#     runButton(input$run_intervention_w_selectors + 1)
#   })
#   
#   pop_reactive <- reactive(
#     past_populations %>% 
#       filter(year==min(year),run==1)
#   )
#   
#   api <- interventionSelectorServer("people_intervention_selector", data = pop_reactive)
#   
#   targeted_pop <- reactive(
#     {
#       reached <- api$data () %>% filter(intervention_reached ==T)
#       targets <- api$data () %>% filter(intervention_target ==T)
#       
#       print(count(api$data(),intervention_reached))
#       print(count(api$data(),intervention_target))
#       print(count(reached,intervention_reached))
#       print(count(targets,intervention_target))
#       
#       print(  past_populations %>% 
#                 mutate(intervention_target = ifelse(id %in% targets$id,T,F)) %>% 
#                 mutate(intervention_reached = ifelse(id %in% reached$id,T,F)) %>% 
#                 summarise(sum(intervention_target)/n()*100,
#                           sum(intervention_reached)*100/n() )
#       )
#       
#       past_populations %>% 
#         mutate(intervention_target = ifelse(id %in% targets$id,T,F)) %>% 
#         mutate(intervention_reached = ifelse(id %in% reached$id,T,F))
#     }
#   )
#   
#   result <- intervention_module_server("intensity_intervention_selector", 
#                                        reactive({runButton()}
#                                                 ))
#   
#   simulation_state <- chartUpdateModuleServer("loading_scenarios_chart", 
#                                               reactive({runButton()}),
#                                               targeted_pop(),
#                                               input$draggable_data
#                                               )
# 
#   output_df <- reactiveVal({past_populations})
#     
# observe({ 
#   
#     # print(simulation_state$past_populations())
#     
#     # print(simulation_state$simulation_active)
# 
#     if(nrow(simulation_state$results())>0){
#       
#       message('run data packet')
#       
#       target_populations_df <- simulation_state$results()
#       
#       t_past_populations <- targeted_pop() %>% 
#         filter(run <= model_specification$model$number_of_runs ) %>%
#         filter(year <= max(target_populations_df$year) &
#                  year >= min(target_populations_df$year)
#         ) %>%
#         # take out runs and year previously, but not now modelled
#         filter(intervention_reached) %>%
#         # take out runs those that are targeted
#         bind_rows(target_populations_df ) %>%
#         # put in those that are targeted
#         mutate(intervention = 'intervention')
#       # mark as an intervention
#       
#       t_past_populations <- t_past_populations %>% 
#         select(-any_of( c('cmms', 'multimorbidity'))) %>% 
#         compute_cmms() %>% 
#         add_multimorbidity_fn()
#       
#       total_pop <- bind_rows(
#         targeted_pop() %>% 
#           filter(run <= max(target_populations_df$run) ) %>%
#           filter(year <= max(target_populations_df$year) &
#                    year >= min(target_populations_df$year)
#           ) %>%
#           mutate(intervention = 'non-intervention'),
#         t_past_populations
#       )
#       
#       # total_pop <- compute_cmms(total_pop)
#       # total_pop <- add_multimorbidity_fn(total_pop)
#       
#       qsave(total_pop, 'total_pop.qs')
#       output_df(total_pop)
#       
#     }else{
#       
#       message('no run yet')
#       output_df(past_populations)
#       
#     }
#     
#     
#     output$stroke_incidence_chart <- renderEcharts4r({
#       # req(simulation_state$past_populations())
#       message('stroke plot')
#       # simulation_state$past_populations() %>%
#       
#       # simulation_state$results()  %>%
#       
#       output_df() %>% 
#      # simulation_state$past_populations() %>% 
#         # simulation_result() %>%
#         mutate(year = as.character(year)) %>% 
#         filter(stroke == year) %>%
#         count(run, intervention, year) %>%
#         # group_by(intervention, year) %>%
#         # summarise(n = mean(n)) %>%
#         # mutate(year = as.character(year),
#         #        n = n*model_specification$population$scale_down_factor) %>%
#         # group_by(intervention) %>% 
#         e_charts(year) %>%
#         e_line(n) %>%
#         e_tooltip(trigger = "axis") %>%
#         e_theme("walden") %>%
#         e_y_axis(name = "Average Count") %>%
#         e_grid(containLabel = TRUE) %>%
#         e_x_axis(name = "Year")
#     })
#     
#     
#     
#     
#   })
#   
#   
#   observeEvent(TRUE, {
#     updateSelectizeInput(session, 
#                          inputId='disease', 
#                          selected = 'stroke',
#                          choices = y
#     )
#   })
  
  
  
}


shinyApp(ui = ui, server = server)
