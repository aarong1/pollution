library(echarts4r)
library(bslib)
library(qs)


render_model_registry <- function(df) {
  
  tags$div(
    style = "width:100%;font-size:80%;", #overflow:scroll;height:100vh;
    tags$div(
      class = "d-flex align-items-center justify-content-center m-5",
      tags$div(
        id    = "model_registry",
        class = "table table-borderless w-100",
        tags$div(
          tagList(
            imap(seq_len(nrow(df)), function(i, idx) {
              
              row <- df[i, , drop = FALSE]
              risk1_vals <- row$risk1[[1]] %||% character(0)
              
              # row_class <- if (i == 1) {
              #   "odd selected"
              # } else if (i %% 2 == 0) {
              #   "even"
              # } else {
              #   "odd"
              # }
              
              tags$div(   
                #   onclick = HTML(
                #   sprintf(
                #     "console.log('row clicked: %s'); Shiny.setInputValue('row_clicked', %s, {priority: 'event'});",
                #     i, i
                #   )
                # ),
                
                onclick = HTML(
                  sprintf(
                    "$('#model_tabs_content').show();$('#model_tabs_content').children().hide();$('#model_%s').show();
                    $('#model_tabs_btm').children().hide();$('#model_btm_%s').show();",
                    i, i
                  )
                ),
                
                
                class = 'row_class',
                # Col 1: person + tags
                tags$div(class = "d-inline",
                         div(
                           class = "p-2 hv d-inline",
                           h5(row$name),
                           div(class = "text-muted d-inline my-5", row$author),
                           p(class = "wdr-ui-element py-3", row$email),
                           
                           tagList(lapply(risk1_vals, function(x) {
                             tags$span(class = "badge rounded-pill text-bg-primary me-1 bg-opacity-50", x)
                           })),
                           
                           span(class = "badge rounded-pill bg-opacity-50 text-bg-warning d-inline p-2", row$risk2),
                           span(class = "badge rounded-pill bg-opacity-50 text-bg-secondary d-inline p-2", row$age_band)
                           
                         )
                ),
                # Col 2: model label + id
                tags$div(class = "d-inline ps-5 rounded-pill text-muted",
                         span(class = "float-right", 
                              # h6(class = "d-inline ","Model"),
                              h5(class = "d-inline",#'{', row$model_id,'}',
                                 span(class = 'fs-5',icon('chevron-right'))),
                         )),
                br(),
                br(),
                # Col 2: model label + id
                tags$div(class = "d-inline my-5 ",
                         
                         # h6(class = "d-inline ","Model"),
                         
                         
                         
                         # span(class = 'fs-5',icon('chevron-right'))
                         
                ),
                
                #p(class = "ps-5", 'ow[ejn;oe; qvon[oq qw[inv[oqengklqnvo[q')
                # Col 3: sparkline
                # tags$td(
                #   sparkline::sparkline(
                #     unlist(row$spark_vals),
                #     width  = 60,
                #     height = 20,
                #     lineColor = "green"
                #   )
                # )
              )
            })
          )
        )
      )
    )
  )
};htmltools::browsable(bslib::page_fluid(render_model_registry(models)))



clear <- function(){
  HTML('<div style="/* top:12vh; */height: 4vh;width: 32vw;z-index:1000" class="bottom-0 d-flex display-absolute flex-column glass-card ms-2 p-3 position-absolute">
  
  
  <div class="nav-section">
  
  
  
  </div>
  </div>')}

# models <- tibble::tibble(
#   name      = c("Margaret Arellano", "Charles Robinson", "Michael Nguyen",
#                 "Robert Boyer", "Jade Curry", "Gregory Wilkins",
#                 "Carla Fernandez", "Daniel Chavez", "Mary Thomas", "Erin Smith"),
#   email     = c("richard.stewart@hscni.net", "tyler.bright@hscni.net",
#                 "deanna.norris@hscni.net", "michelle.bradley@hscni.net",
#                 "dana.hernandez@hscni.net", "chelsea.reed@hscni.net",
#                 "emily.freeman@hscni.net", "alexis.day@hscni.net",
#                 "david.chen@hscni.net", "maria.bradley@hscni.net"
#                 ),
#   risk1     = "PM2.5",
#   # risk2     = "cholesterol",
#   age_band  = 'everybody', ##c("40-49","60-69","30-39","60-69","60-69",
#               #  "20-29","30-39","40-49","60-69","80-89"),
#   model_id  = 1:3,#10,
#   spark_vals = list(
#     1:10,1:10,1:10#,1:10,1:10,1:10,1:10,1:10,1:10,1:10
#   )
# )



exp_model <- tibble::tibble(
  name      = c("Name"),
  email     = c(
    "Email"),


  author = c('aaron.gorman@hscni.net'),

  risk1     = list(c("Targeted Risk factor",' Usually Primary Prevention')),
  risk2     = list(c("Who to intervene on",'The risk factor tackled might not, might ot only include the cohort targeted')),
  age_band  = c("Demographics"),
  # model_id  = 1,#1:10,
  spark_vals = list(
    1:10#,1:10,1:10,1:10,1:10,1:10,1:10,1:10,1:10,1:10
  )
) %>% mutate(model_id = row_number())


models <- tibble::tibble(
  name = c('20%',
           '5 microns per cubic meter',
           'Deprivation and transport',
           'Urban',
           'Established Risk',
           'Cleaner Primary Economic Sector'#,
           # 'Targeting Deprived areas without access to green spaces',
           # 'Targeting those who 2 of 3: Smoke, Drink and Obese',
           # 'Whole Lifestyle Improvement',
           # 'Broad Obesity Intervention',
           # 'CVD Risk Stratification',
           # 'Cancer Risk Stratification',
           # 'Other Modifiable Risk',
           # 'Tirzepatide Prescribing Expansion',
           # 'Bariatric Surgery Expansion',
           # 'Community Weight Management Expansion',
           # 'Diet and excercise',
           # 'Public Campaign'
                ),
  email = c(
  "Reduced pollution uniformly by 20%",
  "Reduced pollution to same baseline - 5 microns per meter cubed",
  'Target reduce pollution in deprived areas beside major thorughfares',
  'Reduce the urban areas pollution to that of rural neighbours',
  'Reduce pollution where there is already higher rates of cardiovascular and respiratory morbidity',
  'Reduce the manufacturing sources of Pollution to 0%'#,#'','','','',
  
  # 'This targets Obesity and Overweight people\'s weight on the basis of their more serious physiological risk profile, 
  #         with consideration of comorbidities of the serious kind taken into account, Atrial Fibrillation, Hypertension, T2DM, CKD etc.',
  # 
  # 'This targets Inner city deprived areas of overweight and obese people\'s weight without risk stratification or other prioritisation.',
  # 'Targets other modifiable risk factors in addition to obesity, such as smoking cessation, alcohol reduction, increased physical activity',
  # 'Expands prescribing of Tirzepatide to all eligible patients with BMI over 30, regardless of comorbidity status',
  # 'Expands access to bariatric surgery to all eligible patients with BMI over 35, regardless of comorbidity status',
  # 'Expands access to community weight management programmes to all overweight and obese patients, regardless of comorbidity status',
  # 'Provides diet and exercise advice to all overweight and obese patients, regardless of comorbidity status',
  # 'Public health campaign to raise awareness of obesity risks and promote healthy lifestyles among the general population'
  ),
  
  
  author = c('aaron.gorman@hscni.net'),
  # "deanna.norris@hscni.net", "michelle.bradley@hscni.net",
                # "dana.hernandez@hscni.net", "chelsea.reed@hscni.net",
                # "emily.freeman@hscni.net", "alexis.day@hscni.net",
                # "david.chen@hscni.net", "maria.bradley@hscni.net"),
  risk1     = list(c("Pollution",'PM25'),
                   c("Pollution",'PM25'),
                   c("Pollution",'PM25'),
                   c("Pollution",'PM25'),
                   c("Pollution",'PM25'),
                   c("Pollution",'PM25')#,
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight'),
                   # c("Obesity",'Overweight')
                   ),
                   
  risk2     = c("All class - indiscriminate",
                "All class - indiscriminate",
                "Deprivation",
                "Urban",
                "Existing High CVD Rates",
                "All class - indiscriminate"#,
                # "Existing High CVD Rates",
                # "Existing High CVD Rates",
                # "Existing High CVD Rates",
                # "One Comorbidity",
                # "Multiple Comorbidities",
                # "Urban Areas",
                # "Multiple Risk Factors",
                # "Tirzepatide Eligible",
                # "Bariatric Surgery Eligible",
                # "Community Weight Management Eligible",
                # "Diet and Exercise Advice",
                # "General Population"
                ),
  age_band  = c("All ages - indiscriminate ",
                "All ages - indiscriminate ",
                "All ages - indiscriminate ",
                "All ages - indiscriminate ",
                "All ages - indiscriminate ",
                "All ages - indiscriminate "#,
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+",
                # "18+"
                ),
  # model_id  = 1,#1:10,
  spark_vals = list(
    1:10#,1:10,1:10,1:10,1:10,1:10,1:10,1:10,1:10,1:10
  )
) %>% mutate(model_id = row_number())

render_model_registry_exp<- function(df) {
  tags$div(
    style = "width:100%;font-size:80%;",
    tags$div(
      id    = "model_registry_exp",
      class = "w-100",
      tags$div(
        tagList(
          imap(seq_len(nrow(df)), function(i, idx) {
            row <- df[i, , drop = FALSE]
            risk1_vals <- row$risk1[[1]] %||% character(0)

            tags$div(
              class = 'row_class',
              'data-model-index' = i,

              tags$div(class = "d-inline",
                       div(
                         class = "p-2 hv d-inline",
                         h5(row$name),
                         div(class = "text-muted d-inline my-5", 'The scenario originator appears here'),
                         p(class = "wdr-ui-element", 'The scenario originator appears here'),
                         
                         tagList(lapply(risk1_vals, function(x) {
                           tags$span(class = "badge rounded-pill text-bg-primary me-1", x)
                         })),
                         
                         span(class = "badge rounded-pill text-bg-warning d-inline p-2", 'health status targeted appears here'),
                         span(class = "badge rounded-pill text-bg-secondary d-inline p-2", 'Demographic status targeted appears here')
                       )
              ),
              tags$div(class = "d-inline ps-5 rounded-pill text-muted",
                       span(class = "float-right", 
                            h5(class = "d-inline",
                               span(class = 'fs-5', 'Click to investigate', icon('chevron-right')),
                               span(class = 'fs-5', 'Scroll for more', icon('chevron-down'))
                            )
                       )
              )
            )
          })
        )
      )
    )
  )
}



scenarios_div1 <- function(){
  
  
  (div(
  
  tags$head(
    tags$style(HTML("
      #model_registry div.row_class {
        padding:15px;
        transition: all 0.3s ease;
      }
      #model_registry div.row_class:hover {
        border-radius: 20px;
        box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
        transform: translateX(5px);
      }
    "))
  ),
  
  div( #class = 'vh-100',
    
    div(class = 'd-flex justify-content-evenly align-center',
        div(id= 'left',
            h2('Intervention Scenarios and Interventions'),
            render_model_registry(models)
        ))),
  
  #div(style = 'padding:100px;',risk_factor_carousel())
)
)

}

htmltools::browsable(bslib::page_fluid(scenarios_div1()))

# Enhanced model registry with data attributes for tab navigation
render_model_registry_enhanced <- function(df) {
  
  tags$div(
    tags$head(
      tags$style('
      #model_registry div.row_class {
        padding:10px;
        margin:10px;
        border-radius: 20px;
        border-left: 4px solid #849A81;
        padding:15px;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
       #model_registry{ 
       padding:10px;
       }
      
      #model_registry div.row_class:hover {
        border-radius: 20px;
        box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
        transform: translate(5px,5px);
      }
      
      #model_registry div.row_class.active {
        /*background-color: rgba(13, 181, 203, 0.1);*/
        border-left: 4px solid #b1e3c6;
      }')),
      
  tags$div(
    style = "width:100%;font-size:80%;",
    tags$div(
      id    = "model_registry",
      class = "w-100",
      tags$div(
        tagList(
          imap(seq_len(nrow(df)), function(i, idx) {
            row <- df[i, , drop = FALSE]
            risk1_vals <- row$risk1[[1]] %||% character(0)

            tags$div(
              class = 'row_class',
              'data-model-index' = i,
              onclick = HTML(
                sprintf("
              $('#model_tabs_content').show();
              
              $('#model_tabs_content')
              .children()
              .removeClass('show')
              .addClass('fade');
              
              $('#model_%s')
              .addClass('show');
              
              $('#model_tabs_btm')
              .children()
              .removeClass('show')
              .addClass('fade');
              
              $('#model_btm_%s')
              .addClass('show');",
                        i, i)
                ),
              # onclick = HTML(
              #   sprintf(
              #     "$('#model_tabs_content').show();$('#model_tabs_content').children().hide();$('#model_%s').show();
              #       $('#model_tabs_btm').children().hide();$('#model_btm_%s').show();",
              #     i, i
              #   )
              # ),
              
              tags$div(class = "d-inline",
                       div(
                         class = "p-2 hv d-inline",
                         h5(row$name),
                         div(class = "text-muted d-inline my-5", row$author),
                         p(class = "wdr-ui-element", row$email),
                         
                         tagList(lapply(risk1_vals, function(x) {
                           tags$span(class = "bg-opacity-75 badge rounded-pill text-bg-primary me-1", x)
                         })),
                         
                         span(class = "bg-opacity-75 badge rounded-pill text-bg-warning d-inline p-2", row$risk2),
                         span(class = "bg-opacity-75 badge rounded-pill text-bg-secondary d-inline p-2", row$age_band)
                       )
              ),
              
              tags$div(class = "d-inline ps-5 rounded-pill text-muted",
                       span(class = "float-right", 
                            h5(class = "d-inline",
                               span(class = 'fs-5', icon('chevron-right'))
                            )
                       )
              )
            )
          })
        )
      )
    )
  )
  )
}




scenarios_div <- function(){
  
  div(
    style = "padding-left: 0%;width:80vw;",
    h3("Scenarios Overview"),
    
    tags$head(
      tags$style(HTML("

      .fade {
        opacity: 0;
        visibility: hidden;
        transition: opacity 250ms ease-in-out, visibility 0s linear 250ms;
      }
      
      .fade.show {
        opacity: 1;
        visibility: visible;
        transition: opacity 250ms ease-in-out;
      }

      #model_registry div.row_class {
        padding:10px;
        margin:10px;
        border-radius: 20px;
        border-left: 4px solid #000000;
        padding:15px;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
       #model_registry{ 
       padding:10px;
       }
      
      #model_registry div.row_class:hover {
        border-radius: 20px;
        box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
        transform: translate(5px,5px);
      }
      
      #model_registry div.row_class.active {
        /*background-color: rgba(13, 181, 203, 0.1);*/
        border-left: 4px solid #13b5cb;
      }
      
      /* Hide tab navigation headers */
      .nav-tabs {
        display: none !important;
      }
      
      /* Table container height */
      #table_container {
        max-height: 80vh;
        overflow-y: auto;
      }
      
      /* Extended tab styling */
      #extended_tab_container {
        height: calc(20vh - 60px);
        overflow-y: auto;
        border-top: 2px solid #e0e0e0;
        padding-top: 20px;
      }
      
      .test-content {
        padding: 20px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 10px;
        text-align: center;
        font-size: 18px;
        font-weight: bold;
      }
    ")),
      
      tags$script(HTML("
        $(document).on('click', '#model_registry .row_class', function() {
          var modelIndex = $(this).data('model-index');
          $('#model_registry .row_class').removeClass('active');
          $(this).addClass('active');
          console.log('run');
          console.log( $('#model_tabs_content a[href=\"#model_' + modelIndex + '\"]'));

          // Switch to the corresponding tab
          $('#model_tabs a[href=\"#model_' + modelIndex + '\"]').tab('show');
        });
        
        // Activate first model on load
        $(document).ready(function() {
          setTimeout(function() {
          console.log('first');
            $('#model_registry .row_class:first').addClass('active');
            $('#model_tabs a:first').tab('show');
          }, 100);
        });
        
         setTimeout(function() {
            $('#model_registry .row_class:first').addClass('active');
            $('#model_tabs a:first').tab('show');
          }, 1000);
          
      "))
    ),
    div(
    div(class = 'd-flex justify-content-around align-items-start gap-3 vh-75',
        # Left side: Model Registry Table
        div(id= 'left', style='flex: 0 0 40%;',
            div(id='table_container',
                sm_hatched_subtitle('Intervention Scenarios and Interventions'),
                render_model_registry_enhanced(models)
            )
        ),
        
        # Right side: Tabbed content area
        div(id = 'right', style='flex: 1;',
            # Summary section with tabs
            div(id='summary_section',
                sm_hatched_subtitle('Intervention Outputs'),
                
                # Hidden tab navigation
                # tags$ul(class='nav nav-hidden nav-tabs tab-pane active show', id='model_tabs', role='tablist',
                #         lapply(1:nrow(models), function(i) {
                #           tags$li(class='nav-item', role='presentation',
                #                   tags$a(class=paste0('nav-link', if(i==1) ' active' else ''),
                #                          id=paste0('model-', i, '-tab'),
                #                          href=paste0('#model_', i),
                #                          role='tab',
                #                          'data-bs-toggle'='tab',
                #                          paste('Model', i)
                #                   )
                #           )
                #         })
                # ),
                # div(HTML('<nav class="navbar navbar-default navbar-inverse navbar-static-top visually-hidden" role="navigation" style="background-color:#0062CC !important;" data-bs-theme="dark">
                #   <div class="container-fluid">
                #   <div class="navbar-header">
                #   <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar-collapse-2806" data-bs-toggle="collapse" data-bs-target="#navbar-collapse-2806">
                #   <span class="sr-only visually-hidden">Toggle navigation</span>
                #   <span class="icon-bar"></span>
                #   <span class="icon-bar"></span>
                #   <span class="icon-bar"></span>
                #   </button>
                #   <span class="navbar-brand">My App</span>
                #   </div>
                #   <div class="navbar-collapse collapse" id="navbar-collapse-2806">
                #   <ul class="nav navbar-nav nav-underline shiny-tab-input" id="nav-model-top" data-tabsetid="1376">
                #   <li class="active">
                #   <a href="#tab-1376-1" data-toggle="tab" data-bs-toggle="tab" data-value="One">One</a>
                #   </li>
                #   <li>
                #   <a href="#tab-1376-2" data-toggle="tab" data-bs-toggle="tab" data-value="Two">Two</a>
                #   </li>
                #   <li>
                #   <a href="#tab-1376-3" data-toggle="tab" data-bs-toggle="tab" data-value="Three">Three</a>
                #   </li>
                #   </ul>
                #   </div>
                #   </div>
                #   </nav>')),
                # page_navbar(id = 'nav-model-top', 
                #             #style='visibility:hidden;',
                #   title = "My App",
                #   navbar_options = navbar_options(
                #     bg = "#0062cc",
                #     underline = TRUE
                #   ),
                #   nav_panel(title = "One", p("First tab content.")),
                #   nav_panel(title = "Two", p("Second tab content.")),
                #   nav_panel(title = "Three", p("Third tab content"))
                # ),
                # Tab content
                div(class='tab-content', id='model_tabs_content',# tab-pane active show
                    
                      div(class='tab-pane fade show active' ,
                          id='model_1',
                          role='tabpanel',
                          # Summary tab content - existing widgets
                          div(style = 'width:100%; padding:20px;',
                              div(class ='d-flex align-center gap-2 flex-row justify-content-center flex-wrap',
                                  div(style = 'height:350px;width:500px;padding:5px;',
                                      plot
                                  ), 
                                  div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                      div(style = 'width:70%', circular_value('10,212')),
                                      div(style='height:20px;width:100%;margin-block:20px;',
                                          rag_line()
                                      )
                                  ),
                                  
                                  div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                      div(p('Bed Days'),
                                          div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                              h3('20k'),
                                              h4('Baseline')
                                              ),
                                          div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                              h3('18k'),
                                              h4('Intervention')
                                          ),
                                          
div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                      div(p('Bed Days'),
                                          div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                              h3('704k'),
                                              h4('Baseline')
                                              ),
                                          div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                              h3('684k'),
                                              h4('Intervention')
                                          ))),
div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
    div(p('CMMS'),
        div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
            h3('33.4'),
            h4('Baseline')
        ),
        div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
            h3('33.2'),
            h4('Intervention')
        )))
                                      )
                                  )
                              )
                          )
                      ),
                    
                    div(class='tab-pane fade show',
                        id='model_2',
                        role='tabpanel',
                        # Summary tab content - existing widgets
                        div(style = 'width:100%; padding:20px;',
                            div(class ='d-flex align-center gap-2 flex-row justify-content-center',
                                div(style = 'height:350px;width:500px;padding:5px;',
                                    plot
                                ), 
                                div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                    div(style = 'width:70%', circular_value('50,212')),
                                    div(style='height:20px;width:100%;margin-block:20px;',
                                        rag_line()
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
    
    # navset_hidden(id = 'nav-model-btm',
    #             # title = "My App",
    #             # navbar_options = navbar_options(
    #             #   bg = "#0062cc",
    #             #   underline = TRUE
    #             # ),
    #             nav_panel(title = "One", p("First tab content.")),
    #             nav_panel(title = "Two", p("Second tab content.")),
    #             nav_panel(title = "Three", p("Third tab content"))
    # ),
    # Extended tab section (bottom)
        sm_hatched_subtitle('Extended Details'),
div(class='tab-content tab-pane active show', 
    id='model_tabs_btm',
    
    div(
        class = 'tab-pane fade show active', 
        id = 'model_btm_1',
        role = 'tabpanel',
        div(class='test-content',
            HTML(paste0(
              '<i class="fa fa-check-circle" style="font-size: 48px; margin-bottom: 10px;"></i><br/>',
              'Extended Tab Rendering Successfully<br/>',
              '<span style="font-size: 14px; opacity: 0.8;">This area is ready for additional content</span>'
            ))
        )
     ) ,

    
    div(class='tab-pane fade show' ,
        id='model_btm_2',
        role='tabpanel',
        # Summary tab content - existing widgets
        div(style = 'width:100%; padding:20px;',
            div(class ='d-flex align-center gap-2 flex-row justify-content-center',
                div(style = 'height:350px;width:500px;padding:5px;',
                    'h'
                ), 
                div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                    div(style = 'width:70%', circular_value('50,212')),
                    div(style='height:20px;width:100%;margin-block:20px;',
                        'g'
                        
                    )
                )
            )
        )
    )

    ) 
)
  
}

# scenarios_div()

# render_model_registry <- function(df) {
# 
#   tags$div(
#     style = "width:100%; font-size:80%;",
#     tags$div(
#       class = "d-flex align-items-center justify-content-center m-5",
#       tags$div(
#         id    = "model_registry",
#         class = "w-100",
#         tagList(
#           imap(seq_len(nrow(df)), function(i, idx) {
#             row <- df[i, , drop = FALSE]
# 
#             row_class <- if (i %% 2 == 0) "model-row even" else "model-row odd"
# 
#             risk1_vals <- row$risk1[[1]] %||% character(0)
# 
#             tags$div(
#               class = row_class,
#               onclick = HTML(sprintf(
#                 "console.log('row clicked: %s'); Shiny.setInputValue('row_clicked', %s, {priority: 'event'});",
#                 i, i
#               )),
#               style = "cursor:pointer; padding:12px; border-radius:12px; margin-bottom:10px;",
#               tags$div(
#                 class = "d-flex align-items-start justify-content-between gap-4",
# 
#                 # Left: person + tags
#                 tags$div(
#                   class = "flex-grow-1",
#                   tags$h5(class = "mb-1", row$name),
#                   tags$p(class = "mb-2 text-muted", row$email),
# 
#                   tagList(lapply(risk1_vals, function(x) {
#                     tags$span(class = "badge rounded-pill text-bg-primary me-1", x)
#                   })),
# 
#                   tags$span(class = "badge rounded-pill text-bg-warning ms-1", row$risk2),
#                   tags$span(class = "badge rounded-pill text-bg-secondary ms-1", row$age_band)
#                 ),
# 
#                 # Right: model label + id
#                 tags$div(
#                   class = "text-end",
#                   tags$div(
#                     tags$span(class = "me-2 fw-semibold", "Model"),
#                     tags$span(class = "fw-bold", row$model_id)
#                   ),
#                   tags$p(class = "mt-2 mb-0 text-muted", "…")
#                 )
#               )
#             )
#           })
#         )
#       )
#     ),
#     # quick styling (optional)
#     tags$style(HTML("
#       .model-row { background: rgba(255,255,255,0.6); border: 1px solid rgba(0,0,0,0.06); }
#       .model-row:hover { filter: brightness(0.98); }
#       .model-row.even { background: rgba(250,250,250,0.8); }
#     "))
#   )
# }
# $('#model_tabs_content_1').show();$('#model_tabs_content_1').children().hide();$('#model_2').show();
# $('#model_tabs_btm_1').children().hide();$('#model_btm_2').show();

# model_tabs_btm_1
# model_btm_2

# htmltools::browsable(bslib::page_fluid(render_model_registry(models)))
# htmltools::browsable(bslib::page_fluid(scenarios_div()))



scenarios_div <- function(){
  
  div(
    style = "padding-left: 0%;width:80vw;",
    # h3("Scenarios Overview"),
    
    tags$head(
      tags$style(HTML("

      .fade {
        opacity: 0;
        visibility: hidden;
        transition: opacity 250ms ease-in-out, visibility 0s linear 250ms;
      }
      
      .fade.show {
        opacity: 1;
        visibility: visible;
        transition: opacity 250ms ease-in-out;
      }

      #model_registry div.row_class {
        padding:10px;
        margin:10px;
        border-radius: 20px;
        border-left: 4px solid #000000;
        padding:15px;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
       #model_registry{ 
       padding:10px;
       }
      
      #model_registry div.row_class:hover {
        border-radius: 20px;
        box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
        transform: translate(5px,5px);
      }
      
      #model_registry div.row_class.active {
        /*background-color: rgba(13, 181, 203, 0.1);*/
        border-left: 4px solid rgba(177, 227, 199, 1);
      }
      
      /* Hide tab navigation headers */
      .nav-tabs {
        display: none !important;
      }
      
      /* Table container height */
      #table_container {
        max-height: 80vh;
        overflow-y: auto;
      }
      
      /* Extended tab styling */
      #extended_tab_container {
        height: calc(20vh - 60px);
        overflow-y: auto;
        border-top: 2px solid #e0e0e0;
        padding-top: 20px;
      }
      
      .test-content {
        padding: 20px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 10px;
        text-align: center;
        font-size: 18px;
        font-weight: bold;
      }
    ")),
      
      tags$script(HTML("
        $(document).on('click', '#model_registry .row_class', function() {
          var modelIndex = $(this).data('model-index');
          $('#model_registry .row_class').removeClass('active');
          $(this).addClass('active');
          console.log('run');
          console.log( $('#model_tabs_content a[href=\"#model_' + modelIndex + '\"]'));

          // Switch to the corresponding tab
          $('#model_tabs a[href=\"#model_' + modelIndex + '\"]').tab('show');
        });
        
        // Activate first model on load
        $(document).ready(function() {
          setTimeout(function() {
          console.log('first');
            $('#model_registry .row_class:first').addClass('active');
            $('#model_tabs a:first').tab('show');
          }, 100);
        });
        
         setTimeout(function() {
            $('#model_registry .row_class:first').addClass('active');
            $('#model_tabs a:first').tab('show');
          }, 1000);
          
      "))
    ),
    div(
      div(class = 'd-flex justify-content-around align-items-start gap-3 vh-75',
          # Left side: Model Registry Table
          div(id= 'left', style='flex: 0 0 40%;',
              div(id='table_container',
                  # h2('Intervention Scenarios and Interventions'),
                  render_model_registry_enhanced(models)
              )
          ))))
  }
                  

                  
                  
                  

htmltools::browsable(bslib::page_fluid(scenarios_div()))
  

