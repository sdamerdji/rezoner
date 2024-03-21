library(shiny)
library(shinyjs)
library(shinyBS)
library(mapboxer)
library(shinyWidgets)
library(sortable)
source('./modules.R', local=T)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  #useShinyBS(),
  id = "main_content",
  title = 'Upzone SF!',
  tags$head(
    tags$script(src = "./js-confetti.browser.js"),
    tags$script(src="./progressbar.js"),
    tags$style('.popover{max-width: 100%;}'),
    tags$head(
      tags$link(rel = "shortcut icon", type = "image/png", href = "sfy.png")
    ),
    titlePanel(
      div("Upzone the City",  style={'padding-left: 34%; display: flex; justify-content: space-between; align-items: center;"'},
      popify(a("About",
               target = "_blank",
               style = "margin-right: 20px; font-size:20px"),
             title='hi there', 
             content= HTML(paste0('Thanks so much for toying around with this tool! You should know a bit about how it came to be.',
                            '<br><br>Tasked with tripling annual housing production, the city of SF agreed to use data and a statistical model so ',
                            'its rezoning would put the city on track to succeed with its housing target. ',
                            'The city paid the Blue Sky consulting firm to build such a model, which is the model under the hood of this web app.',
                            '<br><br>This web app faithfully recreates Blue Sky&apos;s logistic regression model for predicting whether parcels are redeveloped (Appendix B.2),',
                            ' SF Planning&apos;s heuristics for estimating the size of projects that do get built (pages 43-45; Appendix B), and the historical ',
                            'adjustment for state density bonus units (page 4; Appendix B.2).',
                            '<br><br>You should know that this model makes a number of key assumptions: that the economic environment looks like 2016, ',
                            'that statistical associations of the past will hold for hypothetical rezonings, ',
                            'and that projects will be built quite close to their maximum feasible capacity.',
                            '<br><br>Rather than using this model&apos;s output as a single source of truth, we&apos;d recommend evaluating a rezoning proposal',
                            ' against multiple models to ensure that the findings are robust to a range of different assumptions.'))))
    ),
    sidebarLayout(
      sidebarPanel(
        h4('Select a rezoning proposal'),
        selectInput("scenario", NULL,
                    choices = c('Select' = 'blank',
                                "Current SF Planning Proposal" = "E",
                                "Fall 2023 SF Planning Proposal" = "D",
                                #"Housing Element Rezoning A" = "A",
                                #"Housing Element Rezoning B" = "B",
                                #"Housing Element Rezoning C" = "C",
                                #"Draft SF YIMBY Plan - No Decontrol in RH" = "yimby1", 
                                #"Draft SF YIMBY Plan - Decontrol in RH" = "yimby2",
                                "The People's Plan" = "yimby3"),
                    selected = 'blank'),

        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        
        tags$head(
          tags$script(HTML("
            $(document).on('shiny:connected', function() {
              $('#stories input[type=\"number\"]').css('background-color', '#f5f5f5');
            });
          "))
        ),

        h4('Or add your own!'),
        tags$style(HTML("
          input#stories {
            background-color: #f7f7f7;
          }
        ")),
        tags$div(style = "display: flex; align-items: center;", # Ensure alignment of text and input box
                 HTML("Upzone to&nbsp;"),
                 numericInput("stories", label = NULL, value = 6, min = 4, max = 25),
                 HTML("&nbsp;stories.")
        ),
        uiOutput("requirements_ui"),
        actionButton("add_requirement", "Specify where"),
        uiOutput("dynamic_delete_button", style = "display: inline-block;"),
        tags$head(
          tags$style(HTML("
            #rezone {
                border: none;
                outline: none;
                color: #fff;
                background: #111;
                cursor: pointer;
                position: relative;
                z-index: 0;
                border-radius: 10px;
            }
            #rezone:before {
                content: '';
                background: linear-gradient(45deg, #ff0000, #ff7300, #fffb00, #48ff00, #00ffd5, #002bff, #7a00ff, #ff00c8, #ff0000);
                position: absolute;
                top: -2px;
                left: -2px;
                background-size: 400%;
                z-index: -1;
                filter: blur(5px);
                width: calc(100% + 4px);
                height: calc(100% + 4px);
                animation: glowing 20s linear infinite;
                opacity: 0;
                transition: opacity .3s ease-in-out;
                border-radius: 10px;
            }
            #rezone:active {
                color: #000;
            }
            #rezone:active:after {
                background: transparent;
            }
            #rezone:hover:before {
                opacity: 1;
            }
            #rezone:after {
                z-index: -1;
                content: '';
                position: absolute;
                width: 100%;
                height: 100%;
                background: #14B8A6;
                left: 0;
                top: 0;
                border-radius: 10px;
            }
            @keyframes glowing {
                0% { background-position: 0 0; }
                50% { background-position: 400% 0; }
                100% { background-position: 0 0; }
            }
            "))
        ),
        tags$style(HTML("
          input#years_slider {
            background-color: #f7f7f7;
          }
          #sortable {
             background-color: #f7f7f7;
          }
        ")),
        actionButton("rezone", "Rezone!", class = "glow-on-hover"),
        
        uiOutput("dynamic_sort1"),  # Placeholder for dynamic content
        uiOutput('all_things_sort'),
          tags$div(
            icon("trash"),
            "Undo rezoning",
            id = "sortable_bin"
          ),
        sortable_js(
          "sort1",
          options = sortable_options(
            group = list(
              pull = TRUE,
              name = "sortGroup1",
              put = FALSE
            ),
            # swapClass = "sortable-swap-highlight",
            onSort = sortable_js_capture_input("sort_vars")
          )
        ),
        
        sortable_js(
          "sortable_bin",
          options = sortable_options(
            group = list(
              group = "sortGroup1",
              put = TRUE,
              pull = TRUE
            ),
            onAdd = htmlwidgets::JS("function (evt) { 
                                    this.el.removeChild(evt.item);
                                    Shiny.setInputValue('item_deleted', true, {priority: 'event'});

                                    }")
          )
        ),
        HTML('<br></br>'),
        tags$div(style = "display: flex; align-items: center; ", # Ensure alignment of text and input box
                 HTML("Project over&nbsp;"),
                 numericInput("years_slider", label = NULL, value = 5, min = 5, max = 10, width = '60px'),
                 HTML("&nbsp;years.")
        ),
        # h4("Overlays:"),
        # 
        # checkboxInput(
        #   "affh",
        #   label = list(
        #     "High opportunity tracts",
        #     span(
        #       id = "affh_info",
        #       class = "info-icon",
        #       HTML(
        #         '<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> High opportunity is defined by 2024 TCAC Map.</a>">&#9432;</span>'
        #       )
        #     )
        #   )),
        # 
        # checkboxInput("peg", label=list("Priority equity geographies",
        #                                 span(
        #                                   id = "peg_info",
        #                                   class = "info-icon",
        #                                   HTML('<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> SF cannot legally upzone the PEG for the housing element rezoning. Note that this PEG SUD is not the final adopted SUD in the CRO, which added parts of North Beach and removed parts of Inner Richmond.</a>">&#9432;</span>'
        #                                   )
        #                                 ))),
        h4('Map this'),
        selectInput("map", NULL, 
                    choices = c("Allowed Heights" = 'heights', 
                                "Missing Potential Yield" = 'potential', 
                                "E[Units]" = 'E[u]'), 
                              #  "Simulate Buildout" = 'sim'),
                    selected = 'heights'),
        conditionalPanel(
          condition = "input.map == 'sim'",
          actionButton("resimulateBtn", "Resimulate")
        ),
        uiOutput("dynamicEmailButton"),
        
        # plotlyOutput("pieChart"),
        position = "bottom-left"
      ),

      mainPanel(
        tags$style(HTML(
          '.map-overlay {
            position: absolute;
            bottom: 0;
            right: 0;
            background: #fff;
            margin-right: 20px;
            font-family: Arial, sans-serif;
            overflow: auto;
            border-radius: 3px;
          }
          
          #features {
            top: 0;
            height: 100px;
            margin-top: 20px;
            width: 250px;
          }
          
          #legend {
            position: absolute;
            bottom: 10px;
            right: 10px;
            z-index: 1000;
            padding: 10px;
            box-shadow: 0 2px 4px rgba(1 1 1 0.1);
            line-height: 18px;

          }
          
          .legend-key {
            display: inline-block;
            border-radius: 20%;
            width: 10px;
            height: 10px;
            margin-right: 5px;
          }'
        )),
        div(style = "position: relative;",
            uiOutput("dynamicLegend"),
            mapboxerOutput("mainPlot", height = "525px"),
        ),
        uiOutput("customHtmlJs"), # Placeholder for custom HTML and JS
        uiOutput("helpText"),
        position = "top-right",
        height = "525px"
      )
    ),
    tags$script(HTML("
      var jsConfettiInstance; // Global confetti instance
      document.addEventListener('DOMContentLoaded', function() {
        jsConfettiInstance = new JSConfetti(); // Instantiate when the document is ready
      });
      
      // Define the sprayConfetti function
      function sprayConfetti() {
        for (var i = 0; i < 2; i++) {
            setTimeout(function() {
                jsConfettiInstance.addConfetti();
            }, 1000 * i); // Delay of 1 second (1000 milliseconds) between each spray
        }
      }
          var originalLeave = $.fn.popover.Constructor.prototype.leave;
      $.fn.popover.Constructor.prototype.leave = function(obj){
        var self = obj instanceof this.constructor ?
          obj : $(obj.currentTarget)[this.type](this.getDelegateOptions()).data('bs.' + this.type)
        var container, timeout;

        originalLeave.call(this, obj);
      
        if(obj.currentTarget) {
          container = $(obj.currentTarget).siblings('.popover')
          timeout = self.timeout;
          container.one('mouseenter', function(){
            //We entered the actual popover – call off the dogs
            clearTimeout(timeout);
            //Let's monitor popover content instead
            container.one('mouseleave', function(){
              $.fn.popover.Constructor.prototype.leave.call(self, self);
            });
          })
        }
      };
      
      
      $('body').popover({ selector: '[data-popover]', trigger: 'click hover', placement: 'auto', delay: {show: 50, hide: 400}});
  
    "))
  )
)
