library(shiny)
library(shinyjs)
library(leaflet)
source('./modules.R', local=T)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  id = "main_content",
  tags$head(
    tags$script(src = "./js-confetti.browser.js"),
    
    tags$head(
      tags$link(rel = "shortcut icon", type = "image/png", href = "sfy.png")
    ),
    titlePanel(
      "Upzone the City"
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("scenario", "Upzoning Strategies:",
                    choices = c("Current SF Planning Proposal" = "E",
                                "Fall 2023 SF Planning Proposal" = "D",
                                "Housing Element Rezoning Scenario A" = "A",
                                "Housing Element Rezoning Scenario B" = "B",
                                "Housing Element Rezoning Scenario C" = "C",
                                "Take the boldest elements of scenarios A, B, C, and the current proposal" = "Union", 
                                "Parisian zoning in low density neighborhoods" = "Parisian"),
                    selected = 'E'),
        # radioButtons("customize_map", "Customize this rezoning:",
        #              choices = c("No" = "no", "Yes" = "yes"),
        #              selected = "no"),
        # conditionalPanel(
        #   condition = "input.customize_map == 'yes'",
        #   radioButtons("stories", "Select number of stories:",
        #                choices = c("5 stories", "8 stories", "12 stories", "20 stories"),
        #                selected = NULL),
        #   actionButton("reset_map", "Reset", icon = icon("sync"))
        # ),
        HTML("<b>Overlay options:</b>"),
        checkboxInput(
          "lldl",
          label = list(
            "Overlay large, low density lots (outside low opportunity tract)",
            span(
              id = "lldl_info",
              class = "info-icon",
              HTML('<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a href=\'http://www.sfyimby.org\' target=\'blank\' >click me, Ill try not to disappear</a>">&#9432;</span>')
            )
          )
        ),
        
        checkboxInput(
          "affh",
          label = list(
            "Overlay high opportunity tracts",
            span(
              id = "affh_info",
              class = "info-icon",
              HTML(
                '<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> High opportunity is defined by Draft 2024 TCAC Map.</a>">&#9432;</span>'
              )
            )
          )),
        
        checkboxInput("peg", label=list("Overlay priority equity geographies",
                                        span(
                                          id = "peg_info",
                                          class = "info-icon",
                                          HTML('<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> Now that this PEG SUD is not the final adopted SUD in the CRO, which added parts of North Beach and removed parts of Inner Richmond.</a>">&#9432;</span>'
                                          )
                                        ))),
        selectInput('extend', 'Extend rezoning:', choices = 
                      c("Extend rezoning to high opportunity areas" = "extend_affh",
                        "Extend rezoning to anywhere that's not a PEG" = "extend_except_peg",
                        "Extend rezoning to rest of city" = "extend_errwhere",
                        "Extend rezoning to areas with high economic opportunity" = "extend_econ",
                        "Add Parisian zoning for non-rezoned, non-PEG areas" = "extend_broockman1",
                        "Add Parisian zoning on 2500+ sq ft lots in non-rezoned, non-PEG areas" = "extend_broockman2",
                        "Add Parisian zoning on 5000+ sq ft lots in non-rezoned, non-PEG areas" = "extend_broockman3",
                        "Add Parisian zoning on 5000+ sq ft, non-MFR lots in non-rezoned, non-PEG areas" = "extend_broockman4",
                      
                        'Select an option.' = 'none'),
                    selected = 'none'),
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        sliderInput("years_slider", 
                    "Project housing production over the next number of years:", 
                    min = 5, 
                    max = 10, 
                    value = 5,
                    step = 1),
        selectInput("map", "Select Mapping", 
                    choices = c("Allowed Heights" = 'heights', 
                                "Missing Potential Yield" = 'potential', 
                                "E[Units]" = 'E[u]', 
                                "Simulate Buildout" = 'sim'),
                    selected = 'heights'),
        conditionalPanel(
          condition = "input.map == 'sim'",
          actionButton("resimulateBtn", "Resimulate")
        ),
        h4('Define a custom upzoning ⤵️️'),
        tags$div(style = "display: flex; align-items: center;", # Ensure alignment of text and input box
                 tags$b("Upzone to "),
                 numericInput("stories", label = NULL, value = 10, min = 4, max = 25),
                 tags$b(" stories.")
        ),
        uiOutput("requirements_ui"),
        actionButton("add_requirement", "Add a requirement"),
        actionButton("delete_requirement", "Delete last requirement"),
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
                background: #111;
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
        actionButton("rezone", "Rezone!", class = "glow-on-hover"),
        position = "bottom-left"
      ),

      mainPanel(
        leafletOutput("mainPlot", height = "600px"),
        span(verbatimTextOutput("helpText"), style = "color:red; font-size:20px"),
        span(verbatimTextOutput("supervisors"), style = "color:red; font-size:20px"),
        span(verbatimTextOutput("most_units"), style = "color:red; font-size:20px"),
        position = "top-right",
        height = "600px"
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
