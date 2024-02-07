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
        h4('Upzoning strategies:'),
        selectInput("scenario", NULL,
                    choices = c("Current SF Planning Proposal" = "E",
                                "Fall 2023 SF Planning Proposal" = "D",
                                "Housing Element Rezoning A" = "A",
                                "Housing Element Rezoning B" = "B",
                                "Housing Element Rezoning C" = "C",
                                "Take the boldest elements of A, B, C, Fall, & current proposal" = "Union", 
                                "Parisian zoning in low density neighborhoods" = "Parisian"),
                    selected = 'E'),

        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

        tags$head(
          tags$script(HTML("
            $(document).on('shiny:connected', function() {
              $('#stories input[type=\"number\"]').css('background-color', '#f5f5f5');
            });
          "))
        ),

        h4('Define a custom upzoning:'),
        tags$style(HTML("
          input#stories {
            background-color: #f7f7f7;
          }
        ")),
        tags$div(style = "display: flex; align-items: center;", # Ensure alignment of text and input box
                 HTML("Upzone to&nbsp;"),
                 numericInput("stories", label = NULL, value = 10, min = 4, max = 25),
                 HTML("&nbsp;stories.")
        ),
        uiOutput("requirements_ui"),
        actionButton("add_requirement", "Specify where"),

        actionButton("delete_requirement", "Remove last"),
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
        tags$style(HTML("
          input#years_slider {
            background-color: #f7f7f7;
          }
        ")),
        actionButton("rezone", "Rezone!", class = "glow-on-hover"),
        HTML('<br></br>'),
        tags$div(style = "display: flex; align-items: center; ", # Ensure alignment of text and input box
                 HTML("Project over&nbsp;"),
                 numericInput("years_slider", label = NULL, value = 5, min = 5, max = 10, width = '60px'),
                 HTML("&nbsp;years.")
        ),
        h4("Overlays:"),
        
        checkboxInput(
          "affh",
          label = list(
            "High opportunity tracts",
            span(
              id = "affh_info",
              class = "info-icon",
              HTML(
                '<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> High opportunity is defined by 2024 TCAC Map.</a>">&#9432;</span>'
              )
            )
          )),
        
        checkboxInput("peg", label=list("Priority equity geographies",
                                        span(
                                          id = "peg_info",
                                          class = "info-icon",
                                          HTML('<span data-toggle="tooltip" data-popover="true" data-html=true data-content="<a> SF cannot legally upzone the PEG for the housing element rezoning. Note that this PEG SUD is not the final adopted SUD in the CRO, which added parts of North Beach and removed parts of Inner Richmond.</a>">&#9432;</span>'
                                          )
                                        ))),
        h4('Map this'),
        selectInput("map", NULL, 
                    choices = c("Allowed Heights" = 'heights', 
                                "Missing Potential Yield" = 'potential', 
                                "E[Units]" = 'E[u]', 
                                "Simulate Buildout" = 'sim'),
                    selected = 'heights'),
        conditionalPanel(
          condition = "input.map == 'sim'",
          actionButton("resimulateBtn", "Resimulate")
        ),
        position = "bottom-left"
      ),

      mainPanel(
        leafletOutput("mainPlot", height = "600px"),
        span(verbatimTextOutput("helpText"), style = "color:red; font-size:20px"),
        span(verbatimTextOutput("supervisors"), style = "color:red; font-size:20px"),
        #span(verbatimTextOutput("most_units"), style = "color:red; font-size:20px"),
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
