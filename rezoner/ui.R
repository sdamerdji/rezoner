library(shiny)
library(shinyjs)
library(leaflet)

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
                    choices = c("Current SF Planning Proposal for Housing Element Rezoning" = "D",
                                "Housing Element Rezoning Scenario A" = "A",
                                "Housing Element Rezoning Scenario B" = "B",
                                "Housing Element Rezoning Scenario C" = "C",
                                "Take the boldest elements of scenarios A, B, C, and the current proposal" = "Union", 
                                "Parisian zoning in low density neighborhoods" = "Parisian"),
                    selected = 'D'),
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
        position = "bottom-left"
      ),

      mainPanel(
        leafletOutput("mainPlot", height = "600px"),
        span(verbatimTextOutput("helpText"), style = "color:red; font-size:20px"),
        span(verbatimTextOutput("supervisors"), style = "color:red; font-size:20px"),
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
