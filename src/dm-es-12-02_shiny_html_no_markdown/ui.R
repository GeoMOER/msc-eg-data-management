# Shiny app to illustrate interactive visualization using shiny
#
# Copyright (C) 2014 Thomas Nauss
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Please send any comments, suggestions, criticism, or (for our sake) bug
# reports to admin@environmentalinformatics-marburg.de

#### General setttings #########################################################
library(shiny)


#### Initialize shiny user interface ###########################################
shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "n_breaks", label = "Number of bins:",
                  choices = c(10, 20, 35, 50), selected = 20),
      
      sliderInput(inputId = "bw_adjust", label = "Bandwidth adjustment:",
                  min = 0.2, max = 2, value = 1, step = 0.2,
                  animate = animationOptions(
                    interval = 1000, loop = TRUE, 
                    playButton = NULL, pauseButton = NULL))
    ),
    mainPanel(plotOutput("histo")
    )
  )
)
)
