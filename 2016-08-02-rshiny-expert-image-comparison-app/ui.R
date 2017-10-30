# Quantitative cell micromechanics in Arabidopsis
# Publication: http://onlinelibrary.wiley.com/doi/10.1111/tpj.13290/full
#
# Maintainer: sebastienrochettefr@gmail.com
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Source the switch button function
rawWD <- getwd()
source(paste0(rawWD,"/Rsource/functions.R"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "button.css",
  useShinyjs(),
  # Application title
  titlePanel("Image bundling expert detection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.go == 0", # Javascript expression && input.file1.name != null 
        HTML("<p style=\"font-size: 16pt\">
        Hi,<br/>
        You are participating to a scientific experience.<br/>
        This R-shiny app was used in a scientific publication so
        that the authors can test their expertise blindly.<br/>
        Publication associated: <a href=\"http://onlinelibrary.wiley.com/doi/10.1111/tpj.13290/full\">
        Quantitative cell micromechanics in Arabidopsis</a>
        <br/>
        <br/>
        Notice:<br/>
        You will be asked to compare to 2 images on the right side of this window,
        like the ones presented here.<br/>
        The aim is to expertise the two images of plant cells at two different time step.<br/>
        You will be asked whether you identify some bundling on the second image compared to the first one.
        Buttons to answer are in the left panel.<br/>
        If you do not know what bundling is, there will be examples at the bottom of the left panel.<br/>
        You can also have a look at the corresponding publication.<br/>
        <br/>
        To start, just write a pseudo and let's go !<br/>
        Your results are not saved on the server, if you want them to be added to the project results, 
        you will find how to download your results and send them by email to us.
        <i>If you already started an expertise, you'll be able to upload your data.</i><br/>
        Thank you very much !</p>"),
        actionButton(inputId = "go", label = "Go !",
          icon = NULL, width = NULL)
      ),
      # Sidebar Identification
      conditionalPanel(
        condition = "input.go != 0 & input.validExpert == 0", # Javascript expression && input.file1.name != null 
          h2("Expert identification"),
          h3("Have you already started an expertise"),
          switchButton(inputId = "expertYes",
            label = "Yes or No ?", 
            value = FALSE, col = "GB", type = "YN"),
        
            conditionalPanel(
              condition = "input.expertYes == true",
              fileInput(inputId = "Myresults",
                        label = "Please upload your 'username_results.zip' file here")
            ),
        
          h3("Please write your pseudo"),
          helpText("Please use a long pseudo but do not use space or special characters.", style = "font-size:13pt"),
          textInput("expertname", label = "Expert pseudo", value = "mypseudo"),
          h3("Do you want the special GFP-MBD case ?"),
          helpText("The last part of the results of our paper compares stress-induced microtubule response 
                  to tissue growth for only one line (GFP-MBD) over the four lines available. 
                  You can test your observations against ours for this specific case only."),
          helpText("This specific case implies an expertise of 500 couples of cells for a ~30 minutes analysis."),
          helpText("If you already started an expertise, 
                   you will be able to switch between GFP-MBD case and complete case at restart.",
                   style = "font-size:13pt"),
          switchButton(inputId = "MBD",
            label = "Special GFP-MBD case ?", 
            value = FALSE, col = "RG", type = "YN"),
          h3("Please valid your options"),
          helpText("You won't be able to modify your options after validation, 
                   except with a 'Ctrl + F5' to reload the page !"),        
          actionButton(inputId = "validExpert", label = "Validatation",
            icon = NULL, width = NULL)
      ),
      hr(),
      # Sidebar buttons for expertise
      conditionalPanel(
        condition = "input.validExpert != 0", # Javascript expression && input.file1.name != null 
        h2("Expertise"),
        h3("Is there difference in favor of bundling in T2 ?"),
        helpText("Microtubules are green lines on images.
    	    Bundling is when a group of sparse microtubules
    	    aggregate together to form a thick line.
    	    Please have a look at exemples below."),
        radioButtons(inputId = "expert", label = "Your answer",
          choices = c(" YES" = 1, " Sorry, this is not a choice" = 2,
            " NO" = 0), selected = 2),
        h3("Click for next image to expertise"),
          p("Once you click on \'Next\' button,
          you will not be able to modify your expertise",
          style = "font-size: 15pt; color: #428bca"),
          actionButton(inputId = "nextImg", label = "NEXT",
            icon = NULL, width = NULL),
        br(),
        h3("Want to stop ? Click on STOP. Then you will be able to download the results of your expertise"),
        p("If you realised a complete analysis or the special GFP-MBD case entirely, 
          you can send us your results by email to sebastienrochettefr [at] gmail [dot] com"),
        p("This will probably not change our paper, but we will include your results permanently on 
          this Internet page. Thank you for your cooperation."),
        p("You will still be able to continue your expertise later if you download them",
          style = "font-size: 12pt; color: #428bca"),
        actionButton(inputId = "save", label = "STOP",
          icon = NULL, width = NULL),
        # downloadButton(outputId = "download_user", label = "Download Results"),
        uiOutput("downbutton"),
        
        # Example of cells
        hr(),
        h2("Examples of cells"),
        h3("Cells are different in favor of bundling. Answer will be YES",style = "color:#428bca"),
        img(src = "Proj_2015-01-15_N6549xMBD_dis_indent_C_Cell_20.jpg", width = "100%"),
        img(src = "Proj_2015-01-13_MBD_dis_indent_A_Cell_7.jpg", width = "100%"),
        h3("Cells are NOT really different. Answer will be NO", style = "color:#428bca"),
        img(src = "Proj_2015-01-13_MBD_dis_indent_A_Cell_13.jpg", width = "100%"),
        img(src = "Proj_2015-01-13_MBD_dis_indent_B_Cell_13.jpg", width = "100%")
      ), # End of conditional panel
      # Bottom of the sidebar panel
      br(),
      p("Publication associated:",
        a(href = "http://onlinelibrary.wiley.com/doi/10.1111/tpj.13290/full",
        "Quantitative cell micromechanics in Arabidopsis")),     
      p("This shiny interface for expertise has been proposed by",
        a(href = "http://statnmap.com/","StatnMap - Courses and consulting"),
        br(),
      "More details on this analysis on my website : ",
      a(href = "http://statnmap.com/rshiny-expert-image-comparison-app/","http://statnmap.com/"))      
    ),
    
    # Mainpanel
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "input.go == 0",      
        h2("Examples of cells to expertise"),
        h3("Question:"),
        HTML("<p style=\"color:#428bca; font-size: 14pt\">
          Are cells in these two time steps different in favor of bundling?
          <br/></p> <p style=\"font-size: 12pt\"> Here, answer would be YES </p>"),
        img(src = "Proj_2015-01-15_N6549xMBD_dis_indent_C_Cell_20.jpg", width = "60%")
      ),
      conditionalPanel(
        condition = "input.go != 0",
        h2("Messages"),        
        htmlOutput("printmsg")
      ),
      conditionalPanel(
        condition = "input.validExpert != 0", # Javascript expression && input.file1.name != null 
        uiOutput("details"),
        htmlOutput("detailsmsg"),
        uiOutput("imgUI")
     ),
      # Results of the expertises shown when STOP
      uiOutput("comparison")
    )
  )
)
)
