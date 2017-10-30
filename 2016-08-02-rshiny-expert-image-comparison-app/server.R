# Quantitative cell micromechanics in Arabidopsis
# Publication: http://onlinelibrary.wiley.com/doi/10.1111/tpj.13290/full
#
# Maintainer: sebastienrochettefr@gmail.com
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# TO DO --
# Add a graph for people who answered the questionnaire.

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

rawWD <- getwd()

# Source Global information
source(paste0(rawWD, "/Rsource/Global.R"))
# Source image creation function
source(paste0(rawWD, "/Rsource/functions.R"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Store outputs that may be updated during the process 
  values <- reactiveValues()
  # Temporary save observations of current expert
  values$save.expert <- 
    matrix(NA, nrow = max.Cells, ncol = length(Which.Line))
  values$all.saved <- logical(0)    
  values$userdata.compare <- userdata.compare
  values$rand.cells <- sample(which(!is.na(userdata.compare))) #sample(which(userdata.compare <= 0.67))
  values$expertname <- FALSE
  values$msg.expert <- paste0("<p style=\"color:red ; font-size: 16pt;\">
    Please choose a pseudo and validate it</p>")
  values$details <- c(rep("character(0)",3),-1)
  
  # Verify if expert name exist or not
  # Look if user provides a zip file
  observe({
    # If Previous expertise switched to YES
    if (input$expertYes) {
      # If zip file uploaded
      if (!is.null(input$Myresults$datapath)) {
        # Test content of zip and retrieve pseudo
        # Get expert name
        values$expertname <- strsplit(input$Myresults$name[1],
                                      "_results", fixed = TRUE)[[1]][1]
        
        # Update UI to add retrieved expert name
        updateTextInput(session, inputId = "expertname", label = "Expert pseudo", value = values$expertname)
        
        # Extract zip file
        unzip(input$Myresults$datapath, exdir = outWD_details)
        # Move results file to the correct folder, keep others
        expertfile_zip <- list.files(outWD_details, full.names = TRUE)[
          grep("user",list.files(outWD_details, full.names = FALSE))]
        if (length(expertfile_zip) != 0) {
          file.copy(from = expertfile_zip, to = outWD, overwrite = TRUE)
          file.remove(expertfile_zip)
        } else {
          values$msg.expert <- paste0("<p style=\"color:red ; font-size: 14pt;\">
                Hello ", input$expertname, ", Sorry but the zip file you 
                provided does not contain the correct files</p>
                <br/>You will have to try it again...")        
        }
        # Retrieve previous expertise of this expert
        expertfile <- list.files(outWD, full.names = TRUE)[which(
          list.files(outWD, full.names = FALSE) == paste0("user", values$expertname, ".csv")
        )]
        values$save.expert <- as.matrix(read.csv(file = expertfile,
          header = TRUE, row.names = 1, sep = ","))
        # Get details if available
        if (file.exists(paste0(outWD_details,"details", values$expertname, ".RData"))) {
          load(paste0(outWD_details, "details", values$expertname, ".RData"))
          values$details <- details
        }
        values$msg.expert <- paste0("<p style=\"color:forestgreen ; font-size: 14pt;\">
                Hello ", input$expertname, ", <br/></p>
                <p>Please choose if you want to expertise the special case or
                the complete case.<br/>
                Then press the 'Validation' button.</p>")
      } else {
        res <- NULL
        values$msg.expert <- paste0("<p style=\"color:red ; font-size: 14pt;\">
              Please upload the zip file of your previous expertise.<br/> 
              Otherwise, switch back the expertise button to 'No'.</p>")
      }
    } else {
      # InputExpert No
      if (input$expertname == "" | input$expertname == "mypseudo") {
        values$msg.expert <- "<p style=\"color:red ; font-size: 14pt;\">
             Please enter a pseudo for the analysis or tell if you already started an expertise.</p>"        
      } else {
        expertfile <- list.files(outWD, full.names = FALSE)[which(
          list.files(outWD, full.names = FALSE) == paste0("user", input$expertname, ".csv")
        )]
        if (length(expertfile) != 0) {
          values$expertname <- paste0(input$expertname,
                 formatC(length(expertfile) + 1 , width = 4, format = "d", flag = "0"))
          values$msg.expert <- paste0("<p style=\"color:red ; font-size: 14pt;\">
              This pseudo already exists. <br/>
              If you are the owner, please switch the previous button to 'YES'
              and provide your zip file.<br/>
              Otherwise you will be attributed an other pseudo like '", 
              values$expertname,
              "' when you'll press button 'Validation'.</p><br/>
              <p>If satisfied with this, please choose if you want to expertise the special case or the complete case.
              <br/>Then press the 'Validation' button.<p>")
        } else {
          values$msg.expert <- paste0("<p style=\"color:forestgreen ; font-size: 14pt;\">
              The pseudo ", input$expertname, " will be used to save your results.<br/></p>
              <p>If satisfied, please choose if you want to expertise the special case or the complete case.<br/>
              Then press the 'Validation' button.<p>")
          values$expertname <- input$expertname
        }
      }
    }
  })
    
  # If clicked on validation button
  observeEvent(input$validExpert, {
    isolate({
      values$msg.expert <- NULL
      # Create Special GFP-MBD case. Only 500 cells to expertise.
      # Otherwise, all cells
      samp.MBD <- unique(c(
        # MBD indented
        which(!is.na(values$userdata.compare) &
          col(values$userdata.compare) %in% which(Which.Line == 1 & Which.Indent == 1) & 
          is.na(values$save.expert)),
        # Other cells
        sample(
          which(!is.na(values$userdata.compare) & is.na(values$save.expert)),
          size = 200
        )
      ))
      # Load temp RData vectors of cells to analyse if exist for the user
      if (input$MBD) {# MBD
        if (file.exists(paste0(outWD_details,"tempMBD",values$expertname,".RData"))) {
          load(file = paste0(outWD_details,"tempMBD",values$expertname,".RData"))
          temp <- 
            temp[which(temp %in% which(is.na(values$save.expert)))]
        } else {
          temp <- sample(
            samp.MBD
          )
          save(temp,file = paste0(outWD_details,"tempMBD",values$expertname,".RData"))
        }
      } else {# ALL
        if (file.exists(paste0(outWD_details,"tempALL",values$expertname,".RData"))) {
          load(file = paste0(outWD_details,"tempALL",values$expertname,".RData"))
          temp <- sample(
            temp[which(temp %in% which(is.na(values$save.expert)))]
          )          
        } else {
          temp <- sample(which(!is.na(values$userdata.compare) &
            is.na(values$save.expert)))
          save(temp,file = paste0(outWD_details,"tempALL",values$expertname,".RData"))
        }
      }      
      if (length(temp) == 0) {
        values$rand.cells <- 1
      } else {
        values$rand.cells <- temp
      }      
    })
    
  })
  
  # indented BD left
  observe({
    if (length(values$rand.cells) == 0) {
      values$rand.cells <- 1
    }
  })
  
  # Values to go for next image
  values$i <- 1
  values$nextImgVal <- -1
  values$msg3.1 <- ""
  
  # Which cell of which meristem
  m <- reactive({all.cells[values$rand.cells[values$i],1]})
  cell <- reactive({all.cells[values$rand.cells[values$i],2]})
  
  # Messages for user
  # Test if cell exists
  output$printmsg <- renderText({
    msg1.1 <- msg1.2 <- msg1.3 <- msg1.4 <- msg1.5 <- ""
    if (input$validExpert != 0) {
      if (cell() > Cell.count[m()]) {
        msg1.1 <- paste0("Cell n°",cell(),
          " was chosen but there is no more than ", Cell.count[m()],
          " cells delineated in the meristem ", m())#all.single.names[m()])
      }
      if (values$i == length(values$rand.cells)) {
        msg1.2 <- paste0("<p style = \"font-size:16pt; color:red\"> This is the last cell to expertise. <br/>
          Please click on \"NEXT\" once you have done your expertise. Then \"STOP\" <br/>
          Then you will be able to exit the windows</p>")
      } else {
        if (values$nextImgVal != input$nextImg) { # If not STOP
          msg1.2 <- paste("You are expertising image",values$i, "/", length(values$rand.cells))
        } else {
          msg1.3 <- NULL 
        }
      }
      if (values$msg3.1 != "END") {
        if (input$expert == 2) {
          msg1.3 <- "You made no choice"
        } else {
          msg1.3 <- paste("You say there is", c("no", "")[1 + as.numeric(input$expert)], "bundling here")
        }
      }
      # if (values$expertname != FALSE & 
      if (values$nextImgVal == input$nextImg) { # if STOP
        msg1.4 <- paste("<p style = \"font-size:12pt; color:#428bca\"> YOU HAVE CHOSEN TO STOP THE ANALYSIS.",
          "Thank you for your participation.",
          "You can continue to work on this session (click on 'NEXT') or save your results.</p>",
          sep = '<br/>')
        msg1.5 <- paste0("<p style = \"font-size:12pt; color:red\"> To come back later
          and finish the analysis, you can download your results with the button on the left.
          <br/>If you think you provided enough expertise, we would be happy to include your results
          in this shinyapp. To do so, you will need to send us your downloaded results.
          <br/>Otherwise, what you did will be reset on next session and lost forever...
          <br/>Kind regards."
        )}
    }
    if (values$nextImgVal != input$nextImg) {# Not STOP
      HTML(paste(values$msg.expert, msg1.1, msg1.2, msg1.3, sep = '<br/>'))
    } else {# STOP
      HTML(paste(msg1.1, msg1.4, msg1.5, sep = '<br/>'))
    }
  })
  
  # Show images to compare
  output$cellPlot <- renderImage({
      filename <- paste0(all.single.names[m()],"_Cell_",cell(),".jpg")
    list(src = filename,
      width = 800)
  }, deleteFile = FALSE)
  
  output$imgUI <- renderUI(
    if (values$nextImgVal != input$nextImg) {
      list(
        htmlOutput("choosemsg"),        
        h2("Images to compare"),
        h3("Is there a difference between both cells ?"),
        h3("If yes, does this difference shows more bundling in T2 = your answer"),
        plotOutput("cellPlot"),
        br(),
        h3("For any problem, remind that your previous image expertised was:"),
        htmlOutput("prevcell")        
        )
    }
  )
  
  # Check if 'next image' has been clicked
  observeEvent(input$nextImg,{
    isolate({
        if (input$expert == 2) {
          if ((values$i + 1) <= length(values$rand.cells)) {
            values$msg3.1 <- "<p style = \"color:#428bca; font-size:12pt\">
            You did not choose any answer </br> You have to, even if it is difficult !
            </p>"
          } else {
            values$msg3.1 <- "END"
          }
        } else {
          values$msg3.1 <- ""
          values$save.expert[cell(),m()] <- as.numeric(input$expert) * 1
          values$all.saved <- cbind(m(),cell())
          if ((values$i + 1) <= length(values$rand.cells)) {
            values$i <- values$i + 1
          } else {
            values$msg3.1 <- "END"
          }
          # Save table of results
          write.csv(values$save.expert,
            file = paste0(outWD, "user", input$expertname, ".csv"),
            row.names = TRUE)
          # Reset radio button to force for an expertise
          reset("expert")
        }
      output$choosemsg <- renderText({HTML(values$msg3.1)})
    })
  }) # end of next
  
  # Previous cell message
  output$prevcell <- renderText({
    if (values$msg3.1 != "END") {
      if (length(values$all.saved) == 0) {
        msg2.1 <- c("You did not expertised any cell",
          "Please choose between Yes or No with the Switch button,
        and click on the \"Valid\" button")
      } else {
        msg2.1 <- apply(values$all.saved, 1, function(x)
          paste("Meristem", x[1],
            "- Cell n°", x[2], sep = " "))
      }
      HTML(paste(msg2.1, collapse = '<br/>'))
    }
  })
  
  # Save results and combine to other experts
  values$userdata <- userdata
  values$userdata.compare <- userdata.compare
  values$userfiles <- userfiles
  
  observeEvent(input$save, {
    isolate({
        # Save table of results
        write.csv(values$save.expert,
          file = paste0(outWD,"user",input$expertname,".csv"),
          row.names = TRUE)
        
        # Gather details
        if (file.exists(paste0(outWD_details,"details",values$expertname,".RData"))) {
          load(file = paste0(outWD_details,"details",values$expertname,".RData"))
          values$details <- details
        }
        output$details <- renderUI(
          if (values$nextImgVal == input$nextImg) {
            list(
              h3("Just before leaving, would you please tell us a little bit more about you ?"),
              radioButtons(inputId = "scientist", label = "Are you a scientist ?",
                choices = c(" YES" = 1, " NO" = 0), selected = values$details[1], inline = TRUE),
              radioButtons(inputId = "plant", label = "Are you a Plant scientist ?",
                choices = c(" YES" = 1, " NO" = 0), selected = values$details[2], inline = TRUE),
              radioButtons(inputId = "bundle", label = "Did you know about bundling ?",
                choices = c(" YES" = 1, " NO" = 0), selected = values$details[3], inline = TRUE),
              sliderInput(inputId = "years",
                label = "How many years of expertise have you in Plant cell biology ?",
                value = values$details[4], min = -1, max = 50, width = "100%"),
              actionButton(inputId = "ValidDetails", label = "Valid",
                icon = NULL, width = NULL)
            )
          }
        )
        
        output$downbutton <- renderUI(
          if (values$nextImgVal == input$nextImg) {
            downloadButton(outputId = "download_user", label = "Download Results")
          }
        )
        
        # Number of the current expert by alphabetical order
        expert.n <- which(
          list.files(outWD, full.names = FALSE) == paste0("user",input$expertname,".csv")
        )
        # Update expert response
        if (length(grep("user", list.files(outWD))) > dim(values$userdata)[3]) {
          # New user & combine observations
          values$userfiles <- list.files(outWD,full.names = TRUE)[grep("user", list.files(outWD))]
          values$userdata <- array(dim = c(max.Cells, length(Which.Line), length(values$userfiles)))
          for (i in 1:(length(values$userfiles))) {
            values$userdata[,,i] <- as.matrix(read.csv(file = values$userfiles[i],
              header = TRUE, row.names = 1, sep = ","))
          }
        } else {
          # Update user observations
          values$userdata[,, expert.n] <- values$save.expert
        }
        # Calculate ratios             
        values$userdata.compare <- t(apply(values$userdata, 1, function(x) apply(x, 1, 
          function(y) sum(y == 1, na.rm = TRUE) / sum(!is.na(y)))))
        # Rescale to 0.5 - 1 common reponses
        values$userdata.compare[(values$userdata.compare < 0.5 & !is.na(values$userdata.compare))] <- 
          1 - values$userdata.compare[(values$userdata.compare < 0.5 & !is.na(values$userdata.compare))]
        
        # Plot percentage of agreement between experts with ggplot2
        output$distPlot <- renderPlot({
          res.all <- tapply(userdata.compare, rep(Which.Line, each = nrow(userdata.compare)),
            function(x) table(cut(x, breaks = seq(0.5, 1.1, 0.1), right = FALSE, 
              labels = c("50-60%","60-70%","70-80%","80-90%","90-99%","100%"))))
          # Transform list as a matrix
          res.m <- Reduce(rbind, res.all)
          # Transform as percentage
          res.m <- t(apply(res.m, 1, function(x) x / sum(x, na.rm = TRUE)))
          rownames(res.m) <- c("GFP-MBD","spr2-2 GFP-MBD","bot1-7 GFP-MBD","TUA6-GFP")          
          # Transform as FMA system for ggplot2
          res.l <- as.data.frame.table(res.m)
          names(res.l) <- c("lines","Pc.agree","Freq")
         
          g <- ggplot(res.l) + aes(Pc.agree, Freq, fill = lines) +
            geom_bar(stat = "identity") +
            facet_wrap(~lines) +
            theme_linedraw() + 
            theme(legend.position = "none",
                  strip.text = element_text(size = 25),
                  axis.text = element_text(size = 13)
                  )
          
          print(g)
        }, width = 800, height = 800)
        
        # Plot percentage of bundling per expert, per meristem
        # with classic plot
        output$distPlotMBD <- renderPlot({
          # Clean userdata table in a long form
          res.all <- as.data.frame.table(values$userdata)
          names(res.all) <- c("Cell","Meristem","User","Expertise")
          res.all <- mutate(res.all,
              Cell = as.numeric(Cell),
              Meristem = as.numeric(Meristem),
              Cell.out = paste(Meristem, Cell, sep = "-") %in% paste(Cell.out[,1], Cell.out[,2], sep = "-")
            ) 
          res.all <- res.all %>% 
            filter(!Cell.out) %>%
            select(Meristem, User, Expertise) %>%
            left_join(Meristem)

          # Group expertise by NA, 0, 1
          res.all.sum <- res.all %>% 
            group_by(User, Meristem, Expertise) %>% 
            tally() %>%
            left_join(Meristem) %>%
            mutate(Expertise_f = factor(replace(Expertise, is.na(Expertise), "NA"))) %>%
            mutate(Meristem_f = factor(Meristem)) %>%
            mutate(Line_f = factor(Line)) %>%
            mutate(Indent_f = factor(Indent))

            res.all.sum.group <- xtabs(res.all.sum$n ~ res.all.sum$User + 
              res.all.sum$Expertise_f + res.all.sum$Meristem_f)
            par(mfrow = c(4,1), mar = c(5,5,2,1))
            for (Line in 1:4) { # Line <- 1
              for (Indent in 1) { # Indent <- 1
                all.M <- which(Meristem$Line == Line & Meristem$Indent == Indent)
                if (Line == 1 & Indent == 1) {all.M <- all.M[c(3, 2, 4, 1)]}
                res.bind <- numeric(0)
                for (M in 1:length(all.M)) { # M <- 1
                  res.bind <- cbind(res.bind, NA,
                    apply(t(res.all.sum.group[,,all.M[M]]), 2, function(x) x / sum(x)))
                }
                res.bind <- res.bind[,-1]
                res.bind <- res.bind[c(2,1,3),]
                expert.pos <- seq(expert.n, ncol(res.bind), dim(res.all.sum.group)[1] + 1)
                colnames(res.bind)[expert.pos] <- "YOU"
                barplot(res.bind,
                        space = 0, col = c("#7fc97f", "#beaed4", "#fed9a6"),
                        ylab = c("GFP-MBD","spr2-2 GFP-MBD","bot1-7 GFP-MBD","TUA6-GFP")[Line],
                        cex.lab = 2)
                axis(1, at = expert.pos - 0.5,
                     colnames(res.bind)[expert.pos], font.axis = 2,
                     col.axis = "red", col = NA, tick = TRUE, col.ticks = "red")
                if (Line == 1) {
                  title("Expertise for indented meristems (proportion of cells bundling ?)", cex.main = 2) 
                  legend("bottom", inset = -0.35, cex = 1.5,
                         horiz = TRUE, fill = c("#7fc97f", "#beaed4", "#fed9a6"),
                         legend = c("YES","NO","NA"), xpd = TRUE)#, title = "Bundling ?")
                }
                if (Line == 4) {
                  mtext(text = "Expert", side = 1, line = 3, cex = 1.5) 
                }
              }
            }
        }, width = 800, height = 800)
        
        values$nextImgVal <- input$nextImg
    })
  }) # end of save
  
  # Show output results only if clicked on save
  # Disappear if continue analysis
  output$comparison <- renderUI(
    if (values$nextImgVal == input$nextImg) {
      list(
        h2("Expertise accordance including yours"),
        h3("For all lines available"),
        helpText("x: From 50% agree to 100% agree"),
        helpText("y: Frequency of expertises in this x-category"),
        helpText("Answer is YES or NO, there is always more than 50% agree for one of the two possibilities..."),
        plotOutput("distPlot", width = 800, height = 800),
        h3("Expertise on proportion of bundling cell by indented meristem"),
        helpText("1 row per Line, 1 panel per indented meristem"),
        helpText("GFP-MBD meristems are displayed in growth order as in the publication"),
        helpText("'NA' is for cells that have not been expertised by the expert"),
        p("The variability of expertises shows the necessity to implement non subjective 
            and accurate indices to test for potential microtubule reactions. 
            This is one of the reason for which we tested different indices in our paper. 
            Another reason is the time needed by different experts to analyse cell one by one. 
            Implementing automatisation of indices calculation in a pipeline allows 
            the analysis of a greater number of observations.",
          style = "font-size: 12pt; color: #428bca"),
        plotOutput("distPlotMBD", width = 800, height = 800)   
      )
    }
  )
  
  # Validation of details on users
  values$msg4.1 <- ""
  observeEvent(input$ValidDetails, {
    isolate({
      details <- c(input$scientist, input$plant, input$bundle, input$years)
      save(details,file = paste0(outWD_details,"details",values$expertname,".RData"))
      values$details <- details
      values$msg4.1 <- paste("<p style = \"color:#008100; font-size:12pt\">
        We have saved your details, thank you !</p>")
    })
  })
  output$detailsmsg <- renderText({
    if (values$nextImgVal == input$nextImg) {
      HTML(values$msg4.1)
    }
  })
  
  # # Download results
  filename <- reactive({paste0(values$expertname,"_results.zip")})
  output$download_user <- downloadHandler(
    filename = filename,
    content = function(file) {
     zip(zipfile = file,
         files = c(list.files(outWD, full.names = TRUE)[
           grep(input$expertname, list.files(outWD, full.names = TRUE))],
                   list.files(outWD_details, full.names = TRUE)[
           grep(input$expertname, list.files(outWD_details, full.names = TRUE))]
           ),
         flags = "-rj9X")
   },
   contentType = "application/zip"
  )

          
})
