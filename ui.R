library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(useShinyjs(),
    #Application title:
    titlePanel(title =  div(img(src="Logo_MAGPI-white.jpeg"), 'Morphological and Kinematic Classification',img(src="Iam50000yrsolder.jpeg"),)),
    sidebarLayout(position='left', fluid=FALSE,
      sidebarPanel(h4("First, please enter your name and click the \'update name\' button"),
                   textInput("MAGPIte",label="FirstnameLastname",value=""), actionButton("MAGPIteDone",label="Update name"), h5("Name set to:"), uiOutput("MAGPIte"),
                   h5(''),
                   fileInput(inputId = "infile", label="And/or you may manually upload a specific classification file",
                             multiple = FALSE,
                             accept = c("csv",
                                        "comma-separated-values",
                                        ".csv")),
                   h5(strong("Let us know if this is an edge-of-the-field case.")),
                   checkboxInput("EdgeCase",label="Yes, this is an edge case.", value=FALSE),
                   h5(''),
                   radioButtons(inputId="Morph", label="Morphological classification", choices=list("(0) None selected", 
                                                                                                    "(1) Elliptical (E)",
                                                                                                    "(2) Lenticular (S0)",
                                                                                                    "(3) Early Spiral (eSp)",
                                                                                                    "(4) Late Spiral (lSp)",
                                                                                                    "(5) Irregular",
                                                                                                    "(6) Merger",
                                                                                                    "(7) I don't know")),
                   checkboxInput("BarFlag",label='This galaxy is barred.', value=FALSE),
                   checkboxInput("VisFeatFlag",label="There are other features in the image", value=FALSE),
                   h5('Stellar kinematics classification'),
                   fluidRow(
                     column(width = 4,
                            radioButtons(inputId="StellRR", label='', choices=list("(0) None selected", 
                                                                                  "(1) Obvious Rotation",
                                                                                  "(2) No Obvious Rotation",
                                                                                  "(3) I don't know")),
                     ),
                     column(width = 4,
                            radioButtons(inputId="StellFeat", label='', choices=list("(0) None selected", 
                                                                                   "(1) without feature",
                                                                                   "(2) with feature(s)",
                                                                                   "(3) I don't know")),
                     )
                   ),
                   checkboxInput("StellKinFlag",label="There are issue(s) with the stellar kinematics maps.", value=FALSE),
                   
                   h5('Ionised gas kinematics classification'),
                   fluidRow(
                     column(width = 4,
                            radioButtons(inputId="GasRR", label='', choices=list("(0) None selected", 
                                                                                 "(1) Obvious Rotation",
                                                                                 "(2) No Obvious Rotation",
                                                                                 "(3) I don't know")),
                     ),
                     column(width = 4,
                            radioButtons(inputId="GasFeat", label='', choices=list("(0) None selected", 
                                                                                  "(1) without feature",
                                                                                  "(2) with feature(s)",
                                                                                  "(3) I don't know")),
                     )
                   ),
                   checkboxInput("GasKinFlag",label="There are issue(s) with the gas kinematics maps.", value=FALSE),
                   fluidRow(
                     column(width = 9,textInput(inputId = 'comment', 
                             label ='Comment (do not use comma, click add when done)?', 
                             value = "", width = NULL,
                             placeholder = NULL)),
                     column(width = 1, actionButton(inputId = "addcomment", 
                                label = "Add"),
                     )),
                    actionButton("minusone",label="Previous"), actionButton("plusone",label="Next"),
                    downloadButton("saveClass", "Click to also save your work locally")
                  ),
      #Main panel:
      mainPanel(tabsetPanel(
                  tabPanel("Maps",
                        h4("**Before you get started, pls refer to the Instructions tab above.**"),
                        verbatimTextOutput("magpiid"),
                        plotOutput("galimage"),
                        plotOutput('plotstelmaps'),
                        fluidRow(
                          column(4, offset=1,
                                 sliderInput("StelVelRange", "Modify stellar velocity map range:",
                                               min = -1000, max = 1000,
                                               value = c(-500,500))
                          ),
                          column(4, offset=1,
                                 sliderInput("StelSigRange", "Modify stellar dispersion map range:",
                                             min = 0, max = 1000,
                                             value = c(0,500)),
                          ),
                          actionButton("resetstelscales",label="Reset stellar kinematic scales to default")
                        ),
                        plotOutput('plotgasmaps'),
                        fluidRow(
                          column(4,offset=1,
                                 sliderInput("GasVelRange", "Modify gas velocity map range:",
                                             min = -1000, max = 1000,
                                             value = c(-500,500))
                          ),
                          column(4,offset=1,
                                 sliderInput("GasSigRange", "Modify gas dispersion map range:",
                                             min = 0, max = 1000,
                                             value = c(0,500)),
                          ),
                     ),
                     actionButton("resetgasscales",label="Reset gas kinematic scales to default")
                  ),
                  tabPanel("Summary", h5("Your answers so far:"),
                           uiOutput("Summary")
                           ),
                  tabPanel("Instructions",
                           h4('Background:'),
                           h5('We will be following the classification scheme employed by SAMI (see Cortese et al. (2016, MNRAS, 463, 170) 
                              for morphologies and van de Sande et al. (2021, MNRAS, 505, 3078) for kinematics).'),
                           h4('Selection:'),
                           h5('For our purposes, we will only look at galaxies from the Master catalogue that satisfy the following criteria:'),
                           h5('1- R50_it > 0.75 x fwhm_i (i.e. source is extended according to ProFound)'),
                           h5('2- re > 0.75 x fwhm_r (i.e. source is extended according to GalFit)'),
                           h5('3- mag_it < 26 (source is brighter than ~ the completeness limit in i-band, this weeds out a lot of faint blobs, though not all, sorry...)'),
                           h5('The above selection yields 637 galaxies to visually classify (as at February 2023).'),
                           h5('Each MAGPIte gets their own special randomly ordered list of the galaxies.'),
                           h4('Your mission (should you choose to accept it!):'),
                           h5("1- Fill the box with your full name (FirstLast, no spaces). This will be part of the output
                           file name to keep things tidy and ensure you don't have to start from scratch next time around as your
                           work is saved on the server."),
                           h5("2- For each target, select the statements on the left that best apply
                           to your opinion of the morphology and kinematics. Include any comments and tick any statement that apply.
                           When you are happy with your input click the next button to record your answers. Your answers should then appear
                           in the Summary tab."),
                           h5("3- If you made a mistake or change your mind, you may click previous to return to the previous galaxy
                           (but note that it will not retain what you have input in the subsequent targets).",
                           h5("4-If you need a break, the output will be saved locally under your name and can always be loaded back later."),
                           h5("5-Finally, if you ever wanted to save a local copy of your work, click the download button."),
                           h4('Notes on edge cases:'),
                           h5('Some targets are cropped due to being located at the edge of the MAGPI field. Please flag 
                           those by ticking the appropriate box.'),
                           h4('Notes on morphologies:'),
                           h5('Elliptical (E) = smooth, featureless;'),
                           h5('lenticular (S0) = obvious disk, no evidence for spiral arms, substantial bulge;'),
                           h5('Early Spiral (eSp) = evidence for spiral arms and a bulge;'),
                           h5('Late Spiral (lSp) = prominent spiral arms and minimal/no bulge;'),
                           h5('Irregular (Irr) = no distrinct regular shape;'),
                           h5('Merger = visual evidence of ongoing merger'),
                           h5('Despite best efforts, some blobs make it through the selection, which we have tried to keep generous, please mark those as I dont know'),
                           h5('Let us know whether the galaxy has an evident bar by ticking the appropriate box.'),
                           h5('Finally, please alert us of any unlisted features in the image by 
                              ticking the appropriate box (consider leaving a comment at the bottom of the form).'),
                           h4('Notes on stellar and gas dynamics:'),
                           h5('Consider whether the respective (gas/star kinematic) maps exhibit 
                              obvious rotation (OR: clear rotation, possibly accompanied by peaked or 
                              flat sigma) or no obvious rotation (NOR). Examples are shown in the next tab.'),
                           h5('Indicate whether or not you can see unusual features in the kinematic maps 
                              such as 2 velocity dispersion peaks, changes in the kinematic position angle 
                              such as kinematically decouple cores (KDC) or kinematic twists (KT) and consider 
                              including anything that stands out in the comment at the bottom of the form.'),
                           h5('Stellar velocity maps are initially set to range between the 5th and 95th percentile, 
                              while dispersion are initially set to range between the 0th and 95th percentile. You may use
                              the sliders to adjust the range.'),
                           h5('Finally, let us know if there are issues with the kinematic data that may limit 
                              their usability for science by ticking the appropriate box.'),
                           h4('Other notes:'),
                           h5('The image is the synthetic g, r and i image from the MUSE datacube cutout to the ProFound segment for that object.'),
                           h5('Red ellipses show the 1Re from galfit'),
                           h5('White/black circles show the FWHM.'),
                           h4('Contact: '),
                           h5('Caroline Foster, c.foster@unsw.edu.au'),
                           h4('Thank you and clear skies!'),
                           )),
                  tabPanel("Examples",
                           img(src="jvds_kin_class_flowchart_example_v3.png",width=800),
                           h5("This flow chart is taken from van de Sande et al. (2021, MNRAS, 505, 3078) 
                              and shows examples of each class for your consideration.")
                  )
                )
          )
    )
  )
)

