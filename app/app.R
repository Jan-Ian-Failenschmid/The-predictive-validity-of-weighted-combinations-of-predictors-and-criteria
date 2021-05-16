# Loads the necessary Packages

library(shiny)
library(purrr)
library(rhandsontable)
library(shinyBS)
library(psych)

# Creates the underlying background Matrix and the Predictor and Criteria Vectors

background_predictors <- c("Cognitive Ability", "Conscientiousness", "Level 1 Interview")

background_criteria <- c("Job Performance")

background_matrix <- matrix(c(1, .075, .055, .450, 0, .075, 1, .092, .267, 0, .055, .092, 1, .2, 0, .450, .267, .2, 1, 0, 0, 0, 0, 0, 0),
  nrow = 5, ncol = 5,
  dimnames = list(
    c(background_predictors, background_criteria, "Placeholder"),
    c(background_predictors, background_criteria, "Placeholder")
  )
)

# Custom functions

symmetrize <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]

  return(m)
}

mult_val_by_pred <- function(m, n, i, o, p) {
  full_matrix <- matrix(rep(0, 2 + length(i)), nrow = 1, ncol = 2 + length(i))
  full_matrix <- full_matrix[-1, ]

  for (q in c(1:length(n))) {
    pc_matrix <- m[c(n[q], i), c(n[q], i)]

    o1 <- o[q]
    pT <- t(p)

    corr_mat_xx <- pc_matrix[n[q], n[q]]
    corr_mat_yy <- pc_matrix[i, i]
    corr_mat_xy <- pc_matrix[n[q], i]

    numerator <- o1 %*% corr_mat_xy %*% p

    variabley <- pT %*% corr_mat_yy %*% p
    variablex <- o1 %*% corr_mat_xx %*% o1
    denumerator <- sqrt(variabley) * sqrt(variablex)

    multiple_validity <- numerator / denumerator
    multiple_validity_rounded <- round(multiple_validity, digits = 2)

    qmatrix <- matrix(c(n[q], i, multiple_validity_rounded), nrow = 1, ncol = 2 + length(i))

    full_matrix <- rbind(full_matrix, qmatrix)

    criterion_variable_name <- character(0)

    for (r in c(1:length(i))) {
      criterion_variable_number <- paste("Criterion", r, sep = " ")
      criterion_variable_name <- c(criterion_variable_name, criterion_variable_number)
    }
  }

  full_matrix <- as.data.frame(full_matrix)

  colnames(full_matrix) <- c("Predictor", criterion_variable_name, "Multivariate Validity")

  return(full_matrix)
}

custom_names <- function(m, p, c, bp, bc) {
  pred_names1 <- p
  pred_names1[!p %in% bp] <- "Placeholder"

  crit_names1 <- c
  crit_names1[!c %in% bc] <- "Placeholder"

  corr_matrix <- m[c(pred_names1, crit_names1), c(pred_names1, crit_names1)]
  dimnames(corr_matrix) <- list(c(p, c), c(p, c))
  return(corr_matrix)
}

# Crates a UI interface that adjusts to the Size of it's content

ui <- fluidPage(

  # Reference Header

  headerPanel(h1("The predictive validity of weighted combinations of predictors and criteria")),
  sidebarLayout(
    sidebarPanel(

      fluidRow(

        # Asks for the amount of predictors

        column(
          width = 8,
          h4("How many predictors do you want to use?"),
          numericInput("n1", label = NULL, value = 1, min = 1),
          offset = 0
        ),

        # Headers the second "Weights" column

        column(
          width = 2,
          h4("Weights"),
          offset = 0
        )
      ),
      fluidRow(

        # Prints the apropriate amount of dropdown bars created for the predictors

        column(
          width = 8,
          uiOutput("pred"),
          offset = 0
        ),

        # Prints the apropriate amount of dropdown bars created for the predictor weights

        column(
          width = 3,
          uiOutput("pred_weights"),
          offset = 0
        )
      ),
      fluidRow(

        # Asks for the amount of criteria

        column(
          width = 8,
          h4("How many criteria do you want to use?"),
          numericInput("n2", label = NULL, value = 1, min = 1),
          offset = 0
        ),

        # Headers the second "Weights" column

        column(
          width = 2,
          h4("Weights"),
          offset = 0
        )
      ),
      fluidRow(

        # Prints the apropriate amount of dropdown bars created for the criteria

        column(
          width = 8,
          uiOutput("crit"),
          offset = 0
        ),

        # Prints the apropriate amount of dropdown bars created for the criteria weights

        column(
          width = 3,
          uiOutput("crit_weights"),
          offset = 0
        )
      ),
      fluidRow(

        # Action button for optimizer

        actionButton("optim", "Calculate Optimal Predictor Weights", class = "btn-info"),
      ),
      width = 5
    ),
    mainPanel(

      tabsetPanel(
        id = "switcher",
        tabPanel(
          "Manual",
          h4("In most selection procedures, multiple instruments are used to predict one or more criteria of interest."),
          h4("This tool uses the formula presented in Murphy (2019) and allows you to see how the multivariate validity 
            changes depending on how much weight you give to the instruments that are used and the criteria that are 
            predicted."),
          h4("In practice, it may be common to obtain scores or ratings from a cognitive ability test, a 
            conscientiousness questionnaire, and an interview, to predict later job performance. Based on validity 
            estimates presented in Cortina et al. (2000), the default option of the app allows you to choose one or 
            more of these predictors for the prediction of job performance."),
          h4("Alternatively, you can specify your own 
            predictors and criteria via the 'Other' option, and enter your own correlations by clicking on a cell 
            in the correlation matrix presented under the tab 'Multivariate validity'."),
          h4("Lastly, you can also use the 'Calculate Optimal Predictor Weights' button after specifying your 
            predictors and criteria. The app will then calculate the optimal standardized multivariate and univariate 
            regression weights, based your correlation matrix and present them in a new tab."),
          h4("Because the App only updates the Tab that you aree currently using you are advised to select the 
            'Multivariate Validity' when making any changes to the model specifications on the left, or select the Tab 
            briefly to afterwards to apply the changes to the correlation matrix. Otherwice, some calculations on the other 
            Tabs might not be correct.")
        ),
        tabPanel(
          "Multivariate Validity",

          # Prints the relevant subset of the correlation matrix for further insight

          h4("Correlation matrix of the chosen predictors and criteria"),
          h5("(You can click on a cell in the table below to change a correlation value)"),
          rHandsontableOutput("hot_render"),

          # Prints the result of the multivariate validity

          h4(textOutput("validity")),

          # Prints a table including alternative measures of multivariate validity information from Voss & Lake, 2020

          p(h4("Alternative measures of validity information based on Voss and Lake (2020)")),
          fluidRow(
            column(
              width = 9,
              tableOutput("table_alt1"),
            ),
            column(
              width = 1,
              img(src = "blanc_space.png", height = 30, weight = 30),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The proportion of change in one variable predicted by change in another variable."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability that if a randomly selected individual has a high score on one variable, they will also have a high score on another variable."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability of success that can be attributed to a particular method used on one group of individuals in comparison to the probability of success of not using that particular method."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability of failure that can be attributed to a particular method used on one group of individuals in comparison to the probability of failure of not using that particular method."),
                placement = "right", trigger = "hover", options = NULL
              ),
            )
          ),
          fluidRow(

            # Prints a warning if a single correlation is bigger than or equal to the multivariate validity

            h5(textOutput("warning"), style = "color:red"),
            tableOutput("warning1"),
          )
        ),
        tabPanel(
          "Underlying Matrix",
          tableOutput("back_mat_inspec")
        ),
        tabPanel(
          "Optimizer",
          fluidRow(
            h3("Optimized Regression Weights"),
            h4("The optimized regression weights maximize validity based on your model.
                 However, the calculations for the validity are, because of the rounding error,
                 robust to reasonable changes. For practical puposes these weights 
                 should therefore be seen as a suggestion and can be altered to the liking of the user, with potentially little loss.
                 This can be inspected by coppying the regression weights into the model in the side-panel and experiment with the weights."),
            h4("Further, these weights represent standardized regression weights. Hence, should they wished to be used with real data, 
                 the raw scores first have to be standardized. This can be achieved by calculating the difference between 
                 each score and their mean and deviding the result by the standard deviation."),
            a("A more thorough introduction to standardization can be found here",
              href = "https://www.statisticshowto.com/standardized-values-examples/"
            ),
          ),
          fluidRow(
            h3("Optimized Multivariate Regression Weights"),
            h4("The table below shows the optimized multivariate regression weights for the predictors in your selected model.
                 These maximize the overall validity based on the weights that you assigned to each criterion."),
            tableOutput("optweights_tbl"),
            h4(textOutput("Roptimal_out")),
          ),
          fluidRow(
            h4("Alternative measures of validity information based on Voss and Lake (2020)"),
            column(
              width = 9,
              tableOutput("table_alt2"),
            ),
            column(
              width = 1,
              img(src = "blanc_space.png", height = 30, weight = 30),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The proportion of change in one variable predicted by change in another variable."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability that if a randomly selected individual has a high score on one variable, they will also have a high score on another variable."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability of success that can be attributed to a particular method used on one group of individuals in comparison to the probability of success of not using that particular method."),
                placement = "right", trigger = "hover", options = NULL
              ),
              tipify(img(src = "info_button.png", height = 32, weight = 32),
                dQuote("The probability of failure that can be attributed to a particular method used on one group of individuals in comparison to the probability of failure of not using that particular method."),
                placement = "right", trigger = "hover", options = NULL
              ),
            ),
          ),
          fluidRow(
            h3("Optimized Univariate Regression Weights"),
            h4("The table below shows the optimized univatiate regression weights for the predictors in your selected model.
                These maximize the validity of the prediction of each individual criterion."),
            tableOutput("optim_table"),
            h4("The graph below illustrates how the univariate regression weights shown above connect the varaibles in the model."),
            plotOutput("optim_plot"),
          ),
        ),
        tabPanel(
          "References",
          h5(
            "Cortina, J. M., Goldstein, N. B., Payne, S. C., Davison, H. K., & Gilliland, S. W. (2000). The incremental validity of interview scores over and above cognitive ability and conscientiousness scores. ",
            em("Personnel Psychology, 53, "), "325-351. https://doi.org/10.1111/j.1744-6570.2000.tb00204.x"
          ),
          h5(
            "Murphy, K. R. (2019). Understanding how and why adding valid predictors can decrease the validity of selection composites: A generalization of Sackett, Dahlke, Shewach, and Kuncel (2017). ",
            em("International Journal of Selection and Assessment, 27, "), "249-255. https://doi.org/10.1111/ijsa.12253"
          ),
          h5(
            "Voss, N. M., & Lake, C. J. (2020). Communicating validity information to differentially experienced audiences: The effects of numeracy and nontraditional metrics. ",
            em("Personnel Assessment and Decisions, 6. "), "https://doi.org/10.25035/pad.2020.02.003"
          )
        )
      ),
      width = 7
    )
  )
)

server <- function(input, output) {

  # Takes the desired number of predictors and creats a suiting amount of inputs and saves them

  numb_pred <- reactive(paste0("pred", seq_len(input$n1)))

  output$pred <- renderUI({
    map(numb_pred(), ~ selectizeInput(.x,
      label = NULL, choices = c(background_predictors, "Other (Backspace to change name)"),
      selected = isolate(input[[.x]]) %||% "Predictor Name", multiple = FALSE,
      options = list("plugins" = list("remove_button"), placeholder = "Predictor Name", "create" = TRUE, "persist" = TRUE)
    ))
  })

  pred_names <- reactive(map_chr(numb_pred(), ~ input[[.x]]))

  # Takes the desired number of predictors and creats a suiting amount of inputs  for the weights and saves them

  numb_pred1 <- reactive(paste0("pred_weights", seq_len(input$n1)))

  output$pred_weights <- renderUI({
    map(numb_pred1(), ~ numericInput(.x, NULL, value = isolate(input[[.x]]) %||% 1, width = 100))
  })

  pred_weights1 <- reactive(map_dbl(numb_pred1(), ~ input[[.x]]))

  # Takes the desired number of criteria and creats a suiting amount of inputs and saves them

  numb_crit <- reactive(paste0("crit", seq_len(input$n2)))

  output$crit <- renderUI({
    map(numb_crit(), ~ selectizeInput(.x,
      label = NULL, choices = c(background_criteria, "Other (Backspace to change name)"),
      selected = isolate(input[[.x]]) %||% "Predictor Name", multiple = FALSE,
      options = list("plugins" = list("remove_button"), placeholder = "Criterion Name", "create" = TRUE, "persist" = TRUE)
    ))
  })

  crit_names <- reactive(map_chr(numb_crit(), ~ input[[.x]]))

  # Takes the desired number of criteria and creats a suiting amount of inputs  for the weights and saves them

  numb_crit1 <- reactive(paste0("crit_weights", seq_len(input$n2)))

  output$crit_weights <- renderUI({
    map(numb_crit1(), ~ numericInput(.x, NULL, value = isolate(input[[.x]]) %||% 1, width = 100))
  })

  crit_weights1 <- reactive(map_dbl(numb_crit1(), ~ input[[.x]]))

  # Prints the Background Matrix for Inspection

  output$back_mat_inspec <- renderTable(background_matrix[c(background_predictors, background_criteria), c(background_predictors, background_criteria)],
    rownames = T, colnames = T, align = "c", hover = T
  )


  # Creates a Subset of the Background Matrix based on the Input and Renders it as a Handsontable while observing the Changes to it

  corr_matrix1 <- reactive(custom_names(background_matrix, pred_names(), crit_names(), background_predictors, background_criteria))

  output$hot_render <- renderRHandsontable(rhandsontable(corr_matrix1(), rowHeaderWidth = 150) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row == col) {
              td.style.background = 'lightgrey';
             } else if (col > row) {
              td.style.background = 'grey';
              td.style.color = 'grey';
             } else if (value < -0.75) {
              td.style.background = 'lightgreen';
             } else if (value > 0.75) {
              td.style.background = 'lightgreen';
             }
           }") %>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE))

  corr_matrix2 <- reactive(matrix(hot_to_r(input$hot_render),
    nrow = (input$n1 + input$n2), ncol = (input$n1 + input$n2),
    dimnames = list(c(pred_names(), crit_names()), c(pred_names(), crit_names()))
  ))

  # Makes the matrix diagonally symmetrical

  corr_matrix <- reactive(symmetrize(corr_matrix2()))


  # Inputs the criterion weights and the test weights, transforms them into a matrix

  Cweight <- reactive(matrix(crit_weights1(), ncol = 1, byrow = T))

  Tweight <- reactive(matrix(pred_weights1(), ncol = 1, byrow = T))


  # Create transpose of each of these matrices

  CweightT <- reactive(t(Cweight()))
  TweightT <- reactive(t(Tweight()))


  # Input correlation matrices among predictor variables and among criterion variables and between them

  Rxx <- reactive(corr_matrix()[pred_names(), pred_names()])

  Ryy <- reactive(corr_matrix()[crit_names(), crit_names()])

  Rxy <- reactive(corr_matrix()[pred_names(), crit_names()])


  # Calculates multivariate validity and rounds it to two digits

  numer <- reactive(TweightT() %*% Rxy() %*% Cweight())

  vary <- reactive(CweightT() %*% Ryy() %*% Cweight())

  varx <- reactive(TweightT() %*% Rxx() %*% Tweight())

  denom <- reactive(sqrt(vary()) * sqrt(varx()))

  multval <- reactive(numer() / denom())

  multval_round <- reactive(round(multval(), digits = 2))


  # Renders the output of the calculated validity

  output$validity <- renderText({
    c(
      "The validity of your chosen composite is",
      multval_round(), "."
    )
  })


  # Renders a warning if a simple correlation is bigger than or equal to the multivariate validity

  mult_val_by_pred1 <- reactive(mult_val_by_pred(corr_matrix(), pred_names(), crit_names(), pred_weights1(), crit_weights1()))

  mult_val_max <- reactive(mult_val_by_pred1()[which.max(mult_val_by_pred1()[, 2 + length(crit_names())]), ])

  mult_val_max_num <- reactive(as.numeric(mult_val_max()[, 2 + length(crit_names())]))

  output$warning <- renderText({
    if (mult_val_max_num() >= multval() && input$n1 > 1) {
      c("Be careful, the validity using only ", mult_val_max()[, 1], "(validity: ", mult_val_max_num(), ")", " 
        as a predictor is greater or equal to the overall validity of the selection composite. 
        You may consider removing some predictors.")
    }
  })

  output$warning1 <- renderTable(
    {
      if (mult_val_max_num() >= multval() && input$n1 > 1) {
        mult_val_max()
      }
    },
    hover = T
  )


  # Computes and renders non-traditional measures of validity information

  coef_det <- reactive(multval()^2)

  clr <- reactive((asin(multval())) / pi + 0.50)

  besd_s <- reactive(0.50 + multval() / 2)

  besd_f <- reactive(0.50 - multval() / 2)

  names_alt <- c(
    "Coefficient of Determination R^2", "Common Language Effect Size CLR",
    "Binomial Effect Size Display Success", "Binomial Effect Size Display Failure"
  )

  values_alt <- reactive(round(c(coef_det(), clr(), besd_s(), besd_f()), digits = 2))

  matrix_alt <- reactive(matrix(c(names_alt, values_alt()),
    nrow = 4, ncol = 2,
    dimnames = list(c(1, 2, 3, 4), c("Statistic", "Estimate"))
  ))

  output$table_alt1 <- renderTable(matrix_alt(), striped = T, colnames = TRUE)

  # Weight optimizer

  hideTab(inputId = "switcher", target = "Optimizer")

  observeEvent(input$optim, {
    showTab(inputId = "switcher", target = "Optimizer", select = T)

    # Univariate Weights

    sc <- reactive(setCor(crit_names(), pred_names(), corr_matrix(), std = T))

    output$optim_table <- renderTable(coefficients(sc()), rownames = T, hover = T)

    output$optim_plot <- renderPlot(setCor.diagram(sc()))

    # Multivariate Weights

    # Calculate optimal Weights

    numerx <- reactive(solve(Rxx()) %*% Rxy() %*% Cweight())

    denox <- reactive(sqrt(CweightT() %*% Ryy() %*% Cweight()))

    div <- reactive(denox()[1])

    optweights <- reactive(numerx() / div())

    optweights1 <- reactive(optweights() / sum(optweights()) * 100)

    pred_weights2 <- reactive(pred_weights1() / sum(pred_weights1()) * 100)

    optweights_m <- reactive(matrix(c(optweights1(), pred_weights2()),
      ncol = 2,
      dimnames = list(pred_names(), c("Optimal Weight", "Your Weights"))
    ))

    output$optweights_tbl <- renderTable(optweights_m(), rownames = T, colnames = T, hover = T)

    # Calculate Optimal Multivariate Validity

    optweightsT <- reactive(t(optweights()))

    numer2 <- reactive(optweightsT() %*% Rxy() %*% Cweight())

    varx2 <- reactive(optweightsT() %*% Rxx() %*% optweights())

    denom2 <- reactive(sqrt(vary()) * sqrt(varx2()))

    Roptimal <- reactive(round(numer2() / denom2(), digits = 2))

    Rdiff <- reactive(round(Roptimal() - multval(), digits = 2))

    output$Roptimal_out <- renderText(c(
      "The Multivariate Validity of your Model using the 
                                        optimized predictor weights is ", Roptimal(),
      ". This constitutes an improvement of ", Rdiff(),
      "over and above the Multivariate Validity 
                                        of your Model using the predictor weights 
                                        from your input."
    ))

    # Computes and renders non-traditional measures of validity information for the optimal weights

    coef_det_opt <- reactive(Roptimal()^2)

    clr_opt <- reactive((asin(Roptimal())) / pi + 0.50)

    besd_s_opt <- reactive(0.50 + Roptimal() / 2)

    besd_f_opt <- reactive(0.50 - Roptimal() / 2)

    names_alt_opt <- c(
      "Coefficient of Determination R^2", "Common Language Effect Size CLR",
      "Binomial Effect Size Display Success", "Binomial Effect Size Display Failure"
    )

    values_alt_opt <- reactive(round(c(coef_det_opt(), clr_opt(), besd_s_opt(), besd_f_opt()), digits = 2))

    matrix_alt_opt <- reactive(matrix(c(names_alt_opt, values_alt_opt()),
      nrow = 4, ncol = 2,
      dimnames = list(c(1, 2, 3, 4), c("Statistic", "Estimate"))
    ))

    output$table_alt2 <- renderTable(matrix_alt_opt(), striped = T, colnames = TRUE)
  })

  # Switches automatically to the "Multivariate Validity" tab if new Variables are generated to apply changes 
  
  observeEvent(
    {
      input$n1
      input$n2
    },
    updateTabsetPanel(session = getDefaultReactiveDomain(), "switcher", selected = "Multivariate Validity"),
    ignoreInit = T
  )
}


# Runs the application

shinyApp(ui = ui, server = server)