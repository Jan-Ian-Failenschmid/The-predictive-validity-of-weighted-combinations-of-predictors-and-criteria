# The predictive validity of weighted combinations of predictors and criteria
## An interactive demonstration of Murphy (2019).

### Manual

In most selection procedures, multiple instruments are used to predict one or more criteria of interest.
This tool uses the formula presented in Murphy (2019) and allows you to see how the multivariate validity changes depending on how much weight you give to the instruments that are used and the criteria that are predicted.

In practice, it may be common to obtain scores or ratings from a cognitive ability test, a conscientiousness questionnaire, and an interview, to predict later job performance. Based on validity estimates presented in Cortina et al. (2000), the default option of the app allows you to choose one or more of these predictors for the prediction of job performance. 

Alternatively, you can specify your own predictors and criteria via the ‘Other’ option, and enter your own correlations by clicking on a cell in the correlation matrix presented under the tab ‘Multivariate validity’.

### Installation

This Application is written in [Shiny](https://shiny.rstudio.com) and is work-in-progress. Therefore, it has not been published formally yet. 

To use the application you need to have [R](https://www.r-project.org) and [RStudio](https://www.r-project.org) installed.
The latest version of the application uses R Version 4.0.3.

Now, you can download the "app" directory and run the included "app" file in RStudio. 
Set the "app" directory as your working directory. 

Before executing the application, you have to install the required packages by esecuting this code:

    install.packages(c("shiny", "purrr", "rhandsontable", "shinyBS"))
    
Now you can run the app. 

### References 

Cortina, J. M., Goldstein, N. B., Payne, S. C., Davison, H. K., & Gilliland, S. W. (2000). The incremental validity of interview scores over and above cognitive ability and    conscientiousness scores. *Personnel Psychology, 53,* 325–351. https://doi.org/10.1111/j.1744-6570.2000.tb00204.x

Murphy, K. R. (2019). Understanding how and why adding valid predictors can decrease the validity of selection composites: A generalization of Sackett, Dahlke, Shewach, and Kuncel (2017). *International Journal of Selection and Assessment, 27,* 249–255. https://doi.org/10.1111/ijsa.12253

Voss, N. M., & Lake, C. J. (2020). Communicating validity information to differentially experienced audiences: The effects of numeracy and nontraditional metrics. *Personnel Assessment and Decisions,* 6. https://doi.org/10.25035/pad.2020.02.003
