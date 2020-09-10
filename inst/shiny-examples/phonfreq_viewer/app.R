# Shiny web app for plotting Australian segment frequencies in Ausphonlex wordlists
# Jayden Macklin-Cordes

# Design user interface
ui = fluidPage(

  titlePanel("Australian Phoneme Frequencies"),

  sidebarLayout(

    sidebarPanel(
      width = 3,
      h4("Select language"),
      selectInput("select_language",
                  label = NULL,
                  choices = sort(unique(phonfreq::aus_frequencies$variety_name))),
      br(),
      h4("Plotting options"),
      checkboxInput("select_labels", label = "Include phoneme labels", value = TRUE),
      checkboxInput("select_lm", label = "Include linear model"),
      br(), br(), br(), br(),
      br(), br(), br(), br(),
      br(), br(), br(), br(),
      br(), br(), br(), br(),
      br(), br(), br(), br(),
      h4("Distribution types"),
      checkboxGroupInput("select_lines",
                         label = NULL,
                         choices = c("Power law" = "pl",
                                     "Lognormal" = "ln",
                                     "Exponential" = "exp",
                                     "Poisson" = "pois")),
      br(),
      checkboxInput("select_xmin", label = "Include Xmin"),
      checkboxInput("log_scale", label = "Log-scaled axes", value = TRUE)
    ),

    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Home",
                 h3(textOutput("frequencies_header")),
                 plotOutput("frequency_plot"),
                 br(),
                 p("This app contains phoneme frequencies for 168 Australian language varieties.
                   The histogram on the left displays frequencies expressed as a fraction of all phonemes
                   in the selected language's lexicon. Phonemes are ordered by their statistical rank,
                   from most frequent to least frequent."),
                 p("The dot plot on the right gives exactly the same information, but both the x and y axes
                   have been log transformed. Traditionally, a straight line on this plot has been taken as
                   evidence of a power law. You can fit a straight line on the plot by selecting the
                   'Include linear model' option in the sidepanel. For most languages, it seems to fit
                   rather poorly, but there are exceptions."),

                 h3("Fitting distributions"),
                 plotOutput("cdf_plot", width = "66%"),
                 br(),
                 p("On the plot above, you can view the inferred fit of power law, exponential, lognormal
                   and Poisson distributions to the phoneme frequencies of each language. In the sidepanel,
                   you can select which distribution types you'd like to view."),
                 p("If 'Include Xmin' is selected, a maximum likelihood estimate of Xmin is included and
                   the distribution is fitted only to phonemes of equal or greater frequency to this minimum threshold.
                   If this option is unselected, the distribution is fitted to all phoneme frequencies
                   in the selected language."),
                 p("Here, as above, the y axis shows frequency. The x axis shows
                   the cumulative probability, P(X ≥ x). That is, the probability that the frequency of a given phoneme
                   will be greater than or equal to a given value, x."),
                 p("Viewing the plot with log-scaled axes can help illustrate better the differences
                   in fit between distributions."),

                 h3("Parameter uncertainty"),
                 br(),
                 conditionalPanel("input.select_lines==false", em("(Select a distribution type from the sidebar to view)")),
                 conditionalPanel("input.select_lines.includes('pl') && input.select_xmin==false",
                                  h5("Power law distribution"), plotOutput("bs_pl")),
                 conditionalPanel("input.select_lines.includes('ln') && input.select_xmin==false",
                                  h5("Lognormal distribution"), plotOutput("bs_lnorm")),
                 conditionalPanel("input.select_lines.includes('exp') && input.select_xmin==false",
                                  h5("Exponential distribution"), plotOutput("bs_exp")),
                 conditionalPanel("input.select_lines.includes('pois') && input.select_xmin==false",
                                  h5("Poisson distribution"), plotOutput("bs_pois")),
                 conditionalPanel("input.select_lines.includes('pl') && input.select_xmin==true",
                                  h5("Power law distribution (with xmin)"), plotOutput("bs_pl_xmin")),
                 conditionalPanel("input.select_lines.includes('ln') && input.select_xmin==true",
                                  h5("Lognormal distribution (with xmin)"), plotOutput("bs_lnorm_xmin")),
                 conditionalPanel("input.select_lines.includes('exp') && input.select_xmin==true",
                                  h5("Exponential distribution (with xmin)"), plotOutput("bs_exp_xmin")),
                 conditionalPanel("input.select_lines.includes('pois') && input.select_xmin==true",
                                  h5("Poisson distribution (with xmin)"), plotOutput("bs_pois_xmin")),
                 br(),
                 p("These plots show bootstrapping results for any distribution types selected in the sidepanel,
                   giving an indication of statistical uncertainty in our parameter estimates and p values."),
                 p("The bootstrapping process consists of randomly resampling with replacement, from a given language's
                   observed phoneme frequencies. Parameter estimates are taken from each random sample and this process
                   is repeated 5000 times per language."),
                 p("For each distribution type, there are two rows of plots. The top row gives the mean of each
                   parameter estimate/p value at any given point throughout the bootstrapping process.
                   The bottom row gives the standard deviation. On each plot, all bootstrapping iterations,
                   from the first to the 5000th iteration, are ordered on the x axis. The y axis shows
                   the value of interest. Red lines show standard error."),
                 p("Plotted values are: 'Xmin', the lower threshold
                   of the distribution (raw count, not relativised); 'Par1' (and 'Par2' for the lognormal distribution),
                   the parameter/s defining the shape of the distribution (e.g. α for the power law distribution); 'ntail',
                   the size of the sample to which the distribution has been applied (i.e. the number of observations equal
                   or greater than Xmin); and 'p-value' for the null hypothesis that the observations are drawn from
                    the inferred distribution (p ≥ 0.1 is taken as support for this hypothesis, though note that other                            distributions may also
                   fit equally well or better)." ),
                 p("'Xmin' and 'ntail' appear as a flat line when no Xmin is inferred. In these cases, Xmin is set simply to
                   the lowest observed phoneme frequency in the selected language and thus appears as a fixed value throughout
                   bootstrapping. Likewise, 'ntail' will stay fixed simply to the total number of phonemes in the language.")),
        tabPanel("Cite",
                 p("This app constitutes S3 of the Supplementary Materials for the paper",
                   em("Re-evaluating phoneme frequencies"), "Please cite:"),
                 br(),
                 strong("[Citation details TBC]"),
                 br(),
                 p("For further details on the underlying data, maximum likelihood methods
                   used to fit distributions and general background on phoneme frequencies,
                   please see the main paper."),
                 p("See S5 for bibliographic details for original wordlist sources.
                   Please cite both this paper", em("and"), "original data sources if using
                   frequency data."))
      )
    )
  )
)

# Server logic required to run app
server = function(input, output) {

  ### DATA GATHERING ###

  # Get df of segment frequencies in input language
  get_freqs <- reactive({
    df <- filter(phonfreq::aus_frequencies, variety_name == input$select_language)
    df$rank <- rank(desc(df$count), ties.method = "random")
    df
  })

  # Get lex_ID of input language (returns character object) (helps select distribution objects)
  get_id <- reactive({
    id_df <- filter(phonfreq::aus_metadata, variety_name == input$select_language)
    id <- as.character(id_df$lex_ID)
    id
  })

  # Get distribution objects for input language
  dist_pl         <- reactive({ phonfreq::aus_displ[[get_id()]] })
  dist_lnorm      <- reactive({ phonfreq::aus_dislnorm[[get_id()]] })
  dist_exp        <- reactive({ phonfreq::aus_disexp[[get_id()]] })
  dist_pois       <- reactive({ phonfreq::aus_dispois[[get_id()]]})

  dist_pl_xmin    <- reactive({ phonfreq::aus_displ_xmin[[get_id()]] })
  dist_lnorm_xmin <- reactive({ phonfreq::aus_dislnorm_xmin[[get_id()]] })
  dist_exp_xmin   <- reactive({ phonfreq::aus_disexp_xmin[[get_id()]] })
  dist_pois_xmin  <- reactive({ phonfreq::aus_dispois_xmin[[get_id()]]})

  # Get bootstrap objects for input language
  dist_pl_bs         <- reactive({ phonfreq::aus_bootstrap_pl[[get_id()]] })
  dist_lnorm_bs      <- reactive({ phonfreq::aus_bootstrap_lnorm[[get_id()]] })
  dist_exp_bs        <- reactive({ phonfreq::aus_bootstrap_exp[[get_id()]] })
  dist_pois_bs       <- reactive({ phonfreq::aus_bootstrap_pois[[get_id()]]})

  dist_pl_xmin_bs    <- reactive({ phonfreq::aus_bootstrap_pl_xmin[[get_id()]] })
  dist_lnorm_xmin_bs <- reactive({ phonfreq::aus_bootstrap_lnorm_xmin[[get_id()]] })
  dist_exp_xmin_bs   <- reactive({ phonfreq::aus_bootstrap_exp_xmin[[get_id()]] })
  dist_pois_xmin_bs  <- reactive({ phonfreq::aus_bootstrap_pois_xmin[[get_id()]]})

  # Get fitted distribution lines for input language
  #   Returns 100x5 data frame.
  #   First row: sequence of length 100, from xmin to xmax
  #   Other rows: gives P(X > x) for given value, for each fitted distribution
  all_lines <- reactive({
    df       <- lines(dist_pl(), draw = FALSE) %>% dplyr::dplyr::select(freq = x, pl = y)
    df$lnorm <- lines(dist_lnorm(), draw = FALSE)$y
    df$exp   <- lines(dist_exp(), draw = FALSE)$y
    df$pois  <- lines(dist_pois(), draw = FALSE)$y
    df
  })

  # Get list of fitted distribution lines for input language
  # Have to return list where each element is a separate df because xmin differs between distributions
  all_lines_xmin <- reactive({
    list(pl    = lines(dist_pl_xmin(), draw = FALSE) %>% dplyr::select(freq = x, pl = y),
         lnorm = lines(dist_lnorm_xmin(), draw = FALSE) %>% dplyr::select(freq = x, lnorm = y),
         exp   = lines(dist_exp_xmin(), draw = FALSE) %>% dplyr::select(freq = x, exp = y),
         pois  = lines(dist_pois_xmin(), draw = FALSE) %>% dplyr::select(freq = x, pois = y))
  })

  # Generate main section header
  freq_header <- reactive({ paste(input$select_language, "phoneme frequencies") })
  output$frequencies_header <- renderText({
    freq_header()
  })

  ### PLOTTING ####

  # Histogram of phoneme frequencies in input language
  freq_plot <- reactive({
    ggplot(get_freqs(),
           aes(x = reorder(match,-count), y = freq)) +
      geom_col(fill = "tomato3") +
      xlab(paste(input$select_language, "phonemes")) +
      ylab("Phoneme frequency") +
      theme_cowplot()
  })

  # Phoneme frequencies with log-scaled x and y axes.
  loglog_plot <- reactive({
    # Generate base loglog plot
    ll_plot <- ggplot(get_freqs(), aes(x = rank, y = freq)) +
                geom_point(size = 1.5) +
                xlab("Freqency rank (log)") +
                ylab("Phoneme frequency (log)") +
                scale_x_log10() +
                scale_y_log10() +
                theme_cowplot()
    # Optionally add lm
    if (input$select_lm) {
      ll_plot <- ll_plot + geom_smooth(method = "lm", colour = "#1f78b4", fill = "#a6cee3", alpha = 0.2, size = 0.5)
    }
    # Optionally add phoneme labels
    if (input$select_labels) {
      ll_plot <- ll_plot + geom_text_repel(aes(label = match), nudge_x = .03, nudge_y = .03, size = 5, segment.alpha = 0.5)
    }
    ll_plot
  })

  output$frequency_plot <- renderPlot({
    cowplot;;plot_grid(freq_plot(), loglog_plot(), labels = NULL)
  })

  # Plot theoretical vs empirical CDF
  cdf_plot <- reactive({
    obs <- plot(dist_pl(), draw = F)
    colnames(obs) <- c("freq", "p")
    cdf <- ggplot(all_lines(), aes(y = freq)) +
      geom_point(data = obs, aes(x = p, y = freq), size = 1.5) +
      xlab("P(X ≥ x)") +
      ylab("Phoneme frequency") +
      scale_colour_manual(name = "Distributions", values = c("Power law" = "#6a3d9a",
                                                             "Lognormal" = "#a6cee3",
                                                             "Exponential" = "#fb9a99",
                                                             "Poisson" = "#33a02c")) +
      scale_linetype_manual(name = "Distributions", values = c("Power law" = "solid",
                                                               "Lognormal" = "twodash",
                                                               "Exponential" = "longdash",
                                                               "Poisson" = "dotted")) +
      theme_cowplot()

    if (input$select_xmin) {
      if ("pl" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(pl, colour = "Power law", linetype = "Power law"), data = all_lines_xmin()$pl,
                               size = 1, alpha = 0.8) }
      if ("ln" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(lnorm, colour = "Lognormal", linetype = "Lognormal"), data = all_lines_xmin()$lnorm,
                               size = 1, alpha = 0.8) }
      if ("exp" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(exp, colour = "Exponential", linetype = "Exponential"), data = all_lines_xmin()$exp,
                               size = 1, alpha = 0.8) }
      if ("pois" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(pois + 0.0001, colour = "Poisson", linetype = "Poisson"), data = all_lines_xmin()$pois,
                               size = 1, alpha = 0.8) }
    } else if (input$select_xmin == FALSE) {
      if ("pl" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(pl, colour = "Power law", linetype = "Power law"), data = all_lines(),
                               size = 1, alpha = 0.8) }
      if ("ln" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(lnorm, colour = "Lognormal", linetype = "Lognormal"), data = all_lines(),
                               size = 1, alpha = 0.8) }
      if ("exp" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(exp, colour = "Exponential", linetype = "Exponential"), data = all_lines(),
                               size = 1, alpha = 0.8) }
      if ("pois" %in% input$select_lines)
      { cdf <- cdf + geom_line(aes(pois + 0.0001, colour = "Poisson", linetype = "Poisson"), data = all_lines(),
                               size = 1, alpha = 0.8) }
    }
  if (input$log_scale) {
    cdf <- cdf +
      coord_trans(x = "log", y = "log") +
      scale_x_continuous(breaks = c(0.05, 0.1, 0.25, 0.5, 0.75, 1),
                         limits = c(0.04,1.1))
  } else {
    cdf <- cdf + scale_x_continuous(breaks = c(0.25, 0.5, 0.75, 1),
                                    limits = c(0,1))
  }
    cdf
  })

  output$cdf_plot <- renderPlot({ cdf_plot() })

  # Plot parameter uncertainty estimates via bootstrapping
  output$bs_pl <- renderPlot({
    plot(dist_pl_bs())
  })

  output$bs_lnorm <- renderPlot({
    plot(dist_lnorm_bs())
  })

  output$bs_exp <- renderPlot({
    plot(dist_exp_bs())
  })

  output$bs_pois <- renderPlot({
    plot(dist_pois_bs())
  })

  output$bs_pl_xmin <- renderPlot({
    plot(dist_pl_xmin_bs())
  })

  output$bs_lnorm_xmin <- renderPlot({
    plot(dist_lnorm_xmin_bs())
  })

  output$bs_exp_xmin <- renderPlot({
    plot(dist_exp_xmin_bs())
  })

  output$bs_pois_xmin <- renderPlot({
    plot(dist_pois_xmin_bs())
  })

}

# Run the application
shinyApp(ui = ui, server = server)