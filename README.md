library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(dplyr)

sample_path <- file.path("data", "sample_reviews.txt")

load_text <- function(file_path = NULL, raw_text = NULL) {
  if (!is.null(raw_text) && nzchar(trimws(raw_text))) {
    text_lines <- strsplit(raw_text, "\n")[[1]]
    return(text_lines[nzchar(trimws(text_lines))])
  }

  if (!is.null(file_path) && file.exists(file_path)) {
    txt <- suppressWarnings(readLines(file_path, warn = FALSE))
    txt <- txt[nzchar(trimws(txt))]
    return(txt)
  }

  if (file.exists(sample_path)) {
    txt <- suppressWarnings(readLines(sample_path, warn = FALSE))
    txt <- txt[nzchar(trimws(txt))]
    return(txt)
  }

  stop("No text available. Please upload a file or paste text into the dashboard.")
}

preprocess_text <- function(text_lines) {
  corpus <- Corpus(VectorSource(text_lines))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removeWords, c("s", "company", "team", "product", "app"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  corpus
}

summarize_words <- function(corpus) {
  dtm <- TermDocumentMatrix(corpus)
  matrix_data <- as.matrix(dtm)
  word_counts <- sort(rowSums(matrix_data), decreasing = TRUE)
  words_df <- data.frame(
    word = names(word_counts),
    frequency = as.numeric(word_counts),
    stringsAsFactors = FALSE
  )

  list(dtm = dtm, matrix_data = matrix_data, words_df = words_df)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("""
      body { background: #f7f9fc; }
      .well { background: white; border: 1px solid #dfe6ee; }
      .panel { border-radius: 12px; }
      .stat-box {
        background: linear-gradient(135deg, #0B5FFF, #4A90E2);
        color: #fff;
        border-radius: 12px;
        padding: 18px;
        margin: 8px 0;
        min-height: 100px;
      }
      .stat-value { font-size: 28px; font-weight: 700; }
      .stat-label { font-size: 14px; opacity: 0.9; }
      .panel-title { font-weight: 600; }
    """))
  ),

  titlePanel("Sentiment Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        "source_mode",
        "Input source",
        choices = c(
          "Sample data" = "sample",
          "Upload file" = "upload",
          "Paste text" = "manual"
        ),
        selected = "sample"
      ),

      conditionalPanel(
        condition = "input.source_mode == 'upload'",
        fileInput(
          "file1",
          "Choose a .txt, .csv, or .tsv file",
          accept = c(".txt", ".csv", ".tsv")
        )
      ),

      conditionalPanel(
        condition = "input.source_mode == 'manual'",
        textAreaInput(
          "manual_text",
          "Paste text here",
          value = "",
          height = "200px"
        )
      ),

      sliderInput(
        "min_freq",
        "Minimum word frequency",
        min = 1,
        max = 15,
        value = 2
      ),

      selectInput(
        "sentiment_method",
        "Sentiment lexicon",
        choices = c(
          "Syuzhet" = "syuzhet",
          "Bing" = "bing",
          "Afinn" = "afinn"
        ),
        selected = "syuzhet"
      ),

      actionButton("analyze_btn", "Analyze", class = "btn-primary")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overview",
          uiOutput("summary_cards"),
          fluidRow(
            column(
              6,
              plotOutput("sentiment_distribution", height = "300px")
            ),
            column(
              6,
              plotOutput("emotion_plot", height = "300px")
            )
          ),
          tableOutput("sentiment_summary_table")
        ),

        tabPanel(
          "Word Cloud",
          plotOutput("wordcloud_plot", height = "600px")
        ),

        tabPanel(
          "Top Words",
          tableOutput("top_words_table")
        ),

        tabPanel(
          "Insights",
          verbatimTextOutput("analysis_insights")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- eventReactive(input$analyze_btn, {
    text_lines <- NULL

    if (input$source_mode == "sample") {
      text_lines <- load_text(sample_path)
    } else if (input$source_mode == "upload") {
      req(input$file1)
      uploaded_path <- input$file1$datapath
      ext <- tolower(tools::file_ext(input$file1$name))

      if (ext %in% c("csv", "tsv")) {
        sep_value <- if (ext == "csv") "," else "\t"
        df <- read.csv(uploaded_path, sep = sep_value, stringsAsFactors = FALSE)
        text_lines <- unlist(df[sapply(df, is.character)])
      } else {
        text_lines <- readLines(uploaded_path, warn = FALSE)
      }
    } else {
      text_lines <- load_text(raw_text = input$manual_text)
    }

    text_lines <- text_lines[nzchar(trimws(text_lines))]
    req(length(text_lines) > 0)

    corpus <- preprocess_text(text_lines)
    words_summary <- summarize_words(corpus)

    sentiment_vector <- get_sentiment(text_lines, method = input$sentiment_method)
    nrc_data <- get_nrc_sentiment(text_lines)

    emotion_summary <- as.data.frame(colSums(nrc_data))
    names(emotion_summary) <- "count"
    emotion_summary$sentiment <- rownames(emotion_summary)
    emotion_summary <- emotion_summary %>%
      select(sentiment, count) %>%
      arrange(desc(count))

    list(
      text_lines = text_lines,
      corpus = corpus,
      words_summary = words_summary,
      sentiment_vector = sentiment_vector,
      nrc_data = nrc_data,
      emotion_summary = emotion_summary
    )
  })

  output$summary_cards <- renderUI({
    data <- dataset()
    total_words <- sum(lengths(strsplit(data$text_lines, "\\s+")))
    unique_words <- nrow(data$words_summary$words_df)
    avg_words <- if (length(data$text_lines) > 0) mean(lengths(strsplit(data$text_lines, "\\s+"))) else 0
    avg_sentiment <- mean(data$sentiment_vector)

    div(
      fluidRow(
        column(4, div(class = "stat-box", div(class = "stat-value", total_words), div(class = "stat-label", "Total words"))),
        column(4, div(class = "stat-box", div(class = "stat-value", unique_words), div(class = "stat-label", "Unique words"))),
        column(4, div(class = "stat-box", div(class = "stat-value", round(avg_sentiment, 2)), div(class = "stat-label", "Average sentiment score")))
      )
    )
  })

  output$sentiment_distribution <- renderPlot({
    data <- dataset()
    sentiment_values <- data$sentiment_vector

    hist(
      sentiment_values,
      breaks = 20,
      col = "steelblue",
      border = "white",
      main = "Sentiment score distribution",
      xlab = "Sentiment score",
      ylab = "Frequency"
    )
  })

  output$emotion_plot <- renderPlot({
    data <- dataset()
    ggplot(data$emotion_summary, aes(x = reorder(sentiment, count), y = count, fill = sentiment)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Emotion counts",
        x = "Emotion",
        y = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$wordcloud_plot <- renderPlot({
    data <- dataset()
    words_df <- data$words_summary$words_df
    filtered <- words_df %>% filter(frequency >= input$min_freq)

    set.seed(123)
    wordcloud(
      words = filtered$word,
      freq = filtered$frequency,
      min.freq = 1,
      max.words = 100,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
  })

  output$top_words_table <- renderTable({
    data <- dataset()
    words_df <- data$words_summary$words_df
    head(words_df %>% filter(frequency >= input$min_freq), 15)
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$sentiment_summary_table <- renderTable({
    data <- dataset()
    summary_df <- data.frame(
      Metric = c("Mean", "Median", "Min", "Max"),
      Value = c(
        round(mean(data$sentiment_vector), 2),
        round(median(data$sentiment_vector), 2),
        round(min(data$sentiment_vector), 2),
        round(max(data$sentiment_vector), 2)
      )
    )
    summary_df
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$analysis_insights <- renderText({
    data <- dataset()
    words_df <- data$words_summary$words_df
    top_terms <- head(words_df$word, 5)
    avg_score <- round(mean(data$sentiment_vector), 2)
    dominant <- if (avg_score > 0) "positive" else if (avg_score < 0) "negative" else "neutral"

    paste(
      "Top recurring terms:", paste(top_terms, collapse = ", "), "\n",
      "\nOverall sentiment trend:", dominant, "\n",
      "Average sentiment score:", avg_score
    )
  })
}

shinyApp(ui, server)
