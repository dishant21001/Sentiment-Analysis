# Load the CSV file
feedback_data <- read.csv("D:/iphone.csv", stringsAsFactors = FALSE)

# Display the first few rows to understand its structure
head(feedback_data)

# Check the structure of the dataset
str(feedback_data)

# Check summary statistics of each column
summary(feedback_data)

# Check for any missing values in the dataset
colSums(is.na(feedback_data)
        
        # Display column names
        colnames(feedback_data)
        
        # Select relevant columns
        filtered_data <- feedback_data[, c("reviewDescription", "ratingScore", "reviewTitle", "country", "isVerified")]
        
        # Rename columns for easier handling
        colnames(filtered_data) <- c("ReviewText", "Rating", "Title", "Country", "IsVerified")
        
        # Install required packages
        install.packages("tm")
        install.packages("tokenizers")
        
        # Load necessary libraries for text processing
        library(tm)         # For text cleaning
        library(tokenizers) # For tokenization
        
        # Convert to lowercase
        filtered_data$CleanedText <- tolower(filtered_data$ReviewText)
        
        # Remove punctuation
        filtered_data$CleanedText <- removePunctuation(filtered_data$CleanedText)
        
        # Remove stop words
        filtered_data$CleanedText <- removeWords(filtered_data$CleanedText, stopwords("en"))
        
        # Tokenize the text
        filtered_data$Tokens <- tokenize_words(filtered_data$CleanedText)
        
        # Install the syuzhet package if not already installed
        install.packages("syuzhet")
        
        # Load the syuzhet library
        library(syuzhet)
        
        # Calculate sentiment scores for each review
        filtered_data$SentimentScore <- get_sentiment(filtered_data$CleanedText, method = "syuzhet")
        
        # sentiment scores alongside the review text
        head(filtered_data[, c("ReviewText", "SentimentScore")])
        
        # Filter for negative reviews based on sentiment score
        negative_reviews <- filtered_data[filtered_data$SentimentScore < 0, ]
        
        # View a sample of the negative reviews
        head(negative_reviews[, c("ReviewText", "SentimentScore")])
        
        # Combine all negative reviews into a single text for frequency analysis
        negative_text <- paste(negative_reviews$ReviewText, collapse = " ")
        
        # Tokenize and analyze the frequency of words
        library(dplyr)
        
        # Split the text into individual words
        negative_words <- unlist(strsplit(negative_text, " "))
        
        # Convert to a data frame to count word frequency
        word_freq <- as.data.frame(table(negative_words))
        
        # Filter out short words and common stop words for clearer themes
        word_freq <- word_freq %>% 
          filter(nchar(as.character(negative_words)) > 3) %>%  # Keep words longer than 3 characters
          arrange(desc(Freq))                                  # Sort by frequency
        
        # Display top 10 frequent words to identify potential themes
        head(word_freq, 10)
        
        # Load necessary library for visualization
        install.packages("ggplot2")
        library(ggplot2)
        
        # Plot sentiment score distribution with lighter colors for black background
        ggplot(filtered_data, aes(x = SentimentScore)) +
          geom_histogram(binwidth = 0.5, fill = "lightblue", color = "white") +
          labs(title = "Distribution of Sentiment Scores", x = "Sentiment Score", y = "Count") +
          theme_minimal()
        
        # Convert all words to lowercase to ensure consistency
        word_freq$negative_words <- tolower(word_freq$negative_words)
        
        # Define an extended list of custom stop words
        custom_stopwords <- c("phone", "this", "that", "with", "have", "very", "they", 
                              "from", "iphone", "amazon", "only", "product", "even", 
                              "will", "bought", "just", "apple", "when", "time", 
                              "after", "also", "there", "than")
        
        # Filter out custom stop words from word frequency data
        word_freq_filtered <- word_freq %>%
          filter(!(negative_words %in% custom_stopwords)) %>%
          arrange(desc(Freq))
        
        # Display the top 10 refined words to identify potential themes after further refinement
        head(word_freq_filtered, 10)
        
        # Word cloud for the refined frequent words
        install.packages("wordcloud")
        library(wordcloud)
        
        wordcloud(words = word_freq_filtered$negative_words, freq = word_freq_filtered$Freq, max.words = 50, colors = brewer.pal(8, "Set2"))
        
        # Filter the data to include only the top specific complaints
        top_issues <- word_freq_filtered %>%
          filter(negative_words %in% c("battery", "service", "camera", "screen", "quality"))
        
        # Create the bar chart
        ggplot(top_issues, aes(x = reorder(negative_words, -Freq), y = Freq)) +
          geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
          labs(title = "Frequency of Top Specific Complaints", x = "Issue", y = "Frequency") +
          theme_minimal()
        
        # Calculate average sentiment score by top issue
        top_issues_sentiment <- negative_reviews %>%
          filter(grepl("battery|service|camera|screen|quality", CleanedText)) %>%
          mutate(Issue = ifelse(grepl("battery", CleanedText), "Battery",
                                ifelse(grepl("service", CleanedText), "Service",
                                       ifelse(grepl("camera", CleanedText), "Camera",
                                              ifelse(grepl("screen", CleanedText), "Screen", "Quality"))))) %>%
          group_by(Issue) %>%
          summarise(Average_Sentiment = mean(SentimentScore))
        
        # Plot the average sentiment for each issue
        ggplot(top_issues_sentiment, aes(x = Issue, y = Average_Sentiment)) +
          geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
          labs(title = "Average Sentiment Score by Top Issues", x = "Issue", y = "Average Sentiment Score") +
          theme_minimal()
        
        # Count frequency of top issues by country
        issues_by_country <- negative_reviews %>%
          filter(grepl("battery|service|camera|screen|quality", CleanedText)) %>%
          mutate(Issue = ifelse(grepl("battery", CleanedText), "Battery",
                                ifelse(grepl("service", CleanedText), "Service",
                                       ifelse(grepl("camera", CleanedText), "Camera",
                                              ifelse(grepl("screen", CleanedText), "Screen", "Quality"))))) %>%
          count(Country, Issue) 
        
        # Plot the heatmap
        ggplot(issues_by_country, aes(x = Country, y = Issue, fill = n)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "red") +
          labs(title = "Heatmap of Issue Mentions by Country", x = "Country", y = "Issue", fill = "Count") +
          theme_minimal()
        