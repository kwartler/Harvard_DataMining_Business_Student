#' Auto Breiman and Cutler Random Forest Classifier
#' TK
#' Nov 7, 2024

modelfit <- function(response, data, extras) {
  # Fits a model using the randomForest library
  #
  # Args:
  #   response: a vector containing the response values (classification problem)
  #   data: a data frame containing the training data
  #   extras: a list containing any extras to be passsed
  #
  # Returns:
  #   model: a list containing all state information needed to
  #          predict on  new data


  #  Here we initialize some variables to control the fit

  # Any categorical variables with cardinality higher than CARD_MAX will be encoded
  # as numeric rather than as a factor level
  CARD_MAX <- 12

  # Control the number of trees used to build the forest
  if (nrow(data) < 10000) {
    NTREES <- 350
  } else {
    NTREES <- 250
  }

  # Control the mininum leaf size
  if (nrow(data) < 20000) {
    NODESIZE <- 2
  } else {
    NODESIZE <- 3
  }

  # Any categorical variable that occurs fewer than SUPPORT_MIN times will be
  # grouped together in a category by themselves
  SUPPORT_MIN <- max(5,nrow(data)/1000)

  # Set the random number generator seed
  set.seed(543543)

  # Record the time used for preprocessing for now
  start_time <- proc.time()

  # Load the data.table package
  library(data.table)

  # Load the matrix package for sparse model matrix
  library(Matrix)

  # Load the randomForest package which implements the Breiman and
  # Cutler randomForest
  library(randomForest)

  ################################################
  # Data checks
  ################################################

  start_time <- proc.time()
  data <- data.table(data)

  # For binary classification, we want to encode the
  # target as a factor level variable to ensure that we do classification
  response <- factor(as.character(response))

  # Clean names
  setnames(data, make.names(names(data)))

  # Remove NA targets
  data <- data[!is.na(response),]
  response <- response[!is.na(response)]

  # Randomly permute the rows since we might have all of one class at the end
  # and to ensure that the validation set on the number of trees to use
  # is representative

  # Checks
  if(!length(response) == nrow(data)){
    err <- paste0('length(response) [', length(response), ']')
    err <- paste0(err, ' and nrow(data) [', nrow(data), ']')
    err <- paste0(err, ' do not match')
    stop(err)
  }

  # Set the random seed
  set.seed(904384)
  new_rows <- sample(length(response))
  data[,my_new_column_order_308804847902 := new_rows]
  setorder(data, my_new_column_order_308804847902)
  data[,my_new_column_order_308804847902 := NULL]
  response <- response[order(new_rows)]

  # logicals to integer
  logical_cols <- names(which(sapply(data, FUN=is.logical)))
  for(col in logical_cols){
    set(data, j=col, value=as.integer(data[[col]]))
  }

  # Separate character / numeric varaibles
  mchar_cols <- names(which(sapply(data, FUN=is.character) | sapply(data, FUN=is.factor)))
  mnum_cols <- setdiff(names(data), mchar_cols)

  # Remove constant columns
  uniques <- sapply(data[, mnum_cols, with=FALSE], function(x) length(unique(x)))
  remove <- names(uniques)[uniques <= 1]
  mchar_cols <- setdiff(mchar_cols, remove)
  mnum_cols <- setdiff(mnum_cols, remove)

  # Error if no unique columns
  if(length(mchar_cols) == 0 & length(mnum_cols) == 0){
    stop('All columns in the data set are constant-valued.  No modeling can be done.')
  }

  ################################################
  # Categorical variable preprocessing
  ################################################

  # Encode character variables as factors
  #     - Sort factor levels by frequency
  #     - Remove factors that are mostly one level or mostly different levels
  if(length(mchar_cols) > 0){

    #Frequency tables for all categorical vars
    freq_tables <- lapply(mchar_cols, function(x){
      freq <- table(data[[x]])
      freq <- sort(freq, decreasing=TRUE)
      return(freq)
    })
    names(freq_tables) <- mchar_cols

    # Convert to factors, order by frequency
    # Include NA level, which will be used for new factors
    for(col in mchar_cols){
      freq <- freq_tables[[col]]
      x <- factor(data[[col]], levels=names(freq))
      x <- addNA(x, ifany = FALSE)
      set(data, j=col, value = x)
    }

    #Remove factors with all one level or mostly different levels
    col_levels <- sapply(data[,mchar_cols,with=FALSE], FUN=nlevels)
    mchar_cols <- names(which((col_levels > 1) & (col_levels < (0.95 * nrow(data)))))
  } else{
    col_levels <- list()
    freq_tables <- list()
  }

  # Combine low support levels for high-cardinality factors
  #     - Combine low support levels into a new "low support" level
  #         - No need to re-level if no columns is low support
  #         - Also no need to re-level if only one column is low support
  if(length(mchar_cols) > 0){
    mchar_cols_high_card <- names(which(col_levels >= CARD_MAX))
    mchar_cols_low_card  <- setdiff(mchar_cols, mchar_cols_high_card)
    low_support <- lapply(freq_tables, function(x) x < SUPPORT_MIN)
    for(col in mchar_cols_high_card){
      if(sum(low_support[[col]]) > 1){
        x <- data[[col]]
        levels(x)[low_support[[col]]] <- '--Low Support--'
        x <- addNA(x, ifany=FALSE)
        set(data, j=col, value=x)
      }
    }
  } else{
    mchar_cols_high_card <- vector()
    mchar_cols_low_card <- vector()
    low_support <- list()
  }

  # Identify final factor levels
  if(length(mchar_cols) > 0){
    factor_levels <- lapply(data[,mchar_cols,with=FALSE], levels)
  } else{
    factor_levels <- list()
  }

  # Convert high card into numeric, we leave low-cardinality as factor levels
  for(col in mchar_cols_high_card){
    set(data, j=col, value = as.integer(data[[col]]))
  }

  ################################################
  # Numeric variable preprocessing
  ################################################

  # Check that we have at least one numeric variale
  if (length(mnum_cols) > 0) {

    # Replace missing values with a flag
    mmins <- sapply(data[,mnum_cols,with=FALSE], FUN=min, na.rm=TRUE)
    mmins <- as.numeric(mmins)
    mmins <- mmins - 9999
    mmins <- mapply(min, mmins, -9999)
    names(mmins) <- mnum_cols

    # Loop over the numeric columns
    for(col in mnum_cols) {
      #Remove all missing columns
      if (is.na(mmins[col])) {
        set(data, j=col, value=NULL)
      } else if(anyNA(data[[col]])) {
        indicator <- is.na(data[[col]])
        set(data, i=which(indicator), j=col, value=mmins[col])
      }
    }

  } else{
    mmins <- list()
  }

  mnum_cols <- intersect(names(data), mnum_cols)
  mchar_cols <- union(mchar_cols_low_card, mchar_cols_high_card)

  ################################################
  # Model Setup
  ################################################

  # 1-hot encode categorical variables
  modelvars <- c(mchar_cols_low_card, mchar_cols_high_card, mnum_cols)
  data2 <- sparse.model.matrix(~ . - 1, data=data[,modelvars,with=FALSE])
  data2 <- as.matrix(data2)
  rm(data)
  sink <- gc(reset=TRUE)
  final_cols <- colnames(data2)

  print("Preprocessing time:")
  preproc_time <- proc.time() - start_time
  print(preproc_time)

  ################################################
  # Model fitting
  ################################################

  # Choose MTRY
  MTRY <- min(floor(sqrt(ncol(data2))), 15)

  # Fit the model
  start_time <- proc.time()
  mrf <- randomForest(
    x=data2,
    y=response,
    ntree=NTREES,
    nodesize=NODESIZE,
    mtry=MTRY,
    sampsize=min(5000, ceiling(.632*nrow(data2))),
    replace=FALSE,
    importance=FALSE,
    localImp=FALSE,
    proximity=FALSE,
    keep.inbag=FALSE
  )
  fit_time <- proc.time() - start_time

  # Store the model and the information needed for preprocessing at predict time
  model <- list(
    rf=mrf,
    mmins=mmins,
    logical_cols = logical_cols,
    mchar_cols = mchar_cols,
    mnum_cols = mnum_cols,
    #freq_tables = freq_tables,
    factor_levels = factor_levels,
    low_support = low_support,
    mchar_cols_high_card = mchar_cols_high_card,
    mchar_cols_low_card = mchar_cols_low_card,
    final_cols = final_cols,
    preproc_time = preproc_time,
    fit_time = fit_time,
    MTRY=MTRY,
    NTREES=NTREES,
    NODESIZE=NODESIZE,
    CARD_MAX=CARD_MAX,
    SUPPORT_MIN=SUPPORT_MIN
  )
  return(model)
};

modelpredict <- function(model, data) {
  # Function to make predictions using a fitted randomForest model
  #
  # Args:
  #   model : list
  #     Contains stored state information
  #   data : data.frame
  #     Contains data to make predictions on
  #
  # Returns:
  # predictions : vector
  #   Contains predicted values
  library(randomForest)
  library(Matrix)
  library(data.table)

  data <- data.table(data)

  # Clean names
  setnames(data, make.names(names(data)))

  # logicals to integer
  for(col in model$logical_cols){
    set(data, j=col, value=as.integer(data[[col]]))
  }

  ################################################
  # Categorical variable preprocessing
  ################################################

  # Set factor levels
  for(col in model$mchar_cols){
    levels <- model$factor_levels[[col]]
    x <- factor(data[[col]], levels = levels)
    x <- addNA(x, ifany = FALSE)

    # Encode new or missing levels as low support
    # Possible bug: what if there's a large NA level in the original data?
    #     - In this case, new NAs will be encoded as --Low Support--
    if(col %in% model$mchar_cols_high_card) {
      if("--Low Support--" %in% levels) {
        low_support_level <- which(levels(x) == '--Low Support--')
        x[is.na(as.integer(x))] <- low_support_level
      }

      #Encode high cardinality as integer
      x <- as.integer(x)
      x[is.na(x)] <- max(x, na.rm=TRUE) + 1
    }
    set(data, j=col, value=x)
  }

  ################################################
  # Numeric variable preprocessing
  ################################################

  if (length(model$mnum_cols) > 0) {
    for(col in model$mnum_cols) {
      if(anyNA(data[[col]])) {
        indicator <- is.na(data[[col]])
        set(data, i=which(indicator), j=col, value=model$mmins[col])
      }
    }
  }

  ################################################
  # Check data and predict
  ################################################

  # Stop if only constant columns left
  uniq <- sapply(data, function(x) length(unique(x)))
  if(all(uniq <= 1)){
    stop('Random Forest Regressor failed: all predictors are constant')
  }

  # 1-hot encode categorical variables
  # Could be a faster way to do this
  modelvars <- c(model$mchar_cols_low_card, model$mchar_cols_high_card, model$mnum_cols)
  data2 <- sparse.model.matrix(~ . - 1, data=data[,modelvars,with=FALSE])
  data2 <- as.matrix(data2)
  rm(data)
  sink <- gc(reset=TRUE)

  # Check for missing
  miss <- setdiff(model$final_cols, colnames(data2))
  if(length(miss) > 0){
    miss <- paste(miss, collapse=", ")
    stop(paste('The following columns are missing in the new data:', miss))
  }

  # Check for extra
  extra <- setdiff(colnames(data2), model$final_cols)
  if(length(extra) > 0){
    extra <- paste(extra, collapse=", ")
    wrn <- paste('The following columns were added to the new data:', extra)
    print(wrn)
    warning(wrn)
    data2 <- data2[,model$final_cols,drop=FALSE]
  }

  # Return predictions
  predictions <- predict(model[['rf']], newdata=data2, type="prob")[,2]
};
