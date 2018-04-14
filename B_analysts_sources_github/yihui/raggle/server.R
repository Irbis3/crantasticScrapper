library(shiny)

config = './config/' # path to configuration files
testLabels = read.csv(file.path(config, 'TestLabels'))
testLabels = testLabels[order(testLabels$id), ] # order by id
password = readLines(file.path(config, 'password'))
leaderFile = file.path(config, 'leader')
classerr = file.path(config, 'classerror')

print_leader = function() {
  cat('Leader Board (ordered by the best attempt of each group):\n\n')
  leader = read.csv(leaderFile)
  idx = order(apply(leader, 1, function(x) {
    if (all(is.na(x))) return(1)
    min(x, na.rm = TRUE)
  }))
  print(leader[idx, ], digits = 4)

  cat('\n\nBest Error Rates by Genre:\n\n')
  classerr = read.csv(classerr)
  print(classerr, digits = 4, row.names = FALSE)
}
shinyServer(function(input, output) {

  output$howdy = renderText({
    if (input$group == '0') return()
    attempts = sum(is.na(as.matrix(read.csv(leaderFile))[as.integer(input$group), ]))
    sprintf('Howdy, Group %s (%s attempts left)', input$group, attempts)
  })

  output$results = renderPrint({
    if (file.exists('00LOCK'))
      stop('Another group is submitting their predictions.',
           'Please wait a second, refresh this page and retry.')
    group = as.integer(input$group)
    if (group == 0) return(print_leader())
    if (password[group] != input$pass) return(cat('Please input your password'))
    file.create('00LOCK')  # lock the system temporarily
    on.exit(file.remove('00LOCK'))
    leader = as.matrix(read.csv(leaderFile))
    g = leader[group, ]
    na = which(is.na(g))
    if (length(na) == 0L) {
      cat('Sorry but you have used all the attempts. Take a rest now.\n\n')
      return(print_leader())
    }
    if (is.null(input$rdata)) return(cat('Please upload your prediction file'))
    # save a copy of the upload
    file.copy(input$rdata$datapath, overwrite = TRUE,
              file.path(config, sprintf('Group%dAttempt%d.txt', group, ncol(leader) - length(na) + 1)))
    pred = read.csv(input$rdata$datapath)
    if (!identical(colnames(pred), c('id', 'genre')))
      stop('the column names of your prediction file must be "id" and "genre"')
    if (nrow(pred) != nrow(testLabels))
      stop('the length of labels is wrong; you are supposed to upload ',
           nrow(testLabels), ' labels')
    if (any(duplicated(pred$id)))
      stop("there are duplicate id's in your file: ", paste(pred$id[duplicated(pred$id)], collapse = ', '))
    pred = pred[order(pred$id), ]
    if (!all(pred$id == testLabels$id))
      stop("some id's are not in the test set: ", paste(setdiff(pred$id, testLabels$id), collapse = ', '))
    if (!identical(levels(pred$genre), levels(testLabels$genre)))
      stop('your predictions must only contain these labels: ',
      paste(levels(testLabels$genre), collapse = ', '))
    res = table(testLabels$genre, pred$genre, dnn = c('True Labels', 'Prediction'))
    cat('Confusion Matrix\n\n')
    print(res)
    cat('\nPrediction Errors by Class\n\n')
    res = as.matrix(res)
    err = 1 - diag(res) / c(table(testLabels$genre))
    print(err)
    minclasserr = as.matrix(read.csv(classerr))
    for (i in 1:5)
      if (err[i] < minclasserr[i,2]) {
        minclasserr[i,2] = err[i]
        minclasserr[i,3] = group
      }
    write.csv(minclasserr, classerr, row.names = FALSE)
    err = mean(err)
    cat('\nAverage Prediction Error: ', err, '\n\n')
    leader = as.matrix(read.csv(leaderFile))
    leader[group, na[1]] = err
    write.csv(leader, leaderFile, row.names = FALSE)
    print_leader()
  })

})
