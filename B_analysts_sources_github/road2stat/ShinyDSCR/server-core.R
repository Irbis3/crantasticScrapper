library('xlsx')

# A rewrite of write.xlsx2() since it has logical problems and fails the write
xn.write.xlsx = function (x, file, sheetName = "Sheet1", 
                          col.names = TRUE, row.names = TRUE) {
  wb = createWorkbook(type = 'xlsx')
  sheet = createSheet(wb, sheetName)
  addDataFrame(x, sheet, col.names = col.names, row.names = row.names, 
               startRow = 1, startColumn = 1, colStyle = NULL, colnamesStyle = NULL, 
               rownamesStyle = NULL)
  saveWorkbook(wb, file)
  invisible()
}
