
moveCars = 
function(grid, color = "red")
{
  i = row(grid)[grid != ""]
  j = col(grid)[grid != ""]
  pos = cbind(i, j)
  colors = grid[pos]
  cars = data.frame(i = i, j = j, colors = colors)

  w = which(cars$colors == color)
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = if(color == "blue")
                c(cars$i[ idx ] , 
                  if(cars$j[idx] == 1)
                    nrow(grid)
                  else
                    cars$j[idx] - 1L)
             else
                c(if(cars$i[ idx ] == ncol(grid))
                    1L
                  else
                    cars$i + 1L,
                  cars$j[idx])

        # check if nextPos is empty
    if(grid[ nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}





moveCars = 
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = getNextPosition(curPos, dim(grid), color == "red")

        # check if nextPos is empty
    if(grid[ nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}





moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  sz = dim(grid)
  horiz = color == "red"
  for(idx in w) {
    curPos = c(cars$i[ idx ], cars$j[idx])
    nextPos = getNextPosition(curPos, sz, horiz) 

        # check if nextPos is empty
    if(grid[nextPos[1], nextPos[2] ] == "")  {
       grid[nextPos[1], nextPos[2]] = color
       grid[curPos[1], curPos[2]] = ""      
    }
   }

  grid
}





moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  rows = cars$i[w]
  cols = cars$j[w]
 
  if(color == "red") {
    nextRows = rows
    nextCols = ifelse(cols == ncol(grid), 1L, cols + 1L)
  } else {
    nextRows = ifelse(rows == 1, nrow(grid), rows - 1L)
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w,] ] = color
  grid[ cbind(rows, cols)[w,] ] = ""

  grid
}





moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(cars$colors == color)
  rows = cars$i[w]
  cols = cars$j[w]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols >= ncol(grid) ]  = 1L
  } else {
    nextRows = rows - 1L
    nextRows[ nextRows == 0 ] = nrow(grid)
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w,] ] = color
  grid[ cbind(rows, cols)[w,] ] = ""

  grid
}





moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(rownames(cars) == color)
  rows = cars[w, 1]
  cols = cars[w, 2]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols >= ncol(grid) ]  = 1L
  } else {
    nextRows = rows - 1L
    nextRows[ nextRows == 0 ] = nrow(grid)
    nextCols = cols
  }

  w = grid[ cbind(nextRows, nextCols) ]  == ""
  grid[ cbind(nextRows, nextCols)[w,] ] = color
  grid[ cbind(rows, cols)[w,] ] = ""

  grid
}





moveCars =
function(grid, color = "red")
{
  cars = getCarLocations(grid)

  w = which(rownames(cars) == color)
  rows = cars[w, 1]
  cols = cars[w, 2]
 
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[ nextCols >= ncol(grid) ]  = 1L
  } else {
    nextRows = rows - 1L
    nextRows[ nextRows == 0 ] = nrow(grid)
    nextCols = cols
  }

  nextLocs = cbind(nextRows, nextCols)
  w = grid[ nextLocs ]  == ""
  grid[ nextLocs[w,] ] = color
  grid[ cbind(rows, cols)[w,] ] = ""

  grid
}

