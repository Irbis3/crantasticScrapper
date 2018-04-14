playDrop7 = function(W=NULL)
{
    game = Drop7()
    gameEnd = FALSE
    
    while (!gameEnd)
    {
        game = updateDec(game)
        bd = board(game)
        fall = num(game)
        
        if (is.null(W))
        {
            moves = validSteps(game)
            if (length(moves==1))
                tcol = moves
            else
                tcol = sample(moves,1)
        }
        else
            tcol = chooseCol(game)#The Intellegence
        
        tmp = simpleFall(game,tcol,fall)
        game = tmp[[1]]
        gameEnd = tmp[[2]]
    }
    return(score(game))
}
