edebug = idebug =
function(expr, env = globalenv())
{
     #  freeze the expression
    e = substitute(expr)

     # Find the function that is being called and call debug() with that.
    eval(substitute(debug(x), list(x = e[[1]])), env)

     # Arrange to undebug() that function when we return.
    x = substitute(undebug(x), list(x = e[[1]]))
    on.exit(eval(x))

      # Now evaluate the expression
    eval(expr, env)
}



