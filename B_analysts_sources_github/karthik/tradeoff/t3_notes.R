# tradeoffs that work in td3
# -----------------------------

# This function grabs the parameter combinations that worked. A quick way for me to see what worked and what didn't so I can elimiate the ones that don't work and cut down on computation time.
working_sims <- ldply(t3_simple, function(x) {
	if(!is.null(x$data)) {
			if(x$data[10,3]>x$data[20,3]) {
			  return(data.frame(x$params))
			}
		}
	})





