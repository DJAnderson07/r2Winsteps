#' Plot the test characteristic curve
#' @param ob Objects of class batchRunWinsteps
#' @param theta Theta values from which the information function values are 
#' calculated from. Defaults to a sequence from -4 to 4 by 0.1. The limits of 
#' this range are used for the x-axes.
#' @param colors Line colors for the plot. Defaults to NULL, in which case the
#' \code{rainbow} function is used to create colors.
#' @param store Optional logical argument to return data used in plotting.
#' @param ... Additional arguments passed to \code{\link{plot}}.

plot.batchRunWinsteps <- function(ob, theta = seq(-4, 4, 0.1), colors = NULL,
	store = FALSE, ...) {

	args <- as.list(match.call())
	
	b <- lapply(ob, function(x) x$ItemParameters$Difficulty)
	
		
	prob <- function(b, theta) {
		1 /(1 + exp(-(theta - b)))
	}
	p <- lapply(b, function(x) {
			sapply(x, prob, theta)
	})
	q <- sapply(p, function(x) 1 - x)
	IIF <- sapply(seq_along(p), function(i) p[[i]]*q[[i]])

	expectedTotal <- sapply(p, function(x) rowSums(x))
		
	if(is.null(colors)) {
		colors <- rainbow(ncol(expectedTotal))
	}	

	m <- matrix(c(rep(1, 8), 2, 2), ncol = 10)
	layout(m)
	par(mar = c(5.1, 4.1, 4.1, 0))
	plot(theta, seq(0, max(expectedTotal), length.out = length(theta)), 
		type = "n",
		xlab = expression(Theta),
		ylab = "Expected Total Raw Score",
		main = "",
		...)
	title("Test Characteristic Curve", outer = TRUE, line = -3)
	invisible(sapply(1:ncol(expectedTotal), function(i) {
		lines(theta, expectedTotal[ ,i], col = colors[i])	
	}))
	par(mar = c(5.1, 0, 4.1, 0))
	plot(rep(0, length(expectedTotal)), expectedTotal, 
		type = "n", 
		axes = FALSE,
		ann = FALSE)	
	legend("topleft",
		legend = colnames(expectedTotal), lwd = 1, col = colors, box.lwd = 0)	
	if(store == TRUE) {
		return(expectedTotal)
	}
}
