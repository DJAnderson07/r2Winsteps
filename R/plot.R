#' Plot the test characteristic curve
#' @param ob Objects of class r2Winsteps
#' @param type The type of plot to produce. Valid values are "TIF", "IIFs",
#' "TIF/IIF", "TCC", "ICCs" "ICP", and "thresholds". These correspond to,
#' respectively, the test information function,  item information
#' functions, both the test and item information functions, the test
#' characteristic curve, item characteristic curves, item-category
#' probabilities, and the Thurstonion thresholds. Note that ICP and Thresholds
#' are only available for polytomous models. Defaults to "TIF".
#' @param itemSelect Items to be used in the plotting. Defaults to NULL, in
#' which case all items will be used.
#' @param colors Line colors for the plot. Defaults to NULL, in which case the
#' \code{rainbow} function is used to create colors.
#' @param theta Theta values from which the information function values are 
#' calculated from. Defaults to a sequence from -4 to 4 by 0.1. The limits of 
#' this range are used for the x-axes.
#' @param store Optional logical argument to return data used in plotting 
#' (not available for all plot types). For example, when \code{store = TRUE}
#' and \code{type = TIF}, information under the specified theta range will be
#' returned.
#' @param ... Additional arguments passed to \code{plot}

plot.r2Winsteps <- function(ob, type = "TIF", theta = seq(-4, 4, 0.1),
	itemSelect = NULL, colors = NULL, legend = TRUE,
		 store = FALSE, ...) {
	
	b <- ob$ItemParameters$Difficulty
	if(length(ob) == 3) {
		sfile <- ob$StructureFiles
		sfile <- subset(sfile, Category != 0)
	}
	names(b) <- substr(
						as.character(ob$ItemParameters$ItemID), 
						2, 
						nchar(as.character(ob$ItemParameters$ItemID)))

	if(length(ob) > 2 & !is.null(itemSelect)) {
		if(is.character(itemSelect)) {
			num <- cbind(names(b), 1:length(b))
			num <- subset(num, num[ ,1] %in% itemSelect)
			num <- as.numeric(num[ ,2])
		sfile <- subset(sfile, Item %in% num)
		}
		sfile <- subset(sfile, Item %in% itemSelect)
	}
	if(!is.null(itemSelect)) {
		b <- b[itemSelect]
	}

	if(length(ob) == 2) {
		prob <- function(b, theta) {
			1 /(1 + exp(-(theta - b)))
		}

		p <- sapply(b, prob, theta)
		q <- 1 - p
		IIF <- p*q

	colnames(IIF) <- names(b)
	colnames(p) <- names(b)
	}
	if(length(ob) == 3) {
		prob <- function(delta) exp(theta - delta) / (1 + exp(theta - delta))
		p <- mapply(prob, sfile$delta)
		q <- 1 - p
		IIF <- p*q # actually category information
		cats <- sapply(split(sfile, sfile$Item), nrow)
		
		ICCs <- matrix(rep(NA, length(theta)*length(b)), ncol = length(b))
		IIFs <- matrix(rep(NA, length(theta)*length(b)), ncol = length(b))
		for(i in 1:length(b)) {
			ICCs[ ,i] <- rowSums(p[ ,i:(i + (cats[i] - 1))])
			IIFs[ ,i] <- rowSums(IIF[ ,i:(i + (cats[i] - 1))])
		}	
		colnames(ICCs) <- names(b)
		colnames(IIFs) <- names(b)

		
	}


	cols <- colorRampPalette(c("red", "green", "blue"))
	if(is.null(colors)) {
		colors <- cols(ncol(IIF))
	}
	
	if(type == "TIF") {
		plot(theta, rowSums(IIF), 
			type = "l", 
			xlab = expression(Theta),
			ylab = "Information", 
			main = "Test Information Function",
			col = colors,
			...)
		if(store == TRUE) {
			return(rowSums(IIF))
		}
	}
	if(type == "IIFs") {
		if(legend == TRUE) {
			par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		}
		yUpperLim <- max(apply(IIF, 2, max))
		plot(theta, seq(0, yUpperLim, length.out = length(theta)),
			type = "n", 
			xlab = expression(Theta),
			ylab = "Information", 
			main = "Item Information Functions", 
			...)
		for(i in 1:ncol(IIF)) lines(theta, IIF[ ,i], col = colors[i])
		if(legend == TRUE) {
			legend("topright", 
				inset = c(-0.3, 0), 
				legend = colnames(IIF),
				col = colors,
				lty = 1)
		}
	}
	if(type == "TIF/IIF") {
		if(legend == TRUE) {
			par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		}
		plot(theta, rowSums(IIF), type = "l", 
				ylim = c(0, max(rowSums(IIF))),
			xlab = expression(Theta),
			ylab = "Information", 
			main = "Test and Item Information Functions",
			...)
		for(i in 1:ncol(IIF)) lines(theta, IIF[ ,i], col = colors[i])
		if(legend == TRUE) {
			legend("topright", 
				inset = c(-0.3, 0), 
				legend = c("TIF", colnames(IIF)),
				col = c(1, colors),
				lty = 1)
		}
	}
	if(type == "TCC") {
		
		expectedTotal <- rowSums(p)
		
		plot(theta, expectedTotal, 
			type = "l",
			xlab = expression(Theta),
			ylab = "Expected Total Raw Score",
			main = "Test Characteristic Curve",
			col = colors,
			...)
		if(store == TRUE) {
			return(expectedTotal)
		}
	}
	if(type == "ICCs") {
		if(legend == TRUE) {
			par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		}
		if(length(ob) == 2) {
			plot(theta, seq(0, 1, length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Expected Total Raw Score",
				main = "Item Characteristic Curves",
				col = colors,
				...)
			for(i in 1:ncol(p)) lines(theta, p[ ,i], col = colors[i], ...)
			if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = colnames(p),
					col = colors,
					lty = 1)
			}
		}
		else {
			plot(theta, 
					seq(0, max(ICCs), length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Expected Total Raw Score",
				main = "Item Characteristic Curves",
				col = colors,
				...)
			for(i in 1:ncol(ICCs)) lines(theta, 
										ICCs[ ,i], col = colors[i], ...)
			if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = colnames(),
					col = colors,
					lty = 1)
				}	
		}
		
		
		if(store == TRUE) {
			return(p)
		}
	}
	if(type == "ICP") {
		if(length(ob) == 2) {
			stop("Item Category Probability plots only available for
				polytomous models")
		}
		if(legend == TRUE) {
			par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		}
		
		is <- split(sfile, sfile$Item)
		cats <- lapply(is, nrow)

		c1 <- vector("list", length(cats))
		c2 <- vector("list", length(cats))
		c3 <- vector("list", length(cats))
		c4 <- vector("list", length(cats))
		c5 <- vector("list", length(cats))
		c6 <- vector("list", length(cats))
		
		if(identical(colors, cols(ncol(IIF)))) {
			colors <- cols(6)
		}
		for(i in 1:length(cats)) {
			if(cats[[i]] == 2) {
				d1 <- is[[i]]$delta[2]

				c1[[i]] <- 1 / 
						(1+exp(theta - d1) + exp(2*theta))
				c2[[i]] <- exp(theta - d1) / 
						(1+exp(theta - d1) + exp(2*theta))
				c3[[i]] <- exp(2*theta) / 
						(1+exp(theta - d1) + exp(2*theta))
			plot(theta, seq(0, 1, length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = "Item-Category Probabilities",
				...)
			 lines(theta, c1[[i]], col = colors[1])
			 lines(theta, c2[[i]], col = colors[2])
			 lines(theta, c3[[i]], col = colors[3])
			if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = paste("Cat", 1:3),
					col = colors[1:3],
					lty = 1)
				}
			}
			if(cats[[i]] == 3) {
				d1 <- is[[i]]$delta[2]
				d2 <- is[[i]]$delta[3]

				c1[[i]] <- 1 / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta))
				c2[[i]] <- exp(theta - d1) / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta))
				c3[[i]] <- exp(2*theta - (d1 + d2)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta))
				c4[[i]] <- exp(3*theta) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta))

			plot(theta, seq(0, 1, length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = "Item-Category Probabilities",
				...)
			 lines(theta, c1[[i]], col = colors[1])
			 lines(theta, c2[[i]], col = colors[2])
			 lines(theta, c3[[i]], col = colors[3])
			 lines(theta, c4[[i]], col = colors[4])
			 if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = paste("Cat", 1:4),
					col = colors[1:4],
					lty = 1)
				}
			}
			if(cats[[i]] == 4) {
				d1 <- is[[i]]$delta[2]
				d2 <- is[[i]]$delta[3]
				d3 <- is[[i]]$delta[4]

				c1[[i]] <- 1 / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + exp(4*theta))
						
				c2[[i]] <- exp(theta - d1) / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + exp(4*theta))
				c3[[i]] <- exp(2*theta - (d1 + d2)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + exp(4*theta))
				c4[[i]] <- exp(3*theta - (d1 + d2 + d3)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + exp(4*theta))
				c5[[i]] <- exp(4*theta) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + exp(4*theta))

			plot(theta, seq(0, 1, length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = "Item-Category Probabilities",
				...)
			 lines(theta, c1[[i]], col = colors[1])
			 lines(theta, c2[[i]], col = colors[2])
			 lines(theta, c3[[i]], col = colors[3])
			 lines(theta, c4[[i]], col = colors[4])
			 lines(theta, c5[[i]], col = colors[5])
			 if(legend == TRUE) {
				 legend("topright", 
					inset = c(-0.3, 0), 
					legend = paste("Cat", 1:5),
					col = colors[1:5],
					lty = 1)
				}
			}
			if(cats[[i]] == 5) {
				d1 <- is[[i]]$delta[2]
				d2 <- is[[i]]$delta[3]
				d3 <- is[[i]]$delta[4]
				d4 <- is[[i]]$delta[5]

				c1[[i]] <- 1 / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))	
				c2[[i]] <- exp(theta - d1) / 
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))
				c3[[i]] <- exp(2*theta - (d1 + d2)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))
				c4[[i]] <- exp(3*theta - (d1 + d2 + d3)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))
				c5[[i]] <- exp(4*theta - (d1 + d2 + d3 + d4)) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))
				c6[[i]] <- exp(5*theta) /
					(1 + exp(theta - d1) + exp(2*theta - (d1 + d2)) + 
						 exp(3*theta - (d1 + d2 + d3)) + 
						 exp(4*theta - (d1 + d2 + d3 + d4)) + exp(5*theta))

			plot(theta, seq(0, 1, length.out = length(theta)), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = "Item-Category Probabilities",
				...)
			 lines(theta, c1[[i]], col = colors[1])
			 lines(theta, c2[[i]], col = colors[2])
			 lines(theta, c3[[i]], col = colors[3])
			 lines(theta, c4[[i]], col = colors[4])
			 lines(theta, c5[[i]], col = colors[5])
			 lines(theta, c6[[i]], col = colors[6])
			 if(legend == TRUE) {
				 legend("topright", 
					inset = c(-0.3, 0), 
					legend = paste("Cat", 1:6),
					col = colors[1:6],
					lty = 1)
				}
		}
		if(cats[[i]] > 6) {stop("Items with greater than 6 response options
			are not currently supported for type = ICP")}			
		}
	}
	if(type == "thresholds") {
		if(length(ob) == 2) {
			stop("Thurstonian threshold plots only available for
				polytomous models")
		}
		par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		cats <- sapply(split(sfile, sfile$Item), nrow)
		cumulative <- cumsum(cats)

		p <- function(delta) exp(theta - delta) / (1 + exp(theta - delta))
		probs <- mapply(p, sfile$delta)

		if(identical(colors, rainbow(ncol(IIF)))) {
			colors <- rainbow(5)
		}
		
		cols <- unlist(lapply(cats, function(i) 1:i))
		for(i in 1:length(cats)) {
			plot(theta, seq(0, 1, length.out = length(theta)), 
					type = "n", 
					xlab = expression(Theta),
					ylab = "Probability",
					main = "Rasch-Andrich Thresholds",
					...)
			for(j in ((cumulative - cats) + 1)[i]:cumulative[i]) {
				lines(theta, probs[ ,j], col = colors[cols[j]])
			}
			if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = expression(delta [paste(1:cats[i])]),
					col = 
					colors[((cumulative - cats) + 1)[1]:cumulative[1]],
					lty = rep(1, cats[i]))
			}
		}
	}
}