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
		min <- data.frame(minCat = tapply(sfile$Category, sfile$Item, min))
		sfile <- merge(sfile, min, by.x = "Item", by.y = 0) 
		sfile <- subset(sfile, Category != minCat, select = -4)

	}
	names(b) <- substr(
						as.character(ob$ItemParameters$ItemID), 
						2, 
						nchar(as.character(ob$ItemParameters$ItemID)))

	if(length(ob) > 2 & !is.null(itemSelect)) {
		if(ncol(sfile) == 3) {
			if(is.character(itemSelect)) {
				num <- cbind(names(b), 1:length(b))
				num <- subset(num, num[ ,1] %in% itemSelect)
				num <- as.numeric(num[ ,2])
			sfile <- subset(sfile, Item %in% num)
			}
			sfile <- subset(sfile, Item %in% itemSelect)
		}
	}
	if(!is.null(itemSelect)) {
		b <- b[itemSelect]
	}

	if(length(ob) == 2) {
		
		# Rasch model for dichotomous data
		prob <- function(b, theta) {
			1 /(1 + exp(-(theta - b)))
		}

		p <- sapply(b, prob, theta)
		q <- 1 - p
		IIF <- p*q
	}
	if(length(ob) == 3) {
		
		# Rating scale model
		if(ncol(sfile) == 2) {
			prob <- function(delta, beta) exp(theta - (beta + delta)) / (1 + exp(theta - (beta + delta)))		
			
			cats <- nrow(sfile)
			bRep <- rep(b, each = cats)
			d <- rep(sfile$delta, length(b))

			p <- sapply(1:length(d), function(i) {
				prob(d[i], bRep[i])
			})

			q <- 1 - p
			IIF <- p*q # Category information

			ICCs <- sapply(seq(1, ncol(p), by = cats), function(i) {
						rowSums(p[ ,i:(i + (cats - 1))])
					})
			IIFs <- sapply(seq(1, ncol(p), by = cats), function(i) {
						rowSums(IIF[ ,i:(i + (cats - 1))])
					})
		}
	
		# Partial credit model
		if(ncol(sfile) == 3) {
			prob <- function(delta) exp(theta - delta) / (1 + exp(theta - delta))
			
			num <- data.frame(itemID = names(b), Item = 1:length(b))		
			thresholds <- merge(sfile, num, by = "Item")



			p <- mapply(prob, sfile$delta)
			colnames(p) <- paste0(thresholds$itemID, "c", thresholds$Category)
			q <- 1 - p
			IIF <- p*q # Actually the category information

			cats <- sapply(split(sfile, sfile$Item), nrow)

			index <- matrix(c( c(1, (cumsum(cats) + 1)), 
							   c(cumsum(cats), 999)), 
						ncol = 2)
			index <- index[-nrow(index), ]

			sequences <- lapply(1:nrow(index), function(i) {
				index[i, 1]:index[i, 2]
			})

			ICCs <- sapply(1:length(cats), function(i) {
						 rowSums(p[ ,sequences[[i]] ])
					})			
			IIFs <- sapply(1:length(cats), function(i) {
						 rowSums(IIF[ ,sequences[[i]] ])
					})	
		}
	colnames(ICCs) <- names(b)
	colnames(IIFs) <- names(b)
	}

	#cols <- colorRampPalette(c("red", "green", "blue"))
	if(is.null(colors)) {
		colors <- rainbow(ncol(IIF))
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
		if(length(ob) == 2) {
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
			if(store == TRUE) {
				return(IIFs)
			}		
		}
		if(length(ob) == 3) {
			yUpperLim <- max(apply(IIFs, 2, max))
			plot(theta, seq(0, yUpperLim, length.out = length(theta)),
				type = "n", 
				xlab = expression(Theta),
				ylab = "Information", 
				main = "Item Information Functions", 
				...)
			for(i in 1:ncol(IIFs)) lines(theta, IIFs[ ,i], col = colors[i])
			if(legend == TRUE) {
				legend("topright", 
					inset = c(-0.3, 0), 
					legend = colnames(IIFs),
					col = colors,
					lty = 1)
				par(xpd = FALSE)
			}
			if(store == TRUE) {
				return(IIFs)
			}
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
			par(xpd = FALSE)
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
				par(xpd = FALSE)
			}
			if(store == TRUE) {
				return(p)
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
					legend = colnames(ICCs),
					col = colors,
					lty = 1)
				}
			if(store == TRUE) {
				return(ICCs)
			}	
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
		if(ncol(sfile) == 3) {
			is <- split(sfile, sfile$Item)
			names(is) <- names(b)
			delta <- lapply(is, "[", "delta")
		}
		
		catProb <- function(d, b, theta) {
			pieces <- vector("list", length(d))
			for(i in seq_along(d)) {
				pieces[[i]] <- exp(i*theta - sum(b + d[1:i]))	
			}
			
			denom <- 1 + rowSums(matrix(unlist(pieces), ncol = length(d)))  

			lines <- vector("list", length(d) + 1)
			lines[[1]] <- (1 / denom)
			for(i in 2:length(lines)) {
				lines[[i]] <- pieces[[i - 1]] / denom
			}
		return(lines)
		}
		if(ncol(sfile) == 2) {
			colors <- rainbow(length(sfile$delta) + 1)
			catLines <- lapply(1:length(b), function(i) {
				catProb(sfile$delta, b[i], theta)
			})
		}
		if(ncol(sfile) == 3) {
			colors <- rainbow(max(sapply(is, nrow)) + 1)
			catLines <- lapply(1:length(delta), function(i) {
				catProb(delta[[i]]$delta, b[i], theta)
			})
		}

		for(i in seq_along(b)) {
			plot(0, 0, 
				xlim = c(min(theta), max(theta)), 
				ylim = c(0, 1), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = paste("Item-Category Probabilities: ", 
								names(b)[i]),
				...)
		
				for(j in seq_along(catLines[[i]])) {
					lines(x = theta, y = catLines[[i]][[j]], col = colors[j])	
					if(legend == TRUE) {
						 legend("topright", 
							inset = c(-0.3, 0), 
							legend = paste("Cat", 1:length(catLines[[i]])),
							col = colors[1:length(catLines[[i]])],
							lty = 1)
						 par(xpd = FALSE)
					}
				}
			}
	}
	
	if(type == "thresholds") {
		if(legend == TRUE) {
			par(mar=c(5, 4, 4, 8) + .1, xpd = TRUE)
		}

		if(ncol(sfile) == 3) {
			is <- split(sfile, sfile$Item)
			names(is) <- names(b)
			delta <- lapply(is, "[[", "delta")
		}

		thurston <- function(d, b, theta) {
			pieces <- vector("list", length(d))
			for(i in seq_along(d)) {
				pieces[[i]] <- exp(i*theta - sum(b + d[1:i]))	
			}
			
			mat <- matrix(unlist(pieces), ncol = length(pieces))
			denom <- 1 + rowSums(matrix(unlist(pieces), ncol = length(d))) 
			
			cumSums <- sapply(1:(ncol(mat) - 1), function(i) {
				rowSums(mat[ ,i:ncol(mat)]) / denom
			})
			cumSums <- cbind(cumSums, pieces[[length(pieces)]] / denom)
		return(cumSums)
		}

		lines <- lapply(seq_along(b), function(i) thurston(delta[[i]], b[i], theta))				
		colors <- rainbow(max(sapply(lines, ncol)))
		for(i in seq_along(lines)) {
			plot(0, 0, 
					xlim = c(min(theta), max(theta)), 
					ylim = c(0, 1), 
					type = "n",
					xlab = expression(Theta),
					ylab = "Probability",
					main = paste("Thurstonian Thresholds:", 
								names(b)[i]))
			for(j in seq_len(ncol(lines[[i]]))) {
				lines(x = theta, y = lines[[i]][ ,j], col = colors[j])
			}

			if(legend == TRUE) {
				taus <- lapply(1:ncol(lines[[i]]), function(i) {
							bquote(tau[.(i)])
						})
			 legend("topright", 
				inset = c(-0.3, 0), 
				legend = as.expression(taus),
				col = colors[1:ncol(lines[[i]])],
				lty = 1)
			 par(xpd = FALSE)
			}
		}
	if(store == TRUE) {
		return(lines)
	}
	}
}