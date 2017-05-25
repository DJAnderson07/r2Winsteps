#' Plot the test characteristic curve
#' @param ob Objects of class r2Winsteps
#' @param type The type of plot to produce. Valid values are "TIF", "IIFs",
#' "TCC", "ICCs" "ICP", "thresholds", and "ipDens". These
#' correspond to, respectively, the test information function, item information
#' functions, the test characteristic curve, item characteristic curves,
#' item-category probabilities, Thurstonion thresholds, and overlayed item and 
#' person densities. Note that ICP and Thresholds are only available for 
#' polytomous models. When specifying \code{lty} or \code{lwd} for "iDens", a 
#' vector (of length 2) must be supplied, corresponding to each distribution. 
#' Defaults to "TIF".
#' @param rel When \code{type == "TIF"}, should the distribution be shaded 
#' according to reliability? Defaults to \code{TRUE} and shades the
#' distribution according to 0.8 and 0.7 reliabilities.
#' @param itemSelect Items to be used in the plotting. Defaults to NULL, in
#' which case all items will be used.
#' @param colors Line colors for the plot. Defaults to NULL, in which case the
#' colors match the defaults from ggplot.
#' @param theta Theta values from which the information function values are 
#' calculated from. Defaults to a sequence from -4 to 4 by 0.1. The limits of 
#' this range are used for the x-axes.
#' @param store Optional logical argument to return data used in plotting 
#' (not available for all plot types). For example, when \code{store = TRUE}
#' and \code{type = "TIF"}, information under the specified theta range will be
#' returned.
#' @param ... Additional arguments passed to \code{\link{plot}}.

plot.r2Winsteps <- function(ob, type = "TIF", rel = TRUE, 
	theta = seq(-4, 4, 0.1), itemSelect = NULL, colors = NULL, 
	legend = TRUE, store = FALSE, ...) {
	
	args <- as.list(match.call())

	b <- ob$ItemParameters$Difficulty
	if(length(ob) == 3) {
		sfile <- ob$StructureFiles
		# min <- data.frame(minCat = tapply(sfile$Category, sfile$Item, min))
		# sfile <- merge(sfile, min, by.x = "Item", by.y = 0) 
		# sfile <- subset(sfile, Category != minCat, select = -4)

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
			else {
				sfile <- subset(sfile, Item %in% itemSelect)
			}
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
			
			p <- mapply(prob, sfile$delta)
			colnames(p) <- paste0("i", sfile$Item, "c", sfile$Category)
			q <- 1 - p
			IIF <- p*q # Actually the category information

			cats <- sapply(split(sfile, sfile$Item), nrow)

			if(length(cats) == 1) {
				ICCs <- matrix(rowSums(p), ncol = 1)
				IIFs <- matrix(rowSums(IIF), ncol = 1)
			}
			else {
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
		}
	colnames(ICCs) <- names(b)
	colnames(IIFs) <- names(b)
	}

	col_hue <- function(n) {
 		 hues = seq(15, 375, length = n + 1)
  		 grDevices::hcl(h = hues, c = 100, l = 65)[1:n]
	}
	if(is.null(colors)) {
		colors <- col_hue(ncol(IIF))
	}
	
	if(type == "TIF") {
		if(legend == TRUE) {
			par(mar = c(5, 4, 4, 6) + 0.1)
		}
		
		pargs <- list(x = quote(theta), 
				 	  y = quote(rowSums(IIF)),
				 	  type = "l",
				 	  ...)
		if(is.null(pargs$xlab)) {
			pargs$xlab <- quote(expression(Theta))
		}
		if(is.null(pargs$ylab)) {
			pargs$ylab <- "Information"
		}
		if(is.null(pargs$main)) {
			pargs$main <- "Test Information Function"
		}
		if(is.null(pargs$col)) {
			pargs$col <- colors
		}
		do.call("plot", pargs)
		
		if(rel == TRUE) {
			info <- rowSums(IIF)

			if(length(theta[info >= 5]) > 0) {
				theta80 <- theta[info >= 5]
				info80 <- info[info >= 5]
				polygon(c(min(theta80), theta80, max(theta80)), c(0, info80, 0), 
					col = rgb(0, 0.2, 0.4, 0.1),
					border = FALSE)
			}
			if(length(theta[info >= 3.333333]) > 0) {
				theta70 <- theta[info >= 3.33333]
				info70 <- info[info >= 3.333333]
				polygon(c(min(theta70), theta70, max(theta70)), c(0, info70, 0), 
					col = rgb(0, 0.2, 0.4, 0.1),
					border = FALSE)
			}
		}
		if(legend == TRUE) {
			par(mar = c(5.1, 8, 4.1, 2), new = TRUE)
			if(length(theta[info >= 5]) > 0) {
				legend("topright", 
					    box.lwd = 0,
					    fill = c(rgb(0, 0.2, 0.4, 0.2), rgb(0, 0.2, 0.4, 0.1)),
					    border = c("white", "white"),
					    c(expression(italic(p * theta * theta^"'" == 0.80)),
					      expression(italic(p * theta * theta^"'" == 0.70))),
					    cex = 1.5)
			}
			if(length(theta[info >= 5]) == 0 &
			   length(theta[info >= 3.333333]) > 0) {
				legend("topright", 
					    box.lwd = 0,
					    fill = c(rgb(0, 0.2, 0.4, 0.2), rgb(0, 0.2, 0.4, 0.1)),
					    border = c("white", "white"),
					    legend = expression(italic(p * theta * theta^"'" == 0.70)),
					    cex = 1.5)
			}

			par(mar = c(5, 4, 4, 2) + 0.1)
		}
		if(store == TRUE) {
			return(rowSums(IIF))
		}
	}
	if(type == "IIFs") {
		if(legend == TRUE) {
			par(mar = c(5, 4, 4, 7.5) + 0.1)
		}
		if(length(ob) == 2) {
			yUpperLim <- max(apply(IIF, 2, max))
		}
		if(length(ob) == 3) {
			yUpperLim <- max(apply(IIFs, 2, max))
		}
		pargs <- list(x = quote(theta), 
			 	  y = quote(seq(0, yUpperLim, length.out = length(theta))),
			 	  type = "n",
			 	  ...)
		if(is.null(pargs$xlab)) {
			pargs$xlab <- quote(expression(Theta))
		}
		if(is.null(pargs$ylab)) {
			pargs$ylab <- "Information"
		}
		if(is.null(pargs$main)) {
			pargs$main <- "Item Information Functions"
		}
		if(is.null(pargs$bty)) {
			pargs$bty <- "n"
		}
		do.call(plot, pargs)
		
		if(length(ob) == 2) {
			for(i in 1:ncol(IIF)) lines(theta, IIF[ ,i], col = colors[i])	
		}
		if(length(ob) == 3) {
			for(i in 1:ncol(IIFs)) lines(theta, IIFs[ ,i], col = colors[i])
		}
		if(legend == TRUE) {
			par(mar = c(5.1, 0, 4.1, 2), new = TRUE)
			plot(theta, seq(0, yUpperLim, length.out = length(theta)),
				bty = "n",
				type = "n", 
				xaxt = "n",
				yaxt = "n",
				bty = "n",
				xlab = "",
				ylab = "", 
				main = "")
			if(length(ob) == 2) {
				legend("topright",  
					legend = colnames(IIF),
					col = colors,
					lty = 1)
			}
			if(length(ob) == 3) {
				legend("topright", 
					legend = colnames(IIFs),
					col = colors,
					lty = 1,
					box.lwd = 0)
			}
		}
		if(store == TRUE) {
			if(length(ob) == 2) {
				return(IIF)
			}
			if(length(ob) == 3) {
				return(IIFs)
			}
		}		
	}
	if(type == "TCC") {
		expectedTotal <- rowSums(p)
		
		pargs <- list(x = quote(theta), 
			 	  y = quote(expectedTotal),
			 	  type = "l",
			 	  ...)
		if(is.null(pargs$xlab)) {
			pargs$xlab <- quote(expression(Theta))
		}
		if(is.null(pargs$ylab)) {
			pargs$ylab <- "Expected Total Raw Score"
		}
		if(is.null(pargs$main)) {
			pargs$main <- "Test Characteristic Curve"
		}
		if(is.null(pargs$bty)) {
			pargs$bty <- "n"
		}
		if(is.null(pargs$col)) {
			pargs$col <- colors
		}
		do.call(plot, pargs)
		
		if(store == TRUE) {
			return(expectedTotal)
		}
	}
	if(type == "ICCs") {
		if(legend == TRUE) {
			max_char <- max(nchar(as.character(ob$ItemParameters$ItemID)))
			wdth <- 0.9 - (max_char * 0.01)
			layout(t(c(1, 2)), widths = c(wdth, 1 - wdth))	
		}
		if(length(ob) == 2) {
			pargs <- list(x = quote(theta),
						  y = seq(0, 1, length.out = length(theta)),
						  type = "n",
						  ...)
		}
		if(length(ob) == 3) {
			pargs <- list(x = quote(theta),
						  y = seq(0, max(ICCs), length.out = length(theta)),
						  type = "n",
						  ...)
		}
		if(is.null(pargs$xlab)) {
			pargs$xlab <- quote(expression(Theta))
		}
		if(is.null(pargs$ylab)) {
			pargs$ylab <- "Expected Total Raw Score"
		}
		if(is.null(pargs$main)) {
			pargs$main <- "Item Characteristic Curves"
		}
		if(is.null(pargs$bty)) {
			pargs$bty <- "n"
		}
		if(is.null(pargs$col)) {
			pargs$col <- colors
		}
		do.call(plot, pargs)
		
		if(length(ob) == 2) {
			for(i in 1:ncol(p)) lines(theta, p[ ,i], col = pargs$col[i], ...)
		}
		if(length(ob) == 3) {
			for(i in 1:ncol(ICCs)) lines(theta, 
										ICCs[ ,i], col = colors[i], ...)
		}
		if(legend == TRUE) {
			op <- par(mar = c(5.1, max_char * .35, 4.1, 0))
			on.exit(par(op))
			plot(seq(0, 1, length = 15), 
				 1:15,
				type = "n",
				bty = "n", 
				xaxt = "n",
				xlab = "", 
				yaxt = "n",
				ylab = "")

			if(length(ob) == 2) {
				axes <- cbind(c(0, 1), rep(1:ncol(p), each = 2))
			}
			if(length(ob) == 3) {
				axes <- cbind(c(0, 1), rep(1:ncol(ICCs), each = 2))
			}
			
			Map(lines, 
				split(axes[ ,1], axes[ ,2]), 
				split(axes[ ,2], axes[ ,2]),
				col = pargs$col)
			if(length(ob) == 2) {
				axis(2, 
					lwd = 0, 
					at = 1:ncol(p), 
					labels = as.character(ob$ItemParameters$ItemID), 
					las = 2)
			}
			if(length(ob) == 3) {
				axis(2, 
					lwd = 0, 
					at = 1:ncol(ICCs), 
					labels = as.character(ob$ItemParameters$ItemID), 
					las = 2)
			}
		}
		if(store == TRUE) {
			if(length(ob) == 2) return(p)
			if(length(ob) == 3) return(ICCs)
		}
	}
	if(type == "ICP") {
		if(length(ob) == 2) {
			stop("Item Category Probability plots only available for
				polytomous models")
		}
		if(legend == TRUE) {
			par(mar = c(5, 4, 4, 7.5) + 0.1)
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
			colors <- col_hue(length(sfile$delta) + 1)
			catLines <- lapply(1:length(b), function(i) {
				catProb(sfile$delta, b[i], theta)
			})
		}
		if(ncol(sfile) == 3) {
			colors <- col_hue(max(sapply(is, nrow)) + 1)
			catLines <- lapply(1:length(delta), function(i) {
				catProb(delta[[i]]$delta, b[i], theta)
			})
		}

		for(i in seq_along(b)) {
			plot(0, 0, 
				bty = "n", 
				xlim = c(min(theta), max(theta)), 
				ylim = c(0, 1), 
				type = "n",
				xlab = expression(Theta),
				ylab = "Probability",
				main = paste("Item-Category Probabilities: ", 
								names(b)[i]) )
		
				for(j in seq_along(catLines[[i]])) {
					lines(x = theta, y = catLines[[i]][[j]], col = colors[j])
				}	
					if(legend == TRUE) {
						 par(mar = c(5.1, 0, 4.1, 2), new = TRUE)
						 plot(0, 0, 
							xaxt = "n",
							yaxt = "n",
							type = "n",
							xlab = "",
							ylab = "",
							main = "", 
							bty = "n")
						 legend("topright", 
							legend = paste("Cat", 1:length(catLines[[i]])),
							col = colors[1:length(catLines[[i]])],
							lty = 1,
							box.lwd = 0)
						 par(mar = c(5, 4, 4, 7.5) + 0.1)
					}
				}
			}
	
	
	if(type == "thresholds") {
		if(legend == TRUE) {
			par(mar = c(5, 4, 4, 7.5) + 0.1)
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
		colors <- col_hue(max(sapply(lines, ncol)))
		for(i in seq_along(lines)) {
			plot(0, 0, 
					bty = "n",
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
				par(mar = c(5.1, 0, 4.1, 2), new = TRUE)
				taus <- lapply(1:ncol(lines[[i]]), function(i) {
							bquote(tau[.(i)])
						})
				plot(0, 0, 
					bty = "n",
					type = "n",
					xlab = "",
					xaxt = "n",
					ylab = "",
					yaxt = "n",
					bty = "n",
					main = "")
				 legend("topright", 
					legend = as.expression(taus),
					col = colors[1:ncol(lines[[i]])],
					lty = 1,
					box.lwd = 0)
			 	par(mar = c(5, 4, 4, 7.5) + 0.1)
			}
		}
	if(store == TRUE) {
		return(lines)
		}
	}
	if(type == "ipDens") {
		densP <- density(ob$PersonParameters$Theta)
		densI <- density(ob$ItemParameters$Difficulty, bw = densP$bw)
		
		pargs <- list(x = quote(density(ob$PersonParameters$Theta)), 
					  type = "n",
					   ...)
		
		if(is.null(pargs$lwd)) {
			lineW <- quote(c(1, 2))
		}
		if(is.null(pargs$lty)) {
			lineT <- quote(c(1, 2))
		}
		if(is.null(pargs$xlim)) {
			pargs$xlim <- quote(c(min(c(densP$x, densI$x)),
									max(c(densP$x, densI$x))))
		}
		if(is.null(pargs$ylim)) {
			pargs$ylim <- quote(c(0, max(c(densP$y, densI$y))))
		}
		if(is.null(pargs$main)) {
			pargs$main <- "Person/Item Probability Density Distributions"
		}
		if(length(colors) != 2) {
			colors <- c("black", "blue")
		}

		par(mar = c(5, 4, 4, 6) + 0.1)
		do.call("plot", pargs)
		lines(densI, 
			 col = colors[1],
			 lwd = parse(text = lineW)[[2]],
			 lty = parse(text = lineT)[[2]])
		lines(densP, 
			col = colors[2],
			lwd = parse(text = lineW)[[3]],
			lty = parse(text = lineT)[[3]])
		
		par(mar = c(5.1, 0, 4.1, 2), new = TRUE)
		plot(densI, 
			bty = "n",
			type = "n", 
			axes = FALSE,
			ann = FALSE)
		legend("topright",				
			   c("Items", "Persons"), 
			   lwd = c(parse(text = lineW)[[2]], parse(text = lineW)[[3]]),
			   lty = c(parse(text = lineT)[[2]], parse(text = lineT)[[3]]),
			   col = colors,
			   box.lwd = 0)
		par(mar = c(5, 4, 4, 2) + 0.1)
	}
}