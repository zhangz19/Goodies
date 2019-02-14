

# modify GISTools::map.scale
myscale <- function (xc, yc, len, units, ndivs, subdiv = 1, tcol = "black", 
                     scol = "black", sfcol = "black") 
{
  frame = par("usr")
  l <- len
  tic = (frame[4] - frame[3])/100
  ul = l/ndivs
  for (i in seq(0, ndivs - 1, by = 2)) rect(xc - l/2 + i * 
                                              ul, yc, xc - l/2 + (i + 1) * ul, yc + tic/2, border = NA, 
                                            col = sfcol)
  lines(c(xc - l/2, xc - l/2, xc + l/2, xc + l/2), c(yc + tic, 
                                                     yc, yc, yc + tic), col = scol)
  lines(c(xc - l/2, xc + l/2), c(yc + tic/2, yc + tic/2), col = scol)
  for (i in 0:ndivs) text(xc - l/2 + ul * i, yc - strheight(i * 
                                                              subdiv) * 0.7, i * subdiv, col = tcol, cex=.8)
  # text(xc, yc - 2 * strheight(units), units, col = tcol)
  text(xc, yc - 1.4 * strheight(units), units, col = tcol)
}