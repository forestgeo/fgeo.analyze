torusonesp.all_original=function(species="GIROPA", hab.index20=hab.index20, allabund20=allabund20, plotdim=c(1000,500), gridsize=20)
{
  plotdimqx = plotdim[1]/gridsize  		# Calculates no. of x-axis quadrats of plot. (x is the long axis of plot in the case of Pasoh)
  plotdimqy = plotdim[2]/gridsize  		# Calculates no. of y-axis quadrats of plot.
  num.habs = length(unique(hab.index20$habitats))  	# Determines tot. no. of habitat types.

  GrLsEq = matrix(0,1,num.habs*6)    		# Creates empty matrix for output.
  rownames(GrLsEq) = species       		# Names single row of output matrix.


  for(i in 1:num.habs)           		# Creates names for columns of output matrix.
  {
    if(i==1)
      cols=c(paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))
    if(i>1)
      cols=c(cols, paste("N.Hab.",i,sep=""), paste("Gr.Hab.",i,sep=""), paste("Ls.Hab.",i,sep=""), paste("Eq.Hab.",i,sep=""), paste("Rep.Agg.Neut.",i,sep=""), paste("Obs.Quantile.",i,sep=""))
  }
  colnames(GrLsEq)=cols    			# Names columns of output matrix.


  # CALCULATIONS FOR OBSERVED RELATIVE DENSITIES ON THE TRUE HABITAT MAP

  allabund20.sp = allabund20[which(rownames(allabund20)==species),]  				# pulls out the abundance by quad data for the focal species
  spmat = matrix(as.numeric(allabund20.sp), nrow=plotdimqy, plotdimqx, byrow=F)  		# Fills a matrix, with no. rows = plotdimqy (dim 2) and no. columns = plotdimqx (dim 1), with the indiv. counts per quadrat of one species.
  totmat = matrix(apply(allabund20, MARGIN=2, FUN="sum"), plotdimqy, plotdimqx, byrow=F)     	# calculates total number of stems in each quad for all species and puts in matrix

  habmat = matrix(hab.index20$habitats, nrow=plotdimqy, ncol=plotdimqx, byrow=F)		# fills matrix with habitat types, oriented in the same way as the species and total matrices above

  spstcnthab = numeric()   			# Creates empty vector for stem counts per sp. per habitat.
  totstcnthab = numeric()  			# Creates empty vector for tot. stem counts per habitat.

  for(i in 1:num.habs)
  {
    totstcnthab[i] = sum(totmat[habmat==i])	# Determines tot. no. stems per habitat of the true map.
    spstcnthab[i] = sum(spmat[habmat==i]) 	# Determines tot. no. stems for focal sp. per habitat of the true map.
  }

  spprophab=spstcnthab/totstcnthab         	# Calculates observed relative stem density of focal sp. per habitat of the true map.

  # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS
  habmat.template=habmat

  for (j in 1:4)    {
    # apply rotations and mirrors
    # if j==1 do nothing

    if (j==2) habmat = apply(habmat.template, 2, rev)
    if (j==3) habmat = t(apply(habmat.template, 1, rev))
    if (j==4) habmat = t(apply(apply(habmat.template, 2, rev),1,rev))


    # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS

    for(x in 0:(plotdimqx-1))    # Opens "for loop" through all 20-m translations along x-axis.
    {
      for(y in 0:(plotdimqy-1))  # Opens "for loop" through all 20-m translations along y-axis.
      {
        newhab=matrix(0,plotdimqy,plotdimqx)  # Creates empty matrix of quadrats' habitat designations.


        # The following "if" statements create the x,y torus-translation of the habitat map.

        if(y==0 & x==0)
          newhab=habmat

        if(y==0 & x>0)
          newhab=habmat[c(1:plotdimqy), c((plotdimqx-x+1):plotdimqx, 1:(plotdimqx-x))]

        if(x==0 & y>0)
          newhab=habmat[c((plotdimqy-y+1):plotdimqy, 1:(plotdimqy-y)), c(1:plotdimqx)]

        if(x>0 & y>0)
          newhab=habmat[c((plotdimqy-y+1):plotdimqy, 1:(plotdimqy-y)), c((plotdimqx-x+1):plotdimqx, 1:(plotdimqx-x))]


        Torspstcnthab = numeric()   # Creates empty vector for stem counts per sp. per habitat in torus-based maps.
        Tortotstcnthab = numeric()  # Creates empty vector for tot. stem counts per habitat in torus-based maps.

        for(i in 1:num.habs)
        {
          Tortotstcnthab[i] = sum(totmat[newhab==i])  # Determines tot. no. stems per habitat of the focal torus-based map.
          Torspstcnthab[i] = sum(spmat[newhab==i])    # Determines tot. no. stems for focal sp. per habitat of the focal torus-based map.
        }

        Torspprophab = Torspstcnthab/Tortotstcnthab   # Calculates relative stem density of focal sp. per habitat of the focal torus-based map.


        for(i in 1:num.habs)
        {

          if(spprophab[i] > Torspprophab[i])          # If rel. dens. of focal sp. in focal habitat of true map is greater than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "greater than" count.
            GrLsEq[1,(6*i)-4] = GrLsEq[1,(6*i)-4]+1

          if(spprophab[i] < Torspprophab[i])          # If rel. dens. of focal sp. in focal habitat of true map is less than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "less than" count.
            GrLsEq[1,(6*i)-3] = GrLsEq[1,(6*i)-3]+1

          if(spprophab[i] == Torspprophab[i])         # If rel. dens. of focal sp. in focal habitat of true map is equal to rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "equal to" count.
            GrLsEq[1,(6*i)-2] = GrLsEq[1,(6*i)-2]+1

        }
      }    # Closes "for loop" through all 20-m translations along x-axis.
    }      # Closes "for loop" through all 20-m translations along y-axis.
  }	    # Closes for loop through mirrors and rotations (j)


  for(i in 1:num.habs)
  {
    GrLsEq[1,(6*i)-5] = spstcnthab[i]								# add counts of No. stems in each habitat

    if (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) <= 0.025) GrLsEq[1,(6*i)-1] = -1	# if rel.dens. of sp in true map is greater than rel. dens. in torus map less than 2.5% of the time, then repelled
    if (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) >= 0.975) GrLsEq[1,(6*i)-1] = 1	# if rel.dens. of sp in true map is greater than rel. dens. in torus map more than 97.5% of the time, then aggregated
    if ( (GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy)) < 0.975) & (GrLsEq[1,(6*i)- 4]/(4*(plotdimqx*plotdimqy)) > 0.025) ) GrLsEq[1,(6*i)-1] = 0          # otherwise it's neutral (not different from random dist)

    GrLsEq[1,(6*i)] = GrLsEq[1,(6*i)-4]/(4*(plotdimqx*plotdimqy))				# quantile in the TT distribtution of relative densities of the true relative density
  }

  return(GrLsEq)
}
