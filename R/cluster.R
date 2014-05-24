#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


### this is called by cluster in stream!
.cluster.DSC_MOA <- function(dsc, dsd, n, verbose=FALSE, ...) {
  ## data has to be all doubles for MOA clusterers!
  for (i in 1:n) {
    
    
    d <- get_points(dsd, 1)
    ## TODO: Check incoming data
    
    
    x <- .jcast(
      .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(d))),
      "weka/core/Instance"
    )
    
    .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    
    if(verbose && !i%%1000) cat("Processed", i, "points -",
                                nclusters(dsc), "clusters\n")
    
  }	
}

