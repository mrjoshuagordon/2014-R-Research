integrate(function(y) { 
     sapply(y, function(y) {
         integrate(function(x) {
            sapply(x, function(x) x^2 * y )
           }, 0, 1)$value
       })
   }, 0, 2)


#http://rgm.ogalab.net/RGM/R_rdfile?f=stppResid/man/tessresid.Rd&d=R_CC