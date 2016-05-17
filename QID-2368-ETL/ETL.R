# Bivariate normal distribution | SUBROUNTINE of mvncdf()
# Integrand is exp( -(b1^2 + b2^2 - 2*b1*b2*sin(theta))/(2*cos(theta)^2) )
bvnIntegrand = function(theta, b1, b2){

    sintheta   = sin(theta);
    cossqtheta = cos(theta)^2;
    integrand  = exp(-((b1 * sintheta - b2)^2 / cossqtheta + b1^2) / 2)
    return(integrand)
}

# MVNCDF Multivariate normal cumulative distribution function.
# P = MVNCDF(B, MU, SIGMA) returns the joint cumulative
# probability using numerical integration. B is a vector of values, MU is the
# mean parameter vector, and SIGMA is the covariance matrix.
mvncdf = function(b, mu, sigma){
    n   = NROW(b);
    b   = as.matrix(b)
    if (NCOL(b) != length(mu)){
        stop('The first two inputs must be vectors of the same length.')
    }
    rho = sigma[2]
    if (rho > 0){
        p1 = pnorm(apply(b, 2, min));
        p1[any(is.nan(b), 2)] = NaN;
    }else{
        p1 = pnorm(b[, 1])-pnorm(-b[, 2]);
        p1[p1 < 0] = 0; # max would drop NaNs
    }
    if(abs(rho) < 1){
        loLimit = asin(rho);
        hiLimit = sign(rho) * pi / 2
        p2      = numeric()
        for (i in 1:n){
   	        b1    = b[i, 1]
            b2    = b[i, 2]
            p2[i] = integral(function(x) bvnIntegrand(x, b1, b2), xmin = loLimit, xmax = hiLimit, method = "Kronrod", reltol = 1e-10)
        }
    }else{
        p2 = rep(0, length(p1))
    }
      
    p = p1 - p2/(2 * pi)

return(p)
}

# MAIN ROUTINE
ETL = function(a, R, defProb, UAP){
    C     = qnorm(defProb, 0, 1);
	  NinvK = qnorm(UAP / (1 - R), 0, 1);
	  A     = (C - sqrt(1 - a^2) * NinvK) / a;
	  Sigma = matrix(c(1, -a, -a, 1), 2, 2);
	  Mu    = c(0, 0);
	  EL1   = mvncdf(b = cbind(C,-A),mu = Mu, sigma = Sigma);
	  EL2   = pnorm(A);
	  EL    = EL1/UAP*(1-R)+EL2;
	  return(EL)
}

# Example Call

# Install packages if not installed
libraries = c("pracma")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

index     = 37.2416
recoveryR = 0.4                             # Recovery rate
UAP       = c(0.03, 0.06, 0.09, 0.12, 0.22) # Upper attachment points
lam       = index / 10000 / (1 - recoveryR)
tr        = 1
defProb   = 1 - exp(-lam);                  # Default probability
rho       = seq(0.01, 0.99, by = 0.01);     # compound correlation
a         = sqrt(rho);                      # square-root of compound correlation
etl       = numeric()
for (i in 1:length(a)){
    etl[i] = ETL(a[i], recoveryR, defProb, UAP[tr]);
}
etl