var_YC<-function(v_x){
## v_x is a numerical vector of length greater than 2
## this function calculates the sample variance 
## using the Youngs and Cramer algorithm
    T<-v_x[1]
    RSS<-0
    n<-length(v_x)
    for (j in 2:n){
	T<-T+v_x[j]
	RSS<-RSS+((j*v_x[j]-T)^2)/(j*(j-1))
    }
    RSS/(n-1)
}