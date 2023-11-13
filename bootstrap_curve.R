# To obtain bootstrap results, calculate the mean result and then the mean square distance from each dataset to this mean.
# Next, retain the 95% least distant curves (classic bootstrap).

select_curves<-function(scores)
{
  scores$weight |> as.matrix() ->weights
  dim(weights)<-c(length(weights)/150,150)
  m<-colMeans(weights)
  dist_fun <- function(row) {
    # Your custom logic here
    # This example function simply sums the values in each row
    d <- sum((row-m)^2)
    return(d)
  }
  
  # Apply the function to each row of the matrix using apply()
  distances <- apply(weights, 1, dist_fun)
  
  sort_idx = order(distances)

  percentage<-0.025
  ncut =  round(length(distances) * percentage)
  
  curves = weights[sort_idx[ncut:(length(distances)-ncut)],]
  return(curves)
}