checkOutput <-
  function( M,MIN,MAX ) {
    if (is.na(M)) {
      warning('NaN returned. No non-missing entries')
    } else if (M==MAX){
      warning('NaN returned. Data has a mean equal the maximum')}
      else if( M==MIN){
      warning('NaN returned. Data has a mean equal the minimum')}
    else{
      warning('NaN returned.')}
  }
