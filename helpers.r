get_zoom <- function(m) {
    m <- as.data.frame(m)
    if (nrow(unique(m[, c('longitude', 'latitude')])) == 1) {
        return(10)
    } else {
        maxdiff <- max(diff(range(m$longitude)),  diff(range(m$latitude)))
        return(abs(min(c(16, max(1, floor((-1*( (log(maxdiff)/log(2)) - (log(360)/log(2))))))))))
    }
}
