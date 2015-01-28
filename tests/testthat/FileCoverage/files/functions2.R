function1 <- function(x, y) {
    if (x < 0) {
        while (y > 0) {
            x <- x + 1
            y <- y - 1
        }   
    } else {
        while (y > 0) {
            x <- x - 1
            y <- y - 1
        }
    }
    x * y
}