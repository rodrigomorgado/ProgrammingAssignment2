
## Cria cache da matriz
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL


    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }


    get <- function() {
    	## Return the matrix
    	m
    }


    setInverse <- function(inverse) {
        i <<- inverse
    }


    getInverse <- function() {

        i
    }


    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#Computa a inversa e retorna o cache, se tiver
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }


    data <- x$get()


    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
