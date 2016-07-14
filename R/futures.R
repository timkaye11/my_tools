library(future)

# fan_in an environment of futures 
#
# @return: an iterator of resolved futures
# @example:
# e <- new.env()
# e["a"]  %<-% { Sys.sleep(4); return ("foo") } 
# e["b"]  %<-% { Sys.sleep(7); return ("bar") }
#
# fan <- Fan$new( e )
# fan$iter() # prints "foo"
# fan$iter() # prints "bar"
# fan$iter() # prints FALSE
#
Fan <- setRefClass("Fan",
    fields = list(
      labels  = "character", 
      n       = "numeric",
      done    = "numeric",
      removed = "numeric",
      futures = "environment"
    ),
    methods = list(
      initialize = function( futures ) {
        labels <<- ls(futures)
        n <<- length(labels)
        done <<- 0
        removed <<- numeric()
        futures <<- futures
      },
      iter = function() {
        i <- 1
        if ( done == n ) { return (FALSE) }  
        if ( sum(resolved(futures) * 1) == n) { return (FALSE) }
        
        while (done < n) {
          idx <- (i%%n) + 1
          if (idx %in% removed) { i <- i + 1; next }
          label <- labels[idx]
          if (!resolved(futureOf(futures[[label]]))) {
            i <- i + 1
          } else {
            done <<- done + 1
            removed <<- c(removed, idx)
            return (futures[[label]])
          }
        }
      }, 
      stop = function() {
        done <<- n
      }
    )
)

