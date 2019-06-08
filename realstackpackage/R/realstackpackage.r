library(devtools)
library(roxygen2)
library(R6)

rm(list = ls())
ls()

#package.skeleton("shiny_mod")
#my.Rpackage <- as.package("shiny_mod")
#load_all(my.Rpackage)
#document(my.Rpackage)
Stack <- R6::R6Class(classname = "Stack",
                     public = list(items = list(),
                                   push = function(x) {
                                     self$items <- c(self$items, x)
                                     invisible(self) # Makes chaining possible
                                   },
                                   pop = function() {
                                     item_pop <- self$items[[private$length()]]
                                     self$items <- self$items[-private$length()]
                                     item_pop
                                   }
                     ),
                     private = list(length = function() {
                       length(self$items)
                     })

)


# Testing
# s <- Stack$new()
# s$push('haha')$push(5)
# s$items
# s$length
# s$pop()
# s$items

# Inherit from class Stack
NumberStack <- R6::R6Class(classname = "NumberStack",
                           inherit = Stack,
                           public = list(initialize = function(items, StackName) {
                             stopifnot(is.numeric(items))
                             self$items <- items
                             private$.StackName <- StackName
                           }
                           ),
                           private = list(.StackName = NA),
                           active = list(
                             # Active fields are particularly useful in conjunction with private fields,
                             # because they make it possible to implement components that look like fields
                             # from the outside but provide additional checks.
                             StackName = function(value) {
                               if(missing(value)) {
                                 private$.StackName
                               }
                               else {
                                 stop("'$StackName' is read only", call. = FALSE)
                               }}
                           )

)

# Testing
# s <- NumberStack$new('haha','Sean')
# s <- NumberStack$new(5,'Sean')
# s$push(5)$push(c(1:3))
# s$StackName
# s$StackName <- 'hahaha'
# s$length()
# s$fjkalfas()
# s$items
# s$pop()
# s$items
# s$push('haha')
# s$items

# Adding methods
NumberStackk <- NumberStack$set("public", "addup", function(){
  sum(self$items)
})
NumberStackk <- NumberStack$set("public", "print", function(...) {
  cat("Name of the Number Stack: ", private$.StackName, "\n", sep="")
  cat("List of Number Stack: ", paste(self$items, collapse = ","), "\n", sep = "")
  cat("Sum of the Number Stack: ", self$addup(), "\n", sep = "")
  invisible(self)
})

NumberStack2 <- R6::R6Class(classname = "NumberStack2",
                            inherit = NumberStackk)
# Testing
# s <- NumberStack$new(5, 'Sean')
# s$push(3)$addup()
# s$print()
# s$StackName
# s$.StackName
# s$StackName = 'hahahah '
# class(s)
# names(s)

# Reference Semantics
# s1 <- NumberStack$new(5,'Sean')$push(6)
# s2 <- s1$clone()
# s1$push('hahaha')
# c(s1 = s1$items, s2 = s2$items)

###############################################################################
# Finalizer
# NumberStack3 <- NumberStack$set("public", "finalize", function(){
#                                 message("Cleaning up ", self$items)
#                                 unlink(self$items)
# })
#
# s <- NumberStack3$new(5, 'Sean')
