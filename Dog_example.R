# Dog example
# https://class.coursera.org/rprog-002/forum/thread?thread_id=589

dog <- function(name = "Fido", owner = "Bob", barkType = "Woof!", age = 3) {
    noOfBarks <- 5
    swatWithNewspaper <- function() {
        cat("Yelp!\n")
        noOfBarks <<- sample(3:10, 1)
    }
    bark <- function() {
        for (i in 1:noOfBarks) cat(barkType)
    }
    info <- function() {
        cat(paste("Name: ", name))
        cat(paste("\nOwner: ", owner))    
        cat(paste("\nAge: ", age))
        cat("\n")
    }
    list(swatWithNewspaper = swatWithNewspaper, bark = bark, info = info)
}

# test it
Pooh <- dog(name = "Pooh", owner = "Jane Doe", barkType = "LOL!", age = 10)
Joe <- dog(name = "Joe", owner = "Xiaoming", barkType = "Quack!", age = 3)

Pooh$info()
Joe$info()

Pooh$bark()
Joe$bark()

Pooh$swatWithNewspaper()
Joe$swatWithNewspaper()

Pooh$bark()
Joe$bark()