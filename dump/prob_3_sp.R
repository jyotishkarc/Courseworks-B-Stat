library(sp)

data <- data.frame(x = runif(30, 0, 10), y = runif(30, 0, 10))

hull_indices <- chull(data$x, data$y)
hull_points <- data[hull_indices, ]

# Define a test point
test_point <- data.frame(x = 5, y = 5)

position <- point.in.polygon(test_point$x, test_point$y, hull_points$x, hull_points$y)

# Interpret the output
if (position == 1) {
   cat("The point is inside the convex hull.\n")
} else if (position == 0) {
   cat("The point is outside the convex hull.\n")
} else if (position == -1) {
   cat("The point is on the boundary of the convex hull.\n")
}
