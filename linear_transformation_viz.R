library(ggplot2)

plot_coord_system <- function(){
  # Build an x-y coord system
  
  coord_plot = ggplot() +
    geom_hline(yintercept = -10:10, color = "lightgray", linetype = "dotted") +  # Grid lines
    geom_vline(xintercept = -10:10, color = "lightgray", linetype = "dotted") +  
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +  # X-axis
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 1) +  # Y-axis
    scale_x_continuous(breaks = -10:10) + 
    scale_y_continuous(breaks = -10:10) +
    coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  return(coord_plot)
}
  
plot_vector <- function(vector, label, color, coord_plot){
  
  # Plot a vector on a given coord plot
  
  coord_plot +
    geom_segment(
      aes(
        x = 0,
        y = 0,
        xend = vector[1],
        yend = vector[2]),
      arrow = arrow(length = unit(0.2, "inches")),
      color = color, size = 0.5) +
    geom_text(aes(x=vector[1], y=vector[2], label = label),
              vjust = -0.5, hjust = 0.5, size = 5, color = color)

}

apply_transformation <- function(matrix, vector){
  
  # transform a vector using a provided matrix
  
  new_vector = solve(matrix, vector)
  
  return(new_vector)
  
}

plot_basis_vectors <- function(plot) {
  
  # Plot basis vectors
  
  i_hat <- c(1,0)
  j_hat <- c(0,1)
  
  plot |> 
    plot_vector(i_hat, label = 'i', color = 'lightgreen') |> 
    plot_vector(j_hat, label = 'j', color = 'pink')
  
}

show_transformation <- function(matrix, vector){
  
  # Plot a vector and its corresponding transformation given a matrix
  # Includes i_hat and j_hat transformations
  
  plot <- plot_coord_system()
  
  plot <- plot_vector(vector=vector, label = 'v', color = 'lightblue', coord_plot = plot)
  
  t_vector <- apply_transformation(matrix, vector)
  
  plot <- plot_vector(t_vector, label = 'T(v)', color = 'blue', coord_plot = plot)
  
  
  return(plot)
  
}


  
  