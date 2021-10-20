##############################################################################################
# plot of prediction measures
##############################################################################################

# generate sensitivity plot
sensitivity_plot <- function(dataset){
  
  # add row names
  rownames(dataset) <- dataset$name_dataset
  
  # ggplot
  ggplot(data = dataset, aes(x = sensitivity, y = name_dataset)) +
    geom_vline(aes(xintercept = dataset["full",]$sensitivity), size = 0.3, linetype = "dashed", colour = "red") +
    geom_errorbarh(aes(xmin = lower_CI_sensitivity, xmax = upper_CI_sensitivity), size = 0.6, height = 0.3, colour = "black") + # error bar
    geom_point(size = 1.5, colour = "#4384CD") + # add points for means
    theme_stata() + # theme
    theme(axis.text.y = element_text(angle=0)) + # make y-axis labels horizontal
    theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +  # adjust size of title
    labs(x = "mean sensitivity", y = "dataset", title = "Mean sensitivity") + 
    ylim("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40") +
    scale_x_continuous(limits = c(0.72, 0.75), 
                       breaks = scales::pretty_breaks(n = 4),
                        labels = scales::label_number(accuracy = 0.01)) 
}


# generate specificity plot
specificity_plot <- function(dataset){
  
  # add row names
  rownames(dataset) <- dataset$name_dataset
  
  #ggplot
  ggplot(data = dataset, aes(x = specificity, y = name_dataset)) +
    geom_vline(aes(xintercept = dataset["full",]$specificity), size = 0.3, linetype = "dashed", colour = "red") +
    geom_errorbarh(aes(xmin = lower_CI_specificity, xmax = upper_CI_specificity), size = 0.6, height = 0.3, colour = "black") + # error bar
    geom_point(size = 1.5, colour = "#4384CD") + # add points for means
    theme_stata() + # theme
    theme(axis.text.y = element_text(angle=0)) + # make y-axis labels horizontal
    theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +  # adjust size of title
    labs(x = "mean specificity", y = "dataset", title = "Mean specificity") + 
    ylim("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40") +
    scale_x_continuous(limits = c(0.83, 0.86), 
                       breaks = scales::pretty_breaks(n = 4),     
                        labels = scales::label_number(accuracy = 0.01)) 
}


# generate accuracy plot
accuracy_plot <- function(dataset){
  
  # add row names
  rownames(dataset) <- dataset$name_dataset
  
  # ggplot
  ggplot(data = dataset, aes(x = accuracy, y = name_dataset)) +
    geom_vline(aes(xintercept = dataset["full",]$accuracy), size = 0.3, linetype = "dashed", colour = "red") +
    geom_errorbarh(aes(xmin = lower_CI_accuracy, xmax = upper_CI_accuracy), size = 0.6, height = 0.3, colour = "black") + # error bar
    geom_point(size = 1.5, colour = "#4384CD") + # add points for means
    theme_stata() + # theme
    theme(axis.text.y = element_text(angle=0)) + # make y-axis labels horizontal
    theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +  # adjust size of title
    labs(x = "mean accuracy", y = "dataset", title = "Mean accuracy") + 
    ylim("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40") +
    scale_x_continuous(limits = c(0.775, 0.91), 
                       breaks = scales::pretty_breaks(n = 3),    
                        labels = scales::label_number(accuracy = 0.01)) 
}


# generate auc plot
auc_plot <- function(dataset){
  
  # add row names
  rownames(dataset) <- dataset$name_dataset
  
  # ggplot
  ggplot(data = dataset, aes(x = auc, y = name_dataset)) +
    geom_vline(aes(xintercept = dataset["full",]$auc), size = 0.3, linetype = "dashed", colour = "red") +
    geom_errorbarh(aes(xmin = lower_CI_auc, xmax = upper_CI_auc), size = 0.6, height = 0.3, colour = "black") + # error bar
    geom_point(size = 1.5, colour = "#4384CD") + # add points for means                          
    theme_stata() + # theme
    theme(axis.text.y = element_text(angle=0)) + # make y-axis labels horizontal
    theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +  # adjust size of title
    labs(x = "mean auc", y = "dataset", title = "Mean auc") + 
    ylim("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40") +
    scale_x_continuous(limits = c(0.86, 0.885), 
                       breaks = scales::pretty_breaks(n = 3),  
                        labels = scales::label_number(accuracy = 0.01)) 
}


# generate brier score plot
brier_score_plot <- function(dataset){
  
  # add row names
  rownames(dataset) <- dataset$name_dataset
  
  # ggplot
  ggplot(data = dataset, aes(x = brier_score, y = name_dataset)) +
    geom_vline(aes(xintercept = dataset["full",]$brier_score), size = 0.3, linetype = "dashed", colour = "red")+
    geom_errorbarh(aes(xmin = lower_CI_brier_score, xmax = upper_CI_brier_score), size = 0.6, height = 0.3, colour = "black") + # error bar
    geom_point(size = 1.5, colour = "#4384CD") + # add points for means
    theme_stata() + # theme
    theme(axis.text.y = element_text(angle=0)) + # make y-axis labels horizontal
    theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +  # adjust size of title
    labs(x = "mean brier score", y = "dataset", title = "Mean brier score") + 
    ylim("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40") +
    scale_x_continuous(limits = c(0.14, 0.15), 
                       breaks = scales::pretty_breaks(n = 4),  
                        labels = scales::label_number(accuracy = 0.001))
}


# arrange all plots together
prediction_measures_plots <- function(dataset){
  ggarrange(
    sensitivity_plot(data = dataset), 
    specificity_plot(data = dataset), 
    accuracy_plot(data = dataset), 
    auc_plot(data = dataset),
    brier_score_plot(data = dataset),
    nrow = 3, ncol = 2,
    labels = "AUTO"
  )}


