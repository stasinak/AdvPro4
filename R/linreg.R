linreg <- setRefClass("linreg",
   fields = list(
     X="matrix",
     Y="matrix",
     reg_coe="vector",
     fit_val="vector",
     resid_e="vector",
     df="numeric",
     resid_var="vector",
     var_beta="vector",
     t_val="vector"
     ),
    methods = list(
      initialize = function(formula,data){
       X<<-model.matrix(formula,data)  # X, matrix containing all the data
       Y<<-data[all.vars(formula)[1]]  # Y, vector containing response variable
       reg_coe<<-solve((t(X)%*%X))%*%t(X)%*%Y  # Estimation of the regression coefficients
       fit_val<<-X%*%reg_coe  # Estimation of the Y values
       resid_e<<-Y-fit_val  # Estimation of the error variable
       n<<-NROW(X)   # Number of data
       p<<-NCOL(X)   # Number of variables
       df<<-n-p   # Degrees of freedom
       resid_var<<-(t(resid_e)%*%resid_e)/df  # Estimates of the variance of the error variable
       var_beta<<-sigma_hat*solve(t(X)%*%X)  # Estimates the variability of the beta coefficients
       t_val<<-reg_coe/sqrt(var_beta)   # T-values for significance of coefficients
       },
      print_out = function(){
        cat()
      },
      plot = function(){
        require(ggplot2)
        data_plot <- data.frame(fit_val, resid_e, sqrt(resid_e/sd(resid_e)))
        ggplot(data_plot, aes(x=fit_val, y=resid_e)) +
          geom_point() +
          ggtitle("Residual vs Fitted") +
          xlab("Fitted values") +
          ylab("Residuals") +
          theme_light()
      }
      ))

lin<-linreg$print_out()

