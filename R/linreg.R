#' Title
#'
#' @field X matrix.
#' @field Y matrix.
#' @field reg_coe vector.
#' @field fit_val vector.
#' @field resid_e vector.
#' @field n numeric.
#' @field p numeric.
#' @field df numeric.
#' @field resid_var numeric.
#' @field var_beta matrix.
#' @field t_val vector.
#'
#' @return
#' @export
#'
#' @examples
linreg <-setRefClass("linreg",
   fields = list(
     formula="formula",
     data = "data.frame",
     dname = "vector",
     X="matrix",
     Y="matrix",
     reg_coe="vector",
     fit_val="vector",
     resid_e="vector",
     n="numeric",
     p="numeric",
     df="numeric",
     resid_var="numeric",
     var_beta="matrix",
     t_val="vector"
     ),
    methods = list(
      initialize = function(formula, data){
          formula<<-formula
          data<<-data
          X<<-model.matrix(formula,data)  # X, matrix containing all the data
          Y<<-as.matrix(data[all.vars(formula)[1]])  # Y, vector containing response variable
          reg_coe<<-solve((t(X)%*%X))%*%t(X)%*%Y  # Estimation of the regression coefficients
          names(reg_coe) <<- colnames(X)
          fit_val<<-X%*%reg_coe  # Estimation of the Y values
          resid_e<<-Y-fit_val  # Estimation of the error variable
          n<<-NROW(X)   # Number of data
          p<<-NCOL(X)   # Number of variables
          df<<-n-p   # Degrees of freedom
          resid_var<<-as.numeric((t(resid_e)%*%resid_e)/df)  # Estimates of the variance of the error variable
          var_beta<<-resid_var*solve(t(X)%*%X)  # Estimates the variability of the beta coefficients
          t_val<<-reg_coe/sqrt(diag(var_beta))   # T-values for significance of coefficients
          dname <<- deparse(substitute(data))
        }
        ,
      print = function(){
        cat("call:","\n")
        right_formula = paste(all.vars(formula)[-1], collapse = " + ")
        cat("linreg(formula = ", all.vars(formula)[1], " ~ ", right_formula,
                   ", data = ", dname,")", "\n", sep ="")

        #cat(all.vars(formula)[1], " ~ ", paste(all.vars(formula)[-1],sep="+"),"\n")

        cat("coefficients:","\n")
        cat(format(labels(reg_coe), width=25, justify = "right"), "\n")
        cat(format(reg_coe, width=25, justify = "right"))


        #cat(reg_coe, labels = T)
      },
      plot = function(){
        require(ggplot2)
        data_plot <- data.frame(fit_val, resid_e, stand_res = sqrt(abs(resid_e/sd(resid_e))))
        p1 = ggplot(data_plot, aes(x=fit_val, y=resid_e)) +
          geom_point(shape = 1) +
          geom_smooth(se = FALSE, color = "red") +
          ggtitle("Residual vs Fitted") +
          xlab(paste0("Fitted values\nlm( ", all.vars(formula)[1], " ~ ",
                      paste(all.vars(formula)[-1], sep = " + "), " )", sep = "")) +
          ylab("Residuals") +
          theme_light()
        p2 = ggplot(data_plot, aes(x=fit_val, y=stand_res)) +
          geom_point(shape = 1) +
          geom_smooth(se = FALSE, color = "red") +
          ggtitle("Scale - Location") +
          xlab(paste0("Fitted values\nlm( ", all.vars(formula)[1], " ~ ",
                      paste(all.vars(formula)[-1], sep = " + "), " )", sep = "")) +
          ylab(expression(sqrt("|Standardize residuals|"))) +
          theme_light()
        readline(prompt = "Press <Return> to see next plot:")
        print(p1)
        readline(prompt = "Press <Return> to see next plot:")
        print(p2)
      },
      resid = function(){
        return(resid_e)
      },
      pred = function(){
        return(fit_val)
      },
      coef = function(){

        return(reg_coe)
      },
      summary = function(){
        l <- list()
        m = matrix(NA,p,4)
        m[,1] = reg_coe
        m[,2] = sqrt(diag(var_beta)/n)
        m[,3] = t_val
        m[,4] = pt(t_val, df)
        colnames(m) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
        rownames(m) <- colnames(X)
        l$matrix <- m
        l$variance <- resid_var
        cat(l)
      }
      ))


