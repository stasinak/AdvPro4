linreg <- setRefClass("linreg",
   fields = list(
     X="matrix",
     Y="matrix",
     reg_coe="vector",
     fit_val="vector",
     resid_e="vector",
     deg_freed="numeric",
     resid_var="vector",
     var_beta="vector",
     t_val="vector"





     ),
    methods = list(
      initialize = function(formula,data){
       X<<-model.matrix(formula,data)
       Y<<-data[all.vars(formula)[1]]
       reg_coe<<-solve((t(X)%*%X))%*%t(X)%*%Y
       fit_val<<-X%*%reg_coe
       resid_e<<-Y-fit_val},
      print_out= function(){
        cat()
      }




     ))

lin<-linreg$print_out()

