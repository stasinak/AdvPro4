linreg <- setRefClass("linreg",
  fields = list(
    X="matrix",
    Y="vector",
    reg_coe="vector",
    fit_val="vector",
    resid_e="vector",
    deg_freed="numeric",
    resid_var="vector",
    var_beta="vector",
    t_val="vector"),
  methods = list(
    initialize <-function(formula,data){
      X<<-model.matrix(formula,data)
      Y<<-data[all.vars(formula)[1]]
      reg_coe<<-solve((t(X)%*%X))(a%*%t(X))%*%Y
      fit_val<<-X%*%reg_coe
      resid_e<<-Y-fit_val},
    xar<<-function(){
      cat(X)
  }


                      )
)

data("iris")

linreg_mod = linreg$new(Petal.Length~Sepal.Width+Sepal.Length, iris)



