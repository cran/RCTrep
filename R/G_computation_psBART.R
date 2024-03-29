#' @import geex
#' @import ggplot2
#' @import fastDummies
#' @import BART
G_computation_psBART <- R6::R6Class(
  "G_computation_psBART",
  inherit = TEstimator,
  #-------------------------public fields-----------------------------#
  public = list(

    resi = NULL,

    ps.est = NULL,

    id = "G_computation",

    initialize = function(df, vars_name, name,
                          gc.method, gc.formula,
                          var_approach = "Bias_adjusted", isTrial, ...) {
      #browser()
      super$initialize(df, vars_name, name)
      private$gc.method <- gc.method
      private$gc.formula <- gc.formula
      private$var_approach <- var_approach
      private$confounders_treatment_factor <- private$outcome_predictors[sapply(self$data[,private$outcome_predictors],
                                                                                        is.factor)]
      self$ps.est <- private$fit_treatment(...)
      self$data$ps <- self$ps.est
      self$model <- private$fit(...)
      po_mean_var <- private$est_potentialOutcomes_mean_var()
      self$data$y1.hat <- po_mean_var$y1.hat.mean
      self$data$y0.hat <- po_mean_var$y0.hat.mean
      self$data$y1.hat.var <- po_mean_var$y1.hat.var
      self$data$y0.hat.var <- po_mean_var$y0.hat.var
      self$data$ite.var <- self$data$y1.hat.var + self$data$y0.hat.var
      self$resi <- private$est_residual()
      private$set_ATE()
      private$set_CATE(private$outcome_predictors,TRUE)
      private$isTrial <- isTrial
      self$id <- paste(self$id, private$gc.method, sep = "/")
    },

    diagnosis_t_ignorability = function(stratification, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        stratification <- private$outcome_predictors
      }

      residuals.overall <- mean(self$resi)
      residuals.subgroups <- private$aggregate_residual(stratification)

      if(test_binary(self$data[,private$outcome_name])){
        colnames.subgroups <- colnames(residuals.subgroups)
        var_names <- colnames.subgroups[!colnames.subgroups %in% c("sample.size","res.mean", "res.se")]
        var_names_data <- residuals.subgroups[,var_names]
        subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
        subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)

        df <- residuals.subgroups %>%
          select(res.mean, res.se, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=res.mean-1.98*res.se,
                 ci_u=res.mean+1.98*res.se)

        plot.res <- ggplot2::ggplot(data = df, aes(x = res.mean, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("model fit: mean of squared residual (1.98se)")+
          theme(plot.title = element_text(),
                legend.position = "none")

        out <- list(residuals.overall = residuals.overall,
                    residuals.subgroups = residuals.subgroups,
                    plot.res = plot.res)

      } else {
        colnames.subgroups <- colnames(residuals.subgroups)
        var_names <- colnames.subgroups[!colnames.subgroups %in% c("sample.size","res.mean", "res.se","msr","sesr")]
        var_names_data <- residuals.subgroups[,var_names]
        subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
        subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)

        df <- residuals.subgroups %>%
          select(res.mean, res.se, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=res.mean-1.98*res.se,
                 ci_u=res.mean+1.98*res.se)

        plot.res.subgroup <- ggplot2::ggplot(data = df, aes(x = res.mean, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("mean(1.98se) residual")+
          theme(plot.title = element_text(),
                legend.position = "none")

        df <- residuals.subgroups %>%
          select(msr, sesr, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=msr-1.98*sesr,
                 ci_u=msr+1.98*sesr)

        plot.msr.subgroup <- ggplot2::ggplot(data = df, aes(x = msr, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("mean squared error")+
          theme(plot.title = element_text(),
                legend.position = "none")

        df <- data.frame(residual=self$resi)
        plot.res.overall <- ggplot(data = df, aes(x=residual)) + geom_density()

        plot.res.mse <- ggpubr::ggarrange(plot.res.subgroup, plot.res.overall, plot.msr.subgroup, ncol = 3, nrow = 1)

        out <- list(residuals.overall = residuals.overall,
                    residuals.subgroups = residuals.subgroups,
                    plot.res.mse = plot.res.mse)
      }

      out

    }
  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    gc.method = NULL,
    gc.formula = NULL,
    var_approach = "Bayesian",
    iterations = 1,
    confounders_treatment_factor = NULL,

    fit = function(...) {
      #browser()
      x.train <- self$data[, c(private$outcome_predictors, private$treatment_name,"ps")]
      if(length(private$confounders_treatment_factor)>0){
        x.train <- fastDummies::dummy_cols(x.train, select_columns= private$confounders_treatment_factor,
                                           remove_selected_columns = TRUE)
      }
      x.train <- as.matrix(x.train)
      y.train <- as.matrix(self$data[, private$outcome_name])
      if (length(unique(self$data[, private$outcome_name]))>2) {
        model <- BART::wbart(x.train=x.train, y.train = y.train, ...)
      } else {
        model <- BART::pbart(x.train=x.train, y.train = y.train, ...)
      }
      return(model)
    },

    fit_treatment = function(...) {
      #browser()
      x.train <- self$data[, c(private$outcome_predictors)]
      if(length(private$confounders_treatment_factor)>0){
        x.train <- fastDummies::dummy_cols(x.train, select_columns= private$confounders_treatment_factor,
                                           remove_selected_columns = TRUE)
      }
      x.train <- as.matrix(x.train)
      y.train <- as.matrix(self$data[,private$treatment_name])
      if (length(unique(self$data[, private$treatment_name]))>2) {
        message("we don't have the function for more than 2 arms yet")
        model <- BART::wbart(x.train=x.train, y.train = y.train, ...)
      } else {
        model <- BART::pbart(x.train=x.train, y.train = y.train, ...)
        prob.train <- pnorm(model$yhat.train)
        ps <- apply(prob.train,2,mean)
      }
      return(ps)
    },

    est_ATE_SE = function(index) {
      #browser()
      n <- length(index)
      cate <- self$data$y1.hat[index] - self$data$y0.hat[index]
      y1.hat.mu <- mean(self$data$y1.hat[index])
      y0.hat.mu <- mean(self$data$y0.hat[index])
      est <- y1.hat.mu - y0.hat.mu
      var.within <- mean(self$data$ite.var)
      var.between <- var(cate)
      se <- sqrt((var.within+var.between)/n)
      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      #browser()
      y1.hat <- self$data$y1.hat[index]*weight
      y0.hat <- self$data$y0.hat[index]*weight

      data <- as.data.frame(cbind(y1.hat, y0.hat))
      colnames(data) <- c("y1.hat","y0.hat")
      results <- m_estimate(estFUN = private$gc_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
      y1.hat.mu <- results@estimates[1]
      y0.hat.mu <- results@estimates[2]
      est <- results@estimates[3]
      se <- sqrt(results@vcov[3,3])
      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    gc_estfun = function(data){
      #browser()
      Y1 <- data$y1.hat
      Y0 <- data$y0.hat
      function(theta) {
        c(Y1 - theta[1],
          Y0 - theta[2],
          theta[1]-theta[2] - theta[3]
        )
      }
    },

    # compute deviance for continuous outcome/binary outcome
    est_residual = function() {
      #browser()
      y <- self$data[,private$outcome_name]
      if (length(unique(self$data[, private$outcome_name]))>2) {
        y.hat <- self$model$yhat.train.mean
        resi <- (y-y.hat)^2
      } else {
        y.hat <- self$model$prob.train.mean
        resi <- (y-y.hat)^2
        #y.hat.0 <- 1-y.hat.1
        #resi <- -2 * (y * log(y.hat.1) + (1-y)*log(y.hat.0))
      }
      return(resi)
    },

    est_potentialOutcomes_mean_var = function() {
      #browser()
      data0 <- data1 <- self$data[, c(private$outcome_predictors, private$treatment_name,"ps")]
      data0[, private$treatment_name] <- 0
      data1[, private$treatment_name] <- 1
      if(length(private$confounders_treatment_factor)>0){
        data0 <- fastDummies::dummy_cols(data0, select_columns= private$confounders_treatment_factor,
                                         remove_selected_columns = TRUE)
        data1 <- fastDummies::dummy_cols(data1, select_columns= private$confounders_treatment_factor,
                                         remove_selected_columns = TRUE)
      }
      data0 <- as.matrix(data0)
      data1 <- as.matrix(data1)

      if(length(unique(self$data[, private$outcome_name]))>2){
        y1.hat <- predict(self$model, newdata = data1)
        y0.hat <- predict(self$model, newdata = data0)
        y1.hat.mean <- apply(y1.hat, 2, mean)
        y0.hat.mean <- apply(y0.hat, 2, mean)
        y1.hat.var <- apply(y1.hat, 2, var)
        y0.hat.var <- apply(y0.hat, 2, var)
      } else {
        y1.hat <- predict(self$model, newdata=data1)$prob.test
        y0.hat <- predict(self$model, newdata=data0)$prob.test
        y1.hat.mean <- apply(y1.hat, 2, mean)
        y0.hat.mean <- apply(y0.hat, 2, mean)
        y1.hat.var <- apply(y1.hat, 2, var)
        y0.hat.var <- apply(y0.hat, 2, var)
      }
      return(list(y1.hat.mean = y1.hat.mean,
                  y0.hat.mean = y0.hat.mean,
                  y1.hat.var = y1.hat.var,
                  y0.hat.var = y0.hat.var))
    },

    aggregate_residual = function(stratification) {
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      if(test_binary(self$data[,private$outcome_name])){
        res.mean <- res.se <- sample.size <- NULL
        for (i in seq(n_groups)) {
          # for binary outcome, how to evaluate model fit?
          subgroup.id.in.data <- self$data[group_id == i, "id"]
          res.mean[i] <- sum(self$resi[subgroup.id.in.data])/group_sample_size[i]
          res.se[i] <- sqrt(var(self$resi[subgroup.id.in.data])/group_sample_size[i])
          sample.size[i] <- group_sample_size[i]
        }
        res <- cbind(group_strata, sample.size, res.mean, res.se)
      } else {
        res.mean <- res.se <- sample.size <- msr <- sesr <- NULL
        for (i in seq(n_groups)) {
          subgroup.id.in.data <- self$data[group_id == i, "id"]
          res.mean[i] <- sum(self$resi[subgroup.id.in.data])/group_sample_size[i]
          res.se[i] <- sqrt(var(self$resi[subgroup.id.in.data])/group_sample_size[i])
          # mean of squared residual
          msr[i] <- sum((self$resi[subgroup.id.in.data])^2)/group_sample_size[i]
          sesr[i] <- sqrt(var((self$resi[subgroup.id.in.data])^2)/group_sample_size[i])
          sample.size[i] <- group_sample_size[i]
        }
        res <- cbind(group_strata, sample.size, res.mean, res.se, msr, sesr)
      }
      res <- as.data.frame(res)
      return(res)
    }
  )
)
