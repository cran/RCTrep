SEstimator <- R6::R6Class(
  "SEstimator",
  #-------------------------public fields-----------------------------#
  public = list(
    name = character(),
    id = character(),

    statistics = list(),
    estimates = list(ATE = data.frame(y1.hat=NA,
                                      y0.hat=NA,
                                      est=NA,
                                      se=NA),
                     CATE = data.frame()),
    model = NA,
    selection_predictors = NA,
    weighting_method = character(),

    initialize = function(target.obj, source.obj, weighting_method=NULL,
                          selection_predictors){
      private$target.obj <- target.obj
      private$source.obj <- source.obj
      self$weighting_method <- weighting_method
      self$selection_predictors <- selection_predictors
      private$ispublic <- !c("TEstimator_pp") %in% class(source.obj)
      self$name <- source.obj$name
      self$statistics <- source.obj$statistics
      self$id <- paste(private$source.obj$id,
                       self$weighting_estimator,
                       length(self$selection_predictors),sep = '/')
      private$isTrial <- source.obj$.__enclos_env__$private$isTrial
    },

    EstimateRep = function(stratification=self$selection_predictors, stratification_joint=TRUE) {
      #browser()
      private$set_weighted_ATE_SE()

      if(stratification_joint==FALSE){
        message("since stratitification is FALSE, then for each strata,
                we will balance selection_predictors which are not used to stratify!")
        private$set_weighted_CATE_SE(stratification = stratification,
                                     stratification_joint = FALSE)
      } else{
        if (all(self$selection_predictors %in% stratification)==TRUE){
          message("since selection_predictors is a subset of stratification,
                  in each strata, the weight for each individual is 1")
          self$estimates$CATE <- private$source.obj$get_CATE(stratification = stratification,
                                                             stratification_joint = TRUE)
          #self$estimates$CATE <- cbind(self$estimates$CATE,'pt','py')
        } else {
          message("since selection_predictors and stratification is overlapped,
                  in each strata, the weight for each individual according to variables
                  in setdiff(selection_predictors,stratificaiton), i.e., the variables in set selection_predictors
                  while not in the set stratificaiton.")
          private$set_weighted_CATE_SE(stratification = stratification,
                                       stratification_joint = TRUE)
        }
      }
    },

    diagnosis_s_overlap = function(stratification=NULL, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- self$selection_predictors
      } else{
        vars_name <- stratification
      }
      if(stratification_joint){
        source.data <- private$source.obj$data %>%
          select(vars_name) %>%
          mutate(study=private$source.obj$name)
        target.data <- private$target.obj$data %>%
          select(vars_name) %>%
          mutate(study=private$target.obj$name)

        data <- rbind(source.data, target.data) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ",")))

        #data <- bind_rows(source.data, target.data) %>%
        #  mutate(group_name = apply(.[,vars_name], 1, function(x)
        #    paste(vars_name,x,sep = "=",collapse = ",")))

        p.prop <- ggplot(data = data, aes(x=group_name, fill=study)) +
          geom_bar(position = "fill") +
          ylab("proportion") +
          coord_flip() +
          theme(legend.position="none")

        p.count <- ggplot(data = data, aes(x=group_name, fill=study)) +
          geom_bar(stat = "count") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())
      } else{
        stop("stratification_joint must be TRUE for now, the function has not been completed yet.")
      }

      #print(data)

      ggpubr::ggarrange(p.prop, p.count, nrow=1, ncol=2)

    },

    # class SEstimator_pp has unique implementation
    diagnosis_s_ignorability = function(stratification=NULL, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- self$selection_predictors
      } else{
        vars_name <- stratification
      }

      message("to be continued... This function is to check if
              confounders_sampling are balanced between source and target object
              on population and sub-population levels stratified by
              stratificaiton and stratification_joint.")

      weight <- private$get_weight(
        source = private$source.obj$data,
        target = private$target.obj$data,
        vars_weighting = self$selection_predictors
      )

      p.source <- private$source.obj$data %>%
        bind_cols(weight = weight) %>%
        group_by(across(all_of(self$selection_predictors))) %>%
        summarise(size.agg=sum(weight)) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg),
               study=private$source.obj$name)

      p.target <- private$target.obj$data %>%
        group_by(across(all_of(self$selection_predictors))) %>%
        summarise(size.agg=n()) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg),
               study=private$target.obj$name)

      p.combined <- rbind(p.source, p.target) %>%
        mutate(group_name = apply(.[,vars_name], 1, function(x)
          paste(vars_name,x,sep = "=",collapse = ","))) %>%
        ggplot(aes(x=group_name, y=prop, fill=study)) +
        geom_bar(stat='identity', position='dodge') +
        ylab("proportion") +
        coord_flip() +
        #title("Balance of covariates within sub-popultions between samples") +
        theme(legend.position="right")

      p.combined

    }

  ),

  private = list(
    source.obj = NA,
    target.obj = NA,
    ispublic = NA,
    isTrial = NA,

    get_weight = function(source.data,target.data, vars_weighting){},

    set_weighted_ATE_SE = function() {
      #browser()
      weight <- private$get_weight(
        source = private$source.obj$data,
        target = private$target.obj$data,
        vars_weighting = self$selection_predictors
      )

      ATE_se_weighted <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(private$source.obj$data$id,weight)
      self$estimates$ATE$y1.hat <- ATE_se_weighted$y1.hat
      self$estimates$ATE$y0.hat <- ATE_se_weighted$y0.hat
      self$estimates$ATE$est <- ATE_se_weighted$est
      self$estimates$ATE$se <- ATE_se_weighted$se
    },

    set_weighted_CATE_SE = function(stratification, stratification_joint) {
      if (stratification_joint) {
        self$estimates$CATE <- private$est_WeightedCATEestimation4JointStratification(stratification)
        } else {
          self$estimates$CATE <- private$est_WeightedCATEestimation4SeperateStratification(stratification)
          }
    },

    est_WeightedCATEestimation4JointStratification = function(stratification) {
      #browser()
      cate <- se <- size <- y1.hat <- y0.hat <- NULL
      vars_weighting <- self$selection_predictors
      vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% stratification]

      group_data <- private$source.obj$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      cate <- se <- y1.hat <- y0.hat <- NULL
      pattern.type <- sapply(private$target.obj$data[, stratification], class)

      for (i in seq(n_groups)) {
        pattern <- group_strata[i, ]
        subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern, pattern.type)
        target.subgroup.data <- subset(private$target.obj$data, eval(parse(text = subgroup_selection_condition)))
        source.subgroup.data <- private$source.obj$data[group_id == i, ]
        source.subgroup.id.in.data <- private$source.obj$data[group_id == i, "id"]

        weight <- private$get_weight(
          source = source.subgroup.data,
          target = target.subgroup.data,
          vars_weighting = vars_weighting_subgroup
        )

        cate_y1_y0_se <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
        y1.hat[i] <- cate_y1_y0_se$y1.hat
        y0.hat[i] <- cate_y1_y0_se$y0.hat
        cate[i] <- cate_y1_y0_se$est
        se[i] <- cate_y1_y0_se$se
        size[i] <- ifelse(private$ispublic, dim(source.subgroup.data)[1],
                          sum(source.subgroup.data$size))
        #size[i] <- ifelse(private$ispublic, ((sum(weight))^{2}) / (sum(weight^{2})), sum(weight))
      }
      CATE_mean_se <- cbind(group_strata, y1.hat, y0.hat, cate, se, size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      return(CATE_mean_se)
    },

    est_WeightedCATEestimation4SeperateStratification = function(stratification) {
      #browser()
      vars_weighting <- self$selection_predictors
      group_var <- group_level <- cate <- se <- size <- y1.hat <- y0.hat <- NULL
      i <- 1
      for (var_name in stratification) {
        vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% var_name]
        group_data <- private$source.obj$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        #if(!("Synthetic_TEstimator" %in% class(private$target.obj))){
        pattern.type <- class(private$target.obj$data[, var_name])
        #}

        for (group_id in seq(n_groups)) {
          var_level <- group_strata[group_id, 1]
          source.subgroup.id.in.data <- private$source.obj$data[group_id_4each_obs == group_id, "id"]
          source.subgroup.data <- private$source.obj$data[source.subgroup.id.in.data, ]
          #if(!("Synthetic_TEstimator" %in% class(private$target.obj))){
          subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern = group_strata[group_id, ], pattern.type)
          target.subgroup.data <- subset(private$target.obj$data, eval(parse(text = subgroup_selection_condition)))
          #} else{
          #  target.subgroup.data <- private$target.obj$data[(private$target.obj$data$name==var_name) & (private$target.obj$data$value==as.numeric(var_level)),]
          #}

          group_var[i] <- var_name
          group_level[i] <- var_level

          if(length(vars_weighting_subgroup)==0){
            cate_y1_y0_se <- private$source.obj$.__enclos_env__$private$est_ATE_SE(source.subgroup.id.in.data)
          } else {
            weight <- private$get_weight(
              source = source.subgroup.data,
              target = target.subgroup.data,
              vars_weighting = vars_weighting_subgroup
            )
            cate_y1_y0_se <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
          }
            y1.hat[i] <- cate_y1_y0_se$y1.hat
            y0.hat[i] <- cate_y1_y0_se$y0.hat
            cate[i] <- cate_y1_y0_se$est
            se[i] <- cate_y1_y0_se$se
            size[i] <- ifelse(private$ispublic, dim(source.subgroup.data)[1],
                              sum(source.subgroup.data$size))
            #size[i] <- ifelse(private$ispublic, ((sum(weight))^{2}) / (sum(weight^{2})), sum(weight))
            i <- i + 1
        }
      }
      # the output element from group_keys() is not a vector/numeric, hence needs to convert to data.frame reshape(4*1)
      group_level <- t(as.data.frame(group_level))
      CATE_mean_se <- data.frame(
        name = group_var,
        value = group_level,
        y1.hat = y1.hat,
        y0.hat = y0.hat,
        cate = cate,
        se = se,
        size = size,
        stringsAsFactors = FALSE
      )

      return(CATE_mean_se)
    },

    est_statistics = function(){

    }
  )
)
