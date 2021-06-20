
as_ram <- function(model,
                   int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
                   auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
                   auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE, ...){
    defaults <- list(int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
                     auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,
                     auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE)
    cl <- match.call()
    cl[names(defaults)[!names(defaults) %in% names(cl)]] <- defaults[!names(defaults) %in% names(cl)]
    cl[[1L]] <- str2lang("lavaan::lavaanify")
    lavtab <- eval.parent(cl)
    # Remove defined parameters
    lavtab <- lavtab[!(lavtab$group == 0), ]
    # Starting values
    #lavtab$ustart[lavtab$op == "~1"] <- 0
    #lavtab$ustart[lavtab$op == "~~"] <- .5
    # Identify observed and latent
    vnames <- lavaan:::vnames(partable = lavtab, type = "all")
    latent <- unlist(vnames[["lv"]])
    obs <- unlist(vnames[["ov"]])
    # Intercept needs rhs
    lavtab$rhs[lavtab$op == "~1"] <- "one"
    lavtab$op[lavtab$op == "~1"] <- "~"
    # Convert lavtab to paths
    path_list <- lapply(1:nrow(lavtab), function(i){
        path <- lavtab[i, ]
        Args <- list(
            name = "mxPath",
            from = switch(path[["op"]],
                          "=~" = path[["lhs"]],
                          path[["rhs"]]),
            to = switch(path[["op"]],
                        "=~" = path[["rhs"]],
                        path[["lhs"]]),
            connect = "single",
            arrows = switch(path[["op"]],
                            `~~` = 2,
                            1),
            free = !(path[["free"]] == 0),
            values = path[["ustart"]]
        )
        if(!path[["label"]] == "") Args$labels <- path[["label"]]
        do.call(call, Args)
    })
    do.call(mxModel, c(list(model = "model",
                            type='RAM',
                            manifestVars = obs,
                            latentVars = latent),
                       path_list))
}
