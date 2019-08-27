# internal function to extract and assign all information from a reduced form var object
# to be used in svar identification functions

get_var_objects <- function(x){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  if(inherits(x, "var.boot")){
    assign("u", x$residuals,envir = parent.frame())
    assign("Tob", nrow(x$residuals), envir = parent.frame())
    assign("k", ncol(x$residuals), envir = parent.frame())
    assign("residY", x$residuals, envir = parent.frame())
  }else{
    assign("u", residuals(x), envir = parent.frame())
    assign("Tob", nrow(residuals(x)), envir = parent.frame())
    assign("k", ncol(residuals(x)), envir = parent.frame())
    assign("residY", residuals(x), envir = parent.frame())

  }

  if(inherits(x, "var.boot")){
    assign("p", x$p, envir = parent.frame())
    assign("y", t(x$y), envir = parent.frame())
    assign("yOut", x$y, envir = parent.frame())
    assign("type", x$type, envir = parent.frame())
    assign("coef_x", x$coef_x, envir = parent.frame())

    }else if(inherits(x, "varest")){
      assign("p", x$p, envir = parent.frame())
      assign("y", t(x$y), envir = parent.frame())
      assign("yOut", x$y, envir = parent.frame())
      assign("type", x$type, envir = parent.frame())
      assign("coef_x", coef(x), envir = parent.frame())

  }else if(inherits(x, "nlVar")){
    assign("p", x$lag, envir = parent.frame())
    assign("k", x$k, envir = parent.frame())
    assign("y", t(x$model[, 1:x$k]), envir = parent.frame())
    assign("yOut", x$model[, 1:x$k], envir = parent.frame())
    assign("coef_x", t(coef(x)), envir = parent.frame())
    coef_x = t(coef(x))
    if(inherits(x, "VECM")){
      assign("coef_x", t(VARrep(x)), envir = parent.frame())
      coef_x = t(VARrep(x))
    }

    if(rownames(coef_x)[1] %in% c("Intercept", "constant")){
      assign("coef_x", coef_x[c(2:nrow(coef_x),1),], envir = parent.frame())
      coef_x = coef_x[c(2:nrow(coef_x),1),]

    }else if(rownames(coef_x)[1] == "Trend"){
      assign("coef_x", coef_x[c(2:nrow(coef_x),1),], envir = parent.frame())
      coef_x = coef_x[c(2:nrow(coef_x),1),]
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "constant", "Trend")){
      assign("coef_x", coef_x[c(2:nrow(coef_x),1),], envir = parent.frame())
      coef_x = coef_x[c(2:nrow(coef_x),1),]
    }
    assign("type", x$include, envir = parent.frame())
    assign("coef_x", split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x))), envir = parent.frame())
    assign("coef_x", lapply(coef_x, as.matrix), envir = parent.frame())

  }else if(inherits(x, "list")){
    assign("p", x$order, envir = parent.frame())
    assign("y", t(x$data), envir = parent.frame())
    assign("coef_x", x$coef, envir = parent.frame())

    if(x$cnst == TRUE){
      assign("coef_x", coef_x[c(2:nrow(coef_x),1),], envir = parent.frame())
      assign("type", "const", envir = parent.frame())
    }
    assign("coef_x", split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x))), envir = parent.frame())
    assign("coef_x", lapply(coef_x, as.matrix), envir = parent.frame())

  }else if(inherits(x, "vec2var")){
    assign("k", ncol(x$resid), envir = parent.frame())
    k <- ncol(x$resid)
    coef_x <- vector("list", length = k)
    names(coef_x) <- colnames(x$y)
    assign("coef_x", coef_x, envir = parent.frame())
    assign("yOut", x$y, envir = parent.frame())
    p <- x$p
    assign("p", x$p, envir = parent.frame())
    assign("y", t(x$y), envir = parent.frame())

    for (i in seq_len(k)) {
      for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
      coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
    }
    assign("coef_x", lapply(coef_x, matrix), envir = parent.frame())
    assign("type", "const", envir = parent.frame())

  }else{
    stop("Object class is not supported")
  }


}
