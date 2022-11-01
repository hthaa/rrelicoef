#' Raykov's factor reliability coefficient
#'
#' This function computes Raykov's reliability coefficient (RRC) for factors from
#' confirmatory factor analyses, a measure which is commonly seen as a more
#' accurate one than that of Cronbach's alpha which is computed based on the
#' assumption of tau-equivalent measures. It computes reliability coefficients
#' for factors with and without correlated errors.
#'
#' @param mod Lavaan CFA model object.
#' @param reverse Logical. Whether to turn negative factor loadings positive (i.e., calculate RRC as if items with negative factor loadings were reverse-scored.)
#'
#' @examples
#' model.01 <- '
#'            Col =~ respected + secure
#'            Ind =~ accomplish + self_fulfil + self_respect
#'            '
#' est.model.01 <- cfa(model.01, data=values)
#' rrelicoef(est.model.01)
#'
#' @export
rrelicoef <- function(mod, reverse = FALSE){
  latvarname <- lavNames(mod, type="lv")
  obsvarname <- lavNames(mod, type="ov")
  parest <- parameterestimates(mod)
  if (reverse) {
    parest[, "est"] <- abs(parest[, "est"])
  }
  idest <- parest$lhs %in% latvarname & !parest$rhs %in% latvarname
  sumlambdasq <- with(parest[idest,],
                      tapply(est, lhs, sum)^2)
  idest2 <- parest$lhs %in% latvarname & (parest$lhs == parest$rhs)
  factorvar <- with(parest[idest2,],
                    tapply(est, lhs, sum))
  ll=list()
  ll2=list()
  for(lvar in latvarname){
    ix=parest$lhs==lvar & parest$rhs %in% obsvarname
    ovars=parest[ix,"rhs"]
    ix=parest$lhs %in% ovars & parest$rhs==parest$lhs
    ll[[lvar]]=sum(parest[ix,]$est)

    ix=parest$lhs %in% ovars & parest$rhs %in% obsvarname & parest$lhs!=parest$rhs
    ll2[[lvar]]=sum(parest[ix,]$est)
  }
  errvar=unlist(ll)
  corrErr=unlist(ll2)

  # ensure that terms are in same order
  sumlambdasq <- sumlambdasq[latvarname]
  factorvar <- factorvar[latvarname]
  errvar <- errvar[latvarname]
  corrErr <- corrErr[latvarname]

  RRC=(sumlambdasq*factorvar)/(sumlambdasq*factorvar+errvar+2*corrErr)
  data.frame(
    Latent=names(RRC),
    RRC=as.vector(RRC)
  )
}

meas.lpa.mod <- '
Attractive =~ face + sexy
Appearance =~ body + appear + attract
Muscle =~ muscle + strength + endur
Weight =~ lweight + calories + cweight
'

library(haven)
workout2 <- read_dta("workout2.dta")
library(lavaan)
est.meas.lpa.mod <- cfa(meas.lpa.mod, data=workout2)
summary(est.meas.lpa.mod, fit.measures=TRUE, standardized=TRUE)

rrelicoef(est.meas.lpa.mod)
