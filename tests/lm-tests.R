###-- Linear Models, basic functionality -- weights included.

## From John Maindonald :
roller <- data.frame(
 weight     = c(1.9,  3.1,  3.3,  4.8,  5.3,  6.1,  6.4,  7.6,  9.8, 12.4),
 depression = c(  2,  1,    5,    5,   20,   20,   23,   10,   30,   25))

roller.lmu  <-  lm(weight~depression, data=roller)
roller.lsfu <- lsfit(roller$depression, roller$weight)

roller.lsf  <- lsfit(roller$depression, roller$weight, wt = 1:10)
roller.lsf0 <- lsfit(roller$depression, roller$weight, wt = 0:9)
roller.lm  <-  lm(weight~depression, data=roller, weights= 1:10)
roller.lm0 <-  lm(weight~depression, data=roller, weights= 0:9)
roller.lm9 <-  lm(weight~depression, data=roller[-1,],weights= 1:9)
roller.glm <- glm(weight~depression, data=roller, weights= 1:10)
roller.glm0<- glm(weight~depression, data=roller, weights= 0:9)

all.equal(deviance(roller.lm),
          deviance(roller.glm))
all.equal(weighted.residuals(roller.lm),
          residuals         (roller.glm))

all.equal(deviance(roller.lm0),
          deviance(roller.glm0))
all.equal(weighted.residuals(roller.lm0, drop=FALSE),
          residuals         (roller.glm0))

(im.lm0 <- influence.measures(roller.lm0))

all.equal(unname(im.lm0 $ infmat),
          unname(cbind(  dfbetas      (roller.lm0)
                       , dffits       (roller.lm0)
                       , covratio     (roller.lm0)
                       ,cooks.distance(roller.lm0)
                       ,lm.influence  (roller.lm0)$hat)
                 ))

all.equal(rstandard(roller.lm9),
          rstandard(roller.lm0),tol=1e-15)
all.equal(rstudent(roller.lm9),
          rstudent(roller.lm0),tol=1e-15)

all.equal(summary(roller.lm0)$coef,
          summary(roller.lm9)$coef, tol=1e-15)
all.equal(print(anova(roller.lm0), signif.st=FALSE),
                anova(roller.lm9), tol=1e-15)
