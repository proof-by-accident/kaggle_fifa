library(glmnet)
library(ROCR)

dat <- read.csv('fifa.csv')

any(is.na(dat$Date))
any(is.na(dat$Team))
any(is.na(dat$Opponent))

names(dat)[5] <- 'Ball.Possession.per'
names(dat)[14] <- 'Pass.Accuracy.per'

row_skip <- seq(1, dim(dat)[1], 2)

better.dat <- data.frame( target = as.integer( dat$Man.of.the.Match[row_skip] == 'Yes' ),
                         date = dat$Date[row_skip],
                         round = dat$Round[row_skip],
                         team1 = dat$Team[row_skip],
                         team2 = dat$Opponent[row_skip],
                         team1.ball.poss.per = dat$Ball.Possession.per[row_skip],
                         team1.pass.acc.per = dat$Pass.Accuracy.per[row_skip]
                         
                         )

for (col in names(dat)){

    if ( col %in% c('Opponent','Round','Man.of.the.Match','Date','Team','Ball.Possession.per','Pass.Accuracy.per') ){

    }

    else {    
        team1.stat <- dat[,col][ row_skip ]
        team2.stat <- dat[,col][ row_skip + 1 ]
        
        better.dat[ ,c( paste0( 'team1.', tolower(col) ) )] <- team1.stat
        better.dat[ ,c( paste0( 'team2.', tolower(col) ) )] <- team2.stat

    }
            
}

better.dat$team1.own.goals[ is.na(better.dat$team1.own.goals) ] <- 0
better.dat$team2.own.goals[ is.na(better.dat$team2.own.goals) ] <- 0

cols.w.nas <- names(better.dat)[ which( apply( better.dat, 2, function(c){any(is.na(c))} ) ) ]
cols.w.factors <- names(better.dat)[ which( !unlist( lapply( better.dat, is.numeric ) ) ) ]

drop.cols <- c( 'date', 'team1', 'team2', cols.w.nas, cols.w.factors, 'team1.yellow.card', 'team2.yellow.card', 'team1.red', 'team2.red', 'team1.yellow...red', 'team2.yellow...red'  )

x <- better.dat[, !names(better.dat) %in% drop.cols ]

# we should normalize these covariates, and replace indiv stats for team1 and team2 with the difference between the teams' stats

team1.cols <- seq(4,ncol(x),2)
x <- x[,team1.cols] - x[,team1.cols+1]
x$team1.goals.tot <- x$team1.goal.scored + x$team1.goals.in.pso
x <- as.data.frame( apply(x,2,function(c){ (c-mean(c))/sd(c) }) )
names(x) <- sapply( names(x), function(s) { strsplit(s,'1.')[[1]][2] } )
x <- as.matrix( x )

# split data for ROC curve
train.size <- nrow(x) * .3
data.split <- sort(sample(1:nrow(x),train.size))

x.train <- x[-data.split,]
x.test <- x[data.split,]

y <- as.matrix( better.dat$target )
y.train <- y[-data.split]
y.test <- y[data.split]

cvm = 'class'
nf = 10

# Cross Val 
mod <- cv.glmnet(x.train,
                 y.train,
                 alpha = 1,
                 family = 'binomial',
                 type.measure = cvm,
                 nfolds = nf,
                 grouped = FALSE
                 )

best.lam <- mod$lambda.min
best.coefs <- mod$glmnet.fit$beta[ , which( mod$lambda == best.lam ) ]
best.cvm <- mod$cvm[ which( mod$lambda == best.lam ) ]

se.lam <- mod$lambda.1se
se.coefs <- mod$glmnet.fit$beta[ , which( mod$lambda == se.lam ) ]

# Full mod
full.mod <- cv.glmnet(x,
                      y,
                      alpha = 1,
                      family = 'binomial',
                      type.measure = cvm,
                      nfolds = nf,
                      grouped = FALSE
                      )

full.best.lam <- mod$lambda.min
full.best.coefs <- mod$glmnet.fit$beta[ , which( mod$lambda == full.best.lam ) ]
full.best.cvm <- mod$cvm[ which( mod$lambda == best.lam ) ]

plot.coefs <- function(coefs){
    pcs <- coefs
    plot(pcs, xlab='',ylab='coef value',main='LASSO Coefs')
    text(1:length(pcs),
         pcs-.03,
         labels=names(pcs)
         )
}

plot.roc <- function(mod, lam, xt, yt){ 
    preds <- predict(mod$glmnet.fit, newx = xt, type = 'response')[ , which( mod$lambda == lam ) ]
    perf <- performance(prediction(preds, yt), 'tpr', 'fpr')
    plot( perf, main='Cross Val ROC')

    auc <- performance( prediction(preds, yt), measure='auc' )@y.values
    acc <- max(performance( prediction(preds, y.test), measure='acc' )@y.values[[1]])
    
    return(list(preds=preds, auc=auc, acc=acc))
}

plot.coefs(full.best.coefs)
dev.new()
out <- plot.roc( mod, best.lam, x.test, y.test )
dev.new()
plot( y.test, out[[1]], xlab='target',ylab='score',main='Cross Val Predictions' )

print( paste( 'Acc:', out$acc ) )
print( paste( 'AUC:', out$auc ) )
print( paste( 'CVM:', full.best.cvm ) )
print( full.best.coefs )

# looks like a handful of coefficient values jump up and down a lot over repeated runs; my guess is that the likelihood surface is fairly flat, ie. not strongly identifiable.  Could do a full posterior analysis in Stan to confirm?  That's a lot of work though...
