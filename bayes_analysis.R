library(rstan)
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

drop.cols <- c( 'date', 'team1', 'team2', cols.w.nas, cols.w.factors, 'team1.red', 'team2.red', 'team1.yellow...red', 'team2.yellow...red')

x <- better.dat[, !names(better.dat) %in% drop.cols ]

# we should normalize these covariates, and replace indiv stats for team1 and team2 with the difference between the teams' stats

team1.cols <- seq(4,ncol(x),2)
x <- x[,team1.cols] - x[,team1.cols+1]
x$team1.goals.tot <- x$team1.goal.scored + x$team1.goals.in.pso

# fix up the names
names(x) <- sapply( names(x), function(s) { strsplit(s,'1.')[[1]][2] } )

# drop the goal.scored and goals.in.pso cols since they're colinear with goals.tot
x <- subset(x, select = -c(goal.scored,goals.in.pso) )

x <- as.data.frame( apply(x,2,function(c){ (c-mean(c))/sd(c) }) )


x <- as.matrix( x )

# split data for ROC curve
train.size <- nrow(x) * .3
data.split <- sort(sample(1:nrow(x),train.size))

x.train <- x[-data.split,]
x.test <- x[data.split,]

y <- as.matrix( better.dat$target )
y.train <- y[-data.split]
y.test <- y[data.split]

# Stan preliminaries
mod.file <- './stan_scripts/logistic_lasso.stan'
iter  <- 100

N_full <- dim(x)[1]
N_train <- dim(x.train)[1]
N_test <- dim(x.test)[1]
M <- dim(x)[2]
stan.dat <- list(N_full=N_full,
                 N_train=N_train,
                 N_test=N_test,
                 M=M,
                 x_full=x,
                 x_train=x.train,
                 x_test=x.test,
                 y_full=as.numeric(y),
                 y_train=as.numeric(y.train),
                 y_test=as.numeric(y.test)
                 )

#Initialize and fit hierarchical model
print('initialize Stan model and fit...')
stan.mod <- stan(mod.file, data=stan.dat, chains=4, control=list(adapt_delta=0.85))
stan.fit <- stan(fit=stan.mod, data=stan.dat, iter=iter)
print('done')

samps <- extract(stan.fit)
preds <- apply(samps$p_test,2,mean)
roc <- performance(prediction(preds, y.test), 'tpr', 'fpr')
auc <- performance(prediction(preds, y.test), 'auc')
acc <- performance(prediction(preds, y.test), 'acc')

plot(stan.fit,pars=c('beta_full'))
dev.new()
plot(roc)
print( max(auc@y.values[[1]]) )
print( max(acc@y.values[[1]]) )

