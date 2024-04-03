# making the owls

# initialize ####

rm(list = ls())
library(dplyr)
library(stringr)
library(ggplot2)


# make functions

simp <- function(n, perb){
  sample(1:perb, n, replace = T)
}

# testing probabilities

# testing different values: rolls1 is deaths, rolls2 is births

rolls1 <- 4
rolls2 <- 2

nems6 <- paste0(rep(c("j", "sa"), each = rolls1), 1:rolls1)
nems12 <- paste0(rep("a", rolls1), 1:rolls1)
nemsb <- paste0(rep("b", rolls2), 1:rolls2)

# a function for testing how correlated the owl cards are (this was unecessary but fun)

info <- function(CLASS.FREQ){
  freq.class <- CLASS.FREQ
  info <- 0
  for(i in 1:length(freq.class)){
    if(freq.class[[i]] != 0){ # zero check in class
      entropy <- -sum(freq.class[[i]] * log2(freq.class[[i]]))  #I calculate the entropy for each class i here
    }else{ 
      entropy <- 0
    } 
    info <- info + entropy # sum up entropy from all classes
  }
  return(info)
}

# make names

names <- read.csv("Popular_Baby_Names.csv")

nammy <- names %>%
  select(sex = Gender, nem = Child.s.First.Name) %>%
  mutate(sex = tolower(sex), nem = tolower(nem)) %>%
  filter(sex == "female") %>%
  distinct() %>%
  select(nem)

owlNames <- str_replace(nammy$nem, "^.", toupper(str_extract(nammy$nem, "^.")))

indv <- 600
corp <- NULL
ent <- NULL
ow <- list()

# generate owls
set.seed(42)

# for a community of (indv) owls, generate 500 different versions and see how correlated they are

for(r in 1:500){
  
  owls <- cbind.data.frame( 
    
    id = sample(owlNames, indv),
    
    sapply(nems6, function(ii){
      assign(ii, simp(indv, 6))
    }),
    
    sapply(nems12, function(ii){
      assign(ii, simp(indv, 12))
    }),
    
    sapply(nemsb, function(ii){
      assign(ii, simp(indv, 6))
    })
  )
  
  oNums <- owls[,-1]
  
  it <- 0
  kor <- NULL
  for(i in 1: ncol(oNums)){
    for(j in 1:ncol(oNums)){
      if(i != j){
        it <- it + 1
        kk <- cor.test(oNums[,i], oNums[,j])
        kor[it] <- kk$estimate
      }
    }
  }
  
  ent[r] <- sum(apply(oNums, 2, function(o){
    info(table(o)/indv)
  }))
  corp[r] <- mean(abs(kor))
  ow[[r]] <- owls
}

dat <- data.frame(run = 1:500, corry = corp, ent = ent)

ggplot(dat, aes(x = corp, y = ent)) +
  geom_point() +
  geom_label(aes(label = run))

# 167 wins!
# (iteration 167 had the least correlations among cards)

write.csv(ow[[167]], "owls.csv" ,row.names = F)





