setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(futile.matrix)
fullcogsci_byAuthor = read_csv("fullcogsci_byAuthor.csv")

coauthor_mats <- list()
years=2000:2019
for(i in 1:length(years)){
  load(paste0("fullcogsci_binary/author_mat_year_fullcogsci_", years[i],".RData"))
  coauthor_mats[[i]] = author_mat_year
}
peek(coauthor_mats[[19]],15)

# temp = matrix(rbinom(100, 1, 0.25), nrow=10, ncol=10)
# temp[temp==0] <- NA
neighbor_mats = list()

start <- Sys.time()
for(a in 1:length(coauthor_mats)){
  print(paste0("year: ",years[a]))
  author_key = fullcogsci_byAuthor %>% filter(year== years[a]) %>% pull(authorAbbr) %>% unique()
  new.matr = coauthor_mats[[a]]
  rownames(new.matr) <- author_key
  colnames(new.matr) <- author_key
  new.matr[new.matr==0] <- NA
  neighbor = apply(new.matr, 1, function(x) which(x==1))
  for(i in 1:length(neighbor)){ #author
    for(j in neighbor[[i]]){ #author's neighbors, dist=1
      for(k in neighbor[[j]]){ #author's neighbors' neighbors, dist=2
        new.matr[i,k] = pmin(new.matr[i,k], 2, na.rm=TRUE)
        for(l in neighbor[[k]]){ #author's neighbors' neighbors' neighbors, dist=3
          new.matr[i,l] = pmin(new.matr[i,l], 3, na.rm=TRUE)
          for(m in neighbor[[l]]){ #author's neighbors' neighbors' neighbors' neighbors, dist=4
            new.matr[i,m] = pmin(new.matr[i,m], 4, na.rm=TRUE)
            for(n in neighbor[[m]]){ #author's neighbors' ... neighbors, dist=5
              new.matr[i,n] = pmin(new.matr[i,n], 5, na.rm=TRUE)
              for(o in neighbor[[n]]){ #author's neighbors' ... neighbors, dist=6
                new.matr[i,o] = pmin(new.matr[i,o], 6, na.rm=TRUE)
                # for(p in neighbor[[o]]){ #author's neighbors' ... neighbors, dist=7
                #   new.matr[i,p] = pmin(new.matr[i,p], 7, na.rm=TRUE)
                #   
                # }
              }
            }
          }
        }
      }
    }
  }
  neighbor_mats[[a]] = new.matr
  print(Sys.time()-start)
}






