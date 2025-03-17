library(stm)
library(tm)

dat <- read.csv("./Structural_Topic_Modelling/poliblogs2008.csv")

head(dat)

processed <- textProcessor(dat$documents, metadata = dat)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

poliblogPrevFit <- stm(documents = out$documents, 
                       vocab = out$vocab,
                       K = 20, 
                       prevalence =~ rating + s(day, df = 5),
                       max.em.its = 75, 
                       data = out$meta,
                       init.type = "Spectral")

plot(poliblogPrevFit, type = "summary")
plot.STM(poliblogPrevFit)
findThoughts(poliblogPrevFit, n = 2, topics = 3)
labelTopics(poliblogPrevFit, c(3, 7, 20))

findThoughts(poliblogPrevFit, texts = shortdoc,
             n = 2, topics = 3)$docs[[1]]

prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                       meta = out$meta, uncertainty = "Global")

summary(prep, topics=3)

plot(prep, "day", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                 to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
        labels = monthnames)

