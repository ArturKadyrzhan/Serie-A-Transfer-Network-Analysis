library(dplyr)
library(network)
library(sna)
library(ggplot2)
library(tidyr)
library(readr)
library(igraph)
library(intergraph)

transfers <- read.csv('/Users/artur/Downloads/Projects/Project Data U2/transfer_data.csv')
#head(transfers)
#View(transfers)

serie_a <- transfers %>%
  filter(LEAGUE == 'Serie A' & SEASON == '16/17' & WINDOW == 'Pre-Season' & !grepl("loan", DESCRIPTION, ignore.case = TRUE))

# note: making transfers only within seria a
serie_a_transfers_within <- serie_a %>% filter(FROM %in% serie_a$TO & TO %in% serie_a$TO)
#View(serie_a_transfers_within)

# note: fixing Sampdoria dublicate
serie_a <- serie_a %>%
  mutate(FROM = ifelse(FROM == 'Samdoria', 'Sampdoria', FROM),
         TO = ifelse(TO == 'Samdoria', 'Sampdoria', TO))


#note: now we're making count  transfers of between teams of seria a
serie_a_transfers_within_count <- serie_a_transfers_within %>%
  group_by(FROM, TO) %>%
  summarise(count = n())
View(serie_a_transfers_within)



serie_a_transfers_within <- serie_a %>%
  filter(FROM %in% serie_a$TO & TO %in% serie_a$TO)


#_____________Adjacency matrix__________________________________________________
serie_a_transfers_within_count <- serie_a_transfers_within %>%
  group_by(FROM, TO) %>%
  summarise(count = n()) %>%
  ungroup()

#note: not forget first to create list of clubs
clubs <- unique(c(serie_a_transfers_within_count$FROM, serie_a_transfers_within_count$TO))
adj_matrix <- matrix(0, nrow = length(clubs), ncol = length(clubs), dimnames = list(clubs, clubs))

for (i in 1:nrow(serie_a_transfers_within_count)) {
  from <- serie_a_transfers_within_count$FROM[i]
  to <- serie_a_transfers_within_count$TO[i]
  count <- serie_a_transfers_within_count$count[i]
  
#note:we have matrix with counts 2, 1, or 0 
  if (count >= 2) {
    adj_matrix[from, to] <- 2
  } else if (count == 1) {
    adj_matrix[from, to] <- 1
  } else {
    adj_matrix[from, to] <- 0
  }
}

# Converting matrix to data frame for ggplot2
adj_matrix_df <- as.data.frame(as.table(adj_matrix))

# note: finally Visualizing adjacency matrix with ggplot2
library(ggplot2)
ggplot(adj_matrix_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", breaks = c(0, 1, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Adjacency Matrix for Serie A Transfers (16/17 Pre-Season)",
       x = "From Club", y = "To Club", fill = "Number of Transfers")
#______________________________________________________________________________

#note: Finally start creating network first : Players movement to(within SerieA)
"Notice that some nodes can be large, small or even missing,
it means that some clubs were more active in this transfer period than others.
It shows being realistic and reflective of our data."

net_igraph <- graph_from_data_frame(d = serie_a_transfers_within_count, directed = TRUE)
E(net_igraph)$color <- ifelse(E(net_igraph)$count == 5, "red", 
                              ifelse(E(net_igraph)$count == 2, "green", "black"))
V(net_igraph)$centrality <- degree(net_igraph, mode = "in") / 2
V(net_igraph)$size <- V(net_igraph)$centrality * 5 # Scale for better visualization
plot(net_igraph, vertex.label = V(net_igraph)$name,
     vertex.label.cex = 0.7, vertex.label.color = "black",
     edge.width = E(net_igraph)$count, main = 'Players Movement to (Serie A 16/17, Summer)',
     layout = layout_in_circle(net_igraph))



#_______________________________________________________________________________
#Question: what is main politics of clubs during transfer window is open
#1 conclusion: Serie A clubs mostly prefer players from other league, but was
#it usual thing(add also same thing but from previous things)

serie_a <- serie_a %>%
  mutate(Origin = ifelse(FROM %in% serie_a$TO, 'Within Serie A', 'From Other Leagues'))

# Summarize the number of transfers from each category
transfers_summary <- serie_a %>%
  group_by(Origin) %>%
  summarise(Total_Transfers = n())

# Plot the histogram
ggplot(transfers_summary, aes(x = Origin, y = Total_Transfers, fill = Origin)) +
  geom_bar(stat = "identity", color = "black") +
  theme_light() +
  ylab('Number of Transfers') +
  xlab('Origin') +
  ggtitle('Distribution of Transfers for Serie A Teams (16/17 Pre-Season)') +
  scale_fill_manual(values = c('forestgreen', 'royalblue')) +
  theme(legend.position = 'none')

#_______________________________________________________________________________
#2 Question:How transfer activities influence to club's appereance ?
#Conclusion: даже если купил много игроков это не гарантия залога успеха
#момент когда edge идут в команде как влияет на их позицию
#как продажа игроков влияют на их outcome. в таблицe,мне нужна трансферная
#таблица до и после

  
serie_a$Origin <- ifelse(serie_a$FROM %in% serie_a$TO, 'Within Serie A', 'From other leagues')

serie_a_summary <- serie_a %>%
  group_by(TO) %>%
  summarise(Total_Bought = n(),
            FromSerieA = sum(ifelse(Origin == 'Within Serie A', 1, 0)))


ggplot(data = serie_a_summary, aes(x = reorder(TO, Total_Bought), y = Total_Bought)) +
  geom_bar(stat = 'identity', aes(fill = 'From other leagues'), color = 'black') +
  geom_bar(aes(x = TO, y = FromSerieA, fill = 'Within Serie A'), stat = 'identity', color = 'black', position = 'stack') +
  theme_light() +
  ylab('Total number of players purchased') +
  xlab('Team') +
  scale_fill_manual('Legend',
                    values = c('From other leagues' = 'royalblue', 'Within Serie A' = 'forestgreen'),
                    labels = c('From other leagues', 'Within Serie A')) +
  guides(fill = guide_legend(title = 'Origin')) +
  coord_flip()

#сравни все три таблицы, Аталанта показала хороший результат.
#before
SerieAclassifica15_16 <- read_csv("SerieAclassifica15-16.csv")
View(SerieAclassifica15_16)
#predicted
SerieA16_17predictedtableBBC <- read_csv("SerieA16-17predictedtableBBC.csv")
View(SerieA16_17predictedtableBBC)
#overall
SerieAclassifica16_17 <- read_csv("SerieAclassifica16-17.csv")
View(SerieAclassifica16_17)

#_______________________________________________________________________________
#_______________________________________________________________________________

#question:
#ok let's agree with 1 conclusion, but if they prefer players from other leagues
#which leagues are they?
#How was the main suppliers of serie A in 16-17 summer?
serie_a$FROM_LEAGUE <- ifelse(serie_a$FROM %in% serie_a$TO, 'Serie A', 'Foreign')
serie_a$TO_LEAGUE <- ifelse(serie_a$TO %in% serie_a$TO, 'Serie A', 'Foreign')

foreign_leagues_to <- serie_a %>%
  filter(FROM_LEAGUE == 'Foreign') %>%
  group_by(FROM) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(foreign_leagues_to, aes(x = reorder(FROM, count), y = count)) +
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') +
  coord_flip() +
  theme_light() +
  labs(title = 'Clubs from Leagues Exporting Players to Serie A (16/17 Pre-Season)',
       x = 'Clubs from Leagues', y = 'Number of Players') +
  theme(axis.text.y = element_text(hjust = 1, size = 10)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(size = 14, face = "bold"))


league_data <- data.frame(
  League = c("Inferior Italian Leagues", "Belgium Leagues", "England Leagues", "Spanish Leagues", 
             "Argentina", "Brazil Leagues", "Turkey", "Croatia", "Germany", 
             "Russian Leagues", "Serbian Leagues", "Czech Leagues", "Portugal Leagues",
             "Romania", "Switzerland", "Hungary", "China", "Slovenia", "Ukraine"),
  Players = c(13, 9, 7, 5, 4, 4, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)
)

# Построение bar plot
ggplot(league_data, aes(x = reorder(League, Players), y = Players)) +
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') +
  coord_flip() +
  theme_light() +
  labs(title = 'Distribution of Players Coming to Serie A (16/17 Pre-Season)',
       x = 'Leagues', y = 'Number of Players') +
  theme(axis.text.y = element_text(hjust = 1, size = 10)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(size = 14, face = "bold"))
#Conclusion:Mostly serie B,belgium,england.
#______________________________________________________________________________

#note:

#number of nodes,links
num_nodes <- vcount(net_igraph)
num_edges <- ecount(net_igraph)

print(paste("Number of nodes:", num_nodes))
print(paste("Number of edges:", num_edges))


#degrees,avg_degree,degree distribution
degrees <- degree(net_igraph, mode = "in")  # Предположим, что интересует входящая степень
avg_degree <- mean(degrees)

print("Degrees of vertices:")
print(degrees)
print(paste("Average degree:", avg_degree))


degree_dist <- degree_distribution(net_igraph, mode = "in")
print("Degree distribution:")
print(degree_dist)


#clustering coefficient, avg.clustering coefficient
clustering_coef <- transitivity(net_igraph, type = "local")
avg_clustering_coef <- transitivity(net_igraph, type = "global")
print(paste("Clustering coefficient:", clustering_coef))
print(paste("Average clustering coefficient:", avg_clustering_coef))
