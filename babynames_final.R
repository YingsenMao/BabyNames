library(xtable)
library(plotly)
Sys.setenv("plotly_username"="yingsenmao")
Sys.setenv("plotly_api_key"="w96zpo76k0")
library(babynames)
library(ggplot2)
library(dplyr)

print(xtable(head(babynames)), type="html")
print(xtable(tail(babynames)), type="html")

get_len <- function(x){
  return (nchar(x))
}

babynames$len <- sapply(babynames$name, get_len)

head(babynames)

############## the diversity of name for sex ##############
res <- babynames %>%
  group_by(year, sex) %>%
  summarise(total = length(name))

head(res)
tail(res)
t_res <- filter(res, year == 2013)
qplot(year, total, data = res, geom = "line", color = sex)

ggplot(data = res, aes(x = year, y = total, color = sex)) +
  geom_line() + labs(title = 'Name Trend', x = 'Year', y = 'Number of Names') +
  theme(plot.title = element_text(face="bold", vjust=1.5, size = 20))

ggsave(file="nameTrend.png")
getwd()
setwd('C:/Users/Yingsen/Documents/data visualization/post1')


g1 <- ggplot(data = res, aes(x = year, y = total, color = sex)) +
  geom_line() + labs(title = 'Names Count from 1880 to 2013', x = 'Year', y = 'Number of Names') +
  theme(plot.title = element_text(face="bold", vjust=1.5, size = 20))
(gg <- ggplotly(g1))
#########################################################

############## the number of birth for sex ##############
res_2 <- babynames %>%
  group_by(year, sex) %>%
  summarise(total_birth = sum(n))
head(res_2)
f_res2 <- filter(res_2, sex == 'F' & year >= 1980)
m_res2 <- filter(res_2, sex == 'M' & year >= 1980)
mean(m_res2$total_birth - f_res2$total_birth)
t_res2 <- filter(res_2, year == 2013)
qplot(year, total_birth, data = res_2, geom = "line", color = sex) 
g2 <- ggplot(data = res_2, aes(x = year, y = total_birth, color = sex)) +
  geom_line() + 
  labs(title = 'Number of Births from 1880 to 2013', x = 'Year', y = 'Number of Births') +
  theme(plot.title = element_text(face = 'bold', size = 20))
ggplotly(g2)
###################################################################

######### the proportion of top 100 names over all names ##############
head(babynames)
babynames <- babynames %>%
  group_by(year, sex) %>%
  mutate(rank = rank(-prop, ties.method = "first"))

res_3 <- babynames %>%
  filter(rank <= 100) %>%
  group_by(year, sex) %>%
  summarise(total_100 = sum(n))

res_4 <- cbind(res_2, res_3)
res_4$proportion <- res_4$total_100 / res_4$total_birth
head(res_4)
qplot(year, proportion, data = res_4, geom = "line", color = sex) +
  ylim(c(0, 1))

g3 <- ggplot(data = res_4, aes(x = year, y = proportion, color = sex)) +
  geom_line() + scale_y_continuous(limits = c(0,1)) +
  labs(title = 'Proportion of top 100 names', x = 'Year', y = 'Proportion') +
  theme(plot.title = element_text(face = 'bold', size = 20))
ggplotly(g3)
###################################################################

################# top names losing and gaining popularity #################
r_f <- filter(r_2, sex == 'F')
r_m <- filter(r_2, sex == 'M')
r_m_2 <- r_m %>%
  group_by(name) %>%
  summarise(tot = sum(tot)) %>%
  arrange(desc(tot))
head(r_m_2)

m_lt <- r_m_2$name[1:200] 
r_4 <- filter(r_2, name %in% m_lt & sex == 'M')
head(r_4)

m_res_r <- c()
m_res_c <- c()
m_res_p <- c()
for (i in seq_along(m_lt)){
  dum_name <- m_lt[i]
  dum_data <- r_m[r_m$name == dum_name, ]
  l_dum <- lm(tot ~ year, data = dum_data)
  m_res_r <- c(m_res_r, summary(l_dum)$r.squared)
  m_res_c <- c(m_res_c, summary(l_dum)$coefficients[2,1])
  m_res_p <- c(m_res_p, summary(l_dum)$coefficients[2,4])
}

d_res <- data.frame(name = m_lt, R.Square = m_res_r,
                    Coefficient = m_res_c, P.value = m_res_p)
d_res <- d_res %>%
  filter(R.Square >= 0.8) %>%
  arrange(desc(Coefficient)) 
d_res_frst_10 <- d_res[1:10, ]
d_res_last_10 <- tail(d_res, 10)
d_res_last_10 <- arrange(d_res_last_10, Coefficient)
d_res_frst_10$P.value <- as.character(d_res_frst_10$P.value)


sub_str <- function(s){
  return (paste0(substr(s, 1, 4), substr(s, 17, 20)))
}
d_res_frst_10$P.value <- sapply(d_res_frst_10$P.value, sub_str)
d_res_frst_10 <- d_res_frst_10[, c('name', 'Coefficient',
                                   'R.Square', 'P.value')]
print(xtable(d_res_frst_10), type="html")

d_res_last_10$P.value <- sapply(d_res_last_10$P.value, sub_str)
d_res_last_10 <- d_res_last_10[, c('name', 'Coefficient',
                                   'R.Square', 'P.value')]
print(xtable(d_res_last_10), type="html")


r_first <- r_m %>%
  filter(name %in% d_res_last_10$name)
g_t <- ggplot(r_first, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  labs(title = 'Top 10 Male Names Losing Popularity', x = 'Year', y = 'Number of usage') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggplotly(g_t)

r_last <- r_m %>%
  filter(name %in% d_res_frst_10$name)
g_t <- ggplot(r_last, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  labs(title = 'Top 10 Male Names Gaining Popularity', x = 'Year', y = 'Number of usage') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggplotly(g_t)

################# the length of the name #################
head(babynames)
unique(babynames$len)

res_5 <- babynames %>%
  group_by(year, len) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))
head(res_5)
ggplot(data = res_5, aes(x = year, y = total)) + geom_line() +
  facet_wrap(~len, nrow = 5, scales = "free_y")
# change the order of group get the same result 
res_6 <- babynames %>%
  group_by(len, year) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))
head(res_6)

ggplot(data = res_6, aes(x = year, y = total)) + geom_line() +
  facet_wrap(~len, nrow = 5, scales = "free_y")
###

#the length of the name measured by proportion and break by sex
res_dum <- babynames %>%
  group_by(sex, year) %>%
  summarise(tot_sex_year = sum(n))
head(res_dum)

res_9 <- babynames %>%
  group_by(sex, year, len) %>%
  summarise(tot_sex_year_len = sum(n))
head(res_9)

res_10 <- merge(res_dum, res_9, by = c('sex', 'year'), all = TRUE)
head(res_10)
res_10$prop <- res_10$tot_sex_year_len / res_10$tot_sex_year
res_10$len <- as.factor(res_10$len)
levels(res_10$len) <- c("2 Letters Name",  "3 Letters Name",  "4 Letters Name",  "5 Letters Name",  "6 Letters Name",  "7 Letters Name",  
                        "8 Letters Name",  "9 Letters Name",  "10 Letters Name",
                        "11 Letters Name", "12 Letters Name", 
                        "13 Letters Name", "14 Letters Name", "15 Letters Name")
g4 <- ggplot(res_10, aes(x = year, y = prop, color = sex)) + geom_line() +
  facet_wrap(~len, nrow = 5, scales = "free_y") +
  labs(title = 'The Proportion of Name Length from 1880 to 2013', x = 'Year', y = 'Proportion') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggsave(file="nameLength.png", path = 'C:/Users/Yingsen/Documents/data visualization/post1')
ggsave(file="nameLength4.jpg", 
       path = 'C:/Users/Yingsen/Documents/data visualization/post1',
       width=9, height=6)
ggplot(res_10, aes(x = year, y = tot_sex_year_len, color = sex)) + geom_line() +
  facet_wrap(~len, nrow = 5, scales = "free_y") +
  labs(title = 'The Number of Name Length from 1880 to 2013', x = 'Year', y = 'Number') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggsave(file="nameLength5.jpg", 
       path = 'C:/Users/Yingsen/Documents/data visualization/post1',
       width=9, height=6)
#after 1950
res_10_1950 <- filter(res_10, year >= 1950)
ggplot(res_10_1950, aes(x = year, y = prop, color = sex)) + geom_line() +
  facet_wrap(~len, nrow = 5, scales = "free")
filter(res_10_1950, len == '11 Letters Name') 
head(res_10)
head(babynames)
res_10_dum <- filter(babynames, len == 11 & year >= 1950 & sex == 'M')
head(res_10_dum)
res_10_dum3 <- res_10_dum %>%
  group_by(name) %>%
  summarise(tot = sum(n)) %>%
  mutate(rank = rank(-tot, ties.method = "first")) %>%
  arrange(rank)

head(res_10_dum3)
name_lt <- res_10_dum3$name[1:5]

res_10_dum4 <- res_10_dum %>%
  filter(name %in% name_lt) %>%
  group_by(name, year) %>%
  summarise(tot = sum(n)) 
g10 <- ggplot(res_10_dum4, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  labs(title = 'Top 5 Males with 11 Letters Names', x = 'Year', y = 'Number') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggplotly(g10)

########################################################

################# the most frequency name #################
head(babynames)
library(xlsx)
write.xlsx(babynames, "C:/Users/Yingsen/Documents/data visualization/bnames.xlsx")
res_11 <- babynames %>%
  group_by(name) %>%
  summarise(tot = sum(n)) %>%
  arrange(desc(tot))
res_11

res_12 <- babynames %>%
  group_by(sex, name) %>%
  summarise(tot = sum(n)) 

# same result
res_12 <- babynames %>%
  group_by(sex, name) %>%
  summarise(tot = sum(n)) %>%
  arrange(desc(tot)) %>%
  mutate(rank = rank(-tot, ties.method = "first"))
m_12 <- filter(res_12, sex == 'M')
#
res_13 <- res_12 %>%
  group_by(sex) %>%
  arrange(desc(tot)) %>%
  mutate(rank = rank(-tot, ties.method = "first")) %>%
  filter(rank <= 30)
as.data.frame(res_13)
top_30_f <- res_13$name[1:30]
top_30_m <- res_13$name[31:60]
res_13_f <- babynames %>%
  filter((sex == 'F') & (name %in% top_30_f)) %>%
  group_by(name, year) %>%
  summarise(tot = sum(n))

ggplot(res_13_f, aes(x = year, y = tot)) + geom_line() +
  facet_wrap( ~ name, ncol=5, scales = 'free_y')

res_13_f_prop <- babynames %>%
  filter((sex == 'F') & (name %in% top_30_f)) %>%
  group_by(name, year) %>%
  summarise(tot = sum(prop))

ggplot(res_13_f_prop, aes(x = year, y = tot)) + geom_line() +
  facet_wrap( ~ name, ncol=5, scales = 'free_y')

res_13_m <- babynames %>%
  filter((sex == 'M') & (name %in% top_30_m)) %>%
  group_by(name, year) %>%
  summarise(tot = sum(n))

ggplot(res_13_m, aes(x = year, y = tot)) + geom_line() +
  facet_wrap( ~ name, ncol=5, scales = 'free_y')


res_14 <- babynames %>%
  filter(year >= 1975) %>%
  group_by(sex, name) %>%
  summarise(tot = sum(n)) %>%
  arrange(desc(tot)) %>%
  mutate(rank = rank(-tot, , ties.method = "first")) %>%
  filter(rank <= 20)
as.data.frame(res_14)
top_10_1975_f <- res_14$name[1:20]
top_10_1975_m <- res_14$name[21:40]

res_14_f <- babynames %>%
  filter((year >= 1975) & (name %in% top_10_1975_f) & (sex == 'F')) %>%
  group_by(name, year) %>%
  summarise(tot = sum(n))
  
ggplot(res_14_f, aes(x = year, y = tot)) + geom_line() +
  facet_wrap( ~ name, ncol=5, scales = 'free_y')

res_14_m <- babynames %>%
  filter((year >= 1975) & (name %in% top_10_1975_m) & (sex == 'M')) %>%
  group_by(name, year) %>%
  summarise(tot = sum(n))

ggplot(res_14_m, aes(x = year, y = tot)) + geom_line() +
  facet_wrap( ~ name, ncol=5, scales = 'free_y')

head(babynames)
r_1 <- filter(babynames, year >= 1980)
head(r_1)
r_2 <- r_1 %>%
  group_by(sex, name, year) %>%
  summarise(tot = sum(n))
head(r_2)
r_f <- filter(r_2, sex == 'F')
r_m <- filter(r_2, sex == 'M')
r_f_2 <- r_f %>%
  group_by(name) %>%
  summarise(tot = sum(tot)) %>%
  arrange(desc(tot))
head(r_f_2)

f_lt <- r_f_2$name[1:200] 
r_4 <- filter(r_2, name %in% f_lt)
head(r_4)

f_res_r <- c()
f_res_c <- c()
f_res_p <- c()
for (i in seq_along(f_lt)){
  dum_name <- f_lt[i]
  dum_data <- r_f[r_f$name == dum_name, ]
  l_dum <- lm(tot ~ year, data = dum_data)
  f_res_r <- c(f_res_r, summary(l_dum)$r.squared)
  f_res_c <- c(f_res_c, summary(l_dum)$coefficients[2,1])
  f_res_p <- c(f_res_p, summary(l_dum)$coefficients[2,4])
}

d_res <- data.frame(name = f_lt, R.Square = f_res_r,
                    Coefficient = f_res_c, P.value = f_res_p)
d_res <- d_res %>%
  filter(R.Square >= 0.8) %>%
  arrange(desc(Coefficient)) 
d_res_frst_10 <- d_res[1:10, ]
d_res_last_10 <- tail(d_res, 10)
d_res_last_10 <- arrange(d_res_last_10, Coefficient)
d_res_frst_10 <- as.data.frame(d_res_frst_10)
d_res_frst_10$P.value <- as.character(d_res_frst_10$P.value)


sub_str <- function(s){
  return (paste0(substr(s, 1, 4), substr(s, 17, 20)))
}
d_res_frst_10$P.value <- sapply(d_res_frst_10$P.value, sub_str)
d_res_frst_10 <- d_res_frst_10[, c('name', 'Coefficient',
                                   'R.Square', 'P.value')]
print(xtable(d_res_frst_10), type="html")

d_res_last_10$P.value <- sapply(d_res_last_10$P.value, sub_str)
d_res_last_10 <- d_res_last_10[, c('name', 'Coefficient',
                                   'R.Square', 'P.value')]
print(xtable(d_res_last_10), type="html")


r_first <- r_f %>%
  filter(name %in% d_res_last_10$name)
g_t <- ggplot(r_first, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  labs(title = 'Top 10 Female Names Losing Popularity', x = 'Year', y = 'Number of usage') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggplotly(g_t)

r_last <- r_f %>%
  filter(name %in% d_res_frst_10$name)
g_t <- ggplot(r_last, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  labs(title = 'Top 10 Female Names Gaining Popularity', x = 'Year', y = 'Number of usage') +
  theme(plot.title = element_text(face="bold", vjust=1.5))
ggplotly(g_t)

###
d_res_t <- rbind(d_res[1:5, ], tail(d_res, 5))
d_res_t$description <- c(rep('increase', 5), rep('decrease', 5))
d_res_t$description <- as.factor(d_res_t$description)
levels(d_res_t$description) <- c('Fastest Decreasing Female Baby Names',
                                 'Fastest Increasing Female Baby Names')
head(d_res_t)
d_res_t <- d_res_t[, c(1, 5)]
md_res_t_2 <- merge(r_f, d_res_t, by = 'name', all.y = TRUE)
head(md_res_t_2)
ggplot(md_res_t_2, aes(x = year, y = tot, color = name)) + 
  geom_line() +
  facet_wrap( ~ description, ncol=2, scales = 'free_y')
###


d_res[d_res$name == 'Mary', ]
r_f[r_f$name == 'Mary', ]

f_lt[1]
r_t <- r_f[r_f$name == 'Isabella', ]
as.data.frame(r_t)
ggplot(r_t, aes(x = year, y = tot)) + geom_point()
r_l <- lm(tot ~ year, data = r_t)
summary(r_l)$r.squared
names(summary(r_l))
summary(r_l)$coefficients[2,1]
summary(r_l)$coefficients[2,4]

###
head(babynames)
r_1_ <- filter(babynames, year >= 1980)
head(r_1_)
r_2_ <- r_1_ %>%
  group_by(sex, name, year) %>%
  summarise(tot = sum(prop))
head(r_2_)
r_f_ <- filter(r_2_, sex == 'F')
r_m_ <- filter(r_2_, sex == 'M')
r_f_2_ <- r_f_ %>%
  group_by(name) %>%
  summarise(tot = sum(tot)) %>%
  arrange(desc(tot))
head(r_f_2_)

f_lt_ <- r_f_2_$name[1:100] 

r_4_ <- filter(r_2_, name %in% f_lt_)
head(r_4_)

f_res_r_ <- c()
f_res_c_ <- c()
f_res_p_ <- c()
for (i in seq_along(f_lt_)){
  dum_name_ <- f_lt_[i]
  dum_data_ <- r_f_[r_f_$name == dum_name_, ]
  l_dum_ <- lm(tot ~ year, data = dum_data_)
  f_res_r_ <- c(f_res_r_, summary(l_dum_)$r.squared)
  f_res_c_ <- c(f_res_c_, summary(l_dum_)$coefficients[2,1])
  f_res_p_ <- c(f_res_p_, summary(l_dum_)$coefficients[2,4])
}

d_res_ <- data.frame(name = f_lt_, r_2 = f_res_r_,
                    coef = f_res_c_, p_v = f_res_p_)
d_res_ <- arrange(d_res_, desc(coef))
d_res[d_res$name == 'Mary', ]
r_f[r_f$name == 'Mary', ]

f_lt[1]
r_t <- r_f[r_f$name == 'Isabella', ]
as.data.frame(r_t)
ggplot(r_t, aes(x = year, y = tot)) + geom_point()
r_l <- lm(tot ~ year, data = r_t)
summary(r_l)$r.squared
names(summary(r_l))
summary(r_l)$coefficients[2,1]
summary(r_l)$coefficients[2,4]



############## last letter ###################
get_letter <- function(x, n = 1){
  if (n < 0){
    len <- nchar(x) + n + 1
  } else{
    len <- n
  }
  return (substring(x, len, len))
}
babynames$f_char <- get_letter(babynames$name, n = 1)
babynames$l_char <- get_letter(babynames$name, n = -1)

res_15 <- babynames %>%
  group_by(f_char, year) %>%
  summarise(tot = sum(prop))
head(res_15)

ggplot(res_15, aes(x = year, y = tot)) + 
  geom_line() +
  facet_wrap(~f_char, nrow = 5)

res_16 <- babynames %>%
  group_by(l_char, year) %>%
  summarise(tot = sum(prop))
head(res_16)

g_t <- ggplot(res_16, aes(x = year, y = tot)) + 
  geom_line() +
  facet_wrap(~l_char, nrow = 5)
ggplotly(g_t)

res_17 <- babynames %>%
  group_by(sex, l_char, year) %>%
  summarise(tot = sum(prop)) %>%
  filter(year == 1910 | year == 1960 | year == 2010)
head(res_17)

str(res_17)
res_17$year <- as.factor(res_17$year)
ggplot(res_17, aes(x = l_char, y = tot, fill = year)) +
  geom_bar(position = 'dodge', stat = "identity") + 
  facet_wrap(~sex, nrow = 2, , scales = 'free_x')

