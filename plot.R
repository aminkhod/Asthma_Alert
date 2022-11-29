glaz=read.csv('glaz_qual.csv')
plot(glaz$c)
plot(glaz$c, glaz$f)
cor.test(glaz$c, glaz$f)
