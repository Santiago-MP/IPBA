library(readr)
library(ggplot2)
library(ggpmisc)

DBOsol <- read_delim("DBOsol.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)
calibrado <- read_delim("calibrado.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)
data1 <- DBOsol
data2 <- calibrado
calib.lm <- lm(conc~promedio, data2)


ggplot()+geom_line(data=data1, aes(T,DBOsol),color='#FFE599', size=1.5)+
  labs(title=expression(DBO[sol]~(mg~O[2]/L)), x=expression(t~(días)), y=expression(DBO~(mg~O[2]/L)))+
  theme(plot.title.position = 'panel')+
  theme_bw()

ggplot(data=data2, aes(promedio,conc))+
  geom_abline(color='#F9B641FF',size=1, slope = coef(calib.lm)[["promedio"]],
              intercept = coef(calib.lm)[["(Intercept)"]],show.legend=T)+
  geom_point(data=data2, aes(promedio,conc),color='black', size=3)+
  labs(title='Calibrado DQO colorimétrica', x=expression(A[446]), y=expression(Concentración~(ppm)))+
  stat_poly_eq(use_label("eq"), label.x = 0.9, coef.digits = 5)+
  stat_poly_eq(label.x = 0.84,label.y = 0.89, rr.digits = 4)+
  theme(plot.title.position = 'panel')+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 140, by = 20))+
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.1))

