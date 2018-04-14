########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Thu Feb 04 13:35:41 2010
########################################################
#Constants consisting of the start of the military operations against
#drug cartels

#Each state has a number assigned to them in the csv file
baja.california <- "^02"
chihuahua <- "^08"
durango <- "^10"
michoacan <- "^16"
sinaloa <- "^25"
sonora <- "^26"

guerrero <- "^12"
oaxaca <- "^20"

tamaulipas <- "^28"
nuevo.leon <- "^19"

veracruz <- "^30"

########################################################
#Start of the joint operations to combat drug cartels
########################################################
#From: Wikipedia
op.mich <- as.Date("12/11/2006", "%m/%d/%Y")

#OPERATIVO CONJUNTO  TIJUANA
op.tij <-  as.Date("01/03/2007", "%m/%d/%Y")
#Operativo Conjunto Guerrero
op.gue <- as.Date("01/15/2007", "%m/%d/%Y")
#Operativo Conjunto Tri�ngulo Dorado(SINALOA, DURANGO Y CHIHUAHUA)
op.tria.dor <- as.Date("01/22/2007", "%m/%d/%Y")
op.tria.dor.II <- as.Date("05/01/2007", "%m/%d/%Y")
op.tria.dor.III <- as.Date("02/01/2008", "%m/%d/%Y")

#Operativo Conjunto Chihuaha
#El secretario de la Defensa, Guillermo Galv�n, informa que para esa fecha ya estaban destacamentados en ciudad Ju�rez 539 efectivos, y anuncia arribo de tres H�rcules.
op.chi <- as.Date("03/27/2008", "%m/%d/%Y")

#Operaci�n Conjunta Culiac�n-Navolato
op.sin <-  as.Date("05/13/2008", "%m/%d/%Y")

#Opertivo Conjunto Tamaulipas - Nuevo Le�n
op.tam.nl <- as.Date("02/19/2007","%m/%d/%Y")

#Operativo Sonora
op.son <- as.Date("03/07/2008", "%m/%d/%Y")

#Operativo Conjunto Veracruz
op.ver <- as.Date("05/14/2007", "%m/%d/%Y")

#Reinfocements for Ciudad Juarez
#2,000 arrived March 1st 2009 and another 3,000 arrived during the
#next 15 days
cdj.rein <- as.Date("03/01/2009", "%m/%d/%Y")

#Alfredo Beltr�n Leyva caputred
bel.ley <- as.Date("01/21/2008", "%m/%d/%Y")

#Eduardo Arellano F�lix "El Doctor" captured
doctor <- as.Date("10/26/2008", "%m/%d/%Y")

#Presidential visit to Ciudad Juarez after 16 students were killed
calderon <- as.Date("02/11/2010", "%m/%d/%Y")

#The killing of thre people linked with the US consulate in Juarez
consulate <- as.Date("03/13/2010", "%m/%d/%Y")

#El Chapo takes control of the drug trade in Ciudad Juarez
#http://www.google.com/hostednews/ap/article/ALeqM5gMi5B2USfJStXxfqgWWr2xjRYpOgD9EVPTA01
fall <- as.Date("04/09/2010", "%m/%d/%Y")

#Police takes over from army
#http://www.google.com/hostednews/ap/article/ALeqM5gMi5B2USfJStXxfqgWWr2xjRYpOgD9EV8GRO0
police <- as.Date("04/08/2010", "%m/%d/%Y")

#Vicente Fox sends troops to Nuevo Laredo
#http://www.univision.com/content/content.jhtml?cid=625397
foxy.troops <- as.Date("06/13/2005", "%m/%d/%Y")



