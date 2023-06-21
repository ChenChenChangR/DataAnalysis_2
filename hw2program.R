library(dplyr)
library(readr)
abroad105 <- read_csv("C:/Users/s1042/OneDrive/Desktop/105abroad.csv")
abroad106 <- read_csv("C:/Users/s1042/OneDrive/Desktop/106abroad.csv")
abroad107 <- read_csv("C:/Users/s1042/OneDrive/Desktop/107abroad.csv")
abroad105<-select(abroad105,國別:境外專班,-洲別)
abroad106<-select(abroad106,國別:境外專班,-洲別)
abroad107<-select(abroad107,國別:境外專班,-洲別)
ab1<-full_join(abroad105,abroad106,by="國別")
abroadAll<-full_join(ab1,abroad107,by="國別")
is.numeric(abroadAll$學位生_正式修讀學位外國生.x)
is.numeric(abroadAll$學位生_正式修讀學位外國生.y)
is.numeric(abroadAll$學位生_正式修讀學位外國生)
abroadAll$正式修讀學位外國生加總<-
  abroadAll$學位生_正式修讀學位外國生.x+
  abroadAll$學位生_正式修讀學位外國生.y+
  abroadAll$學位生_正式修讀學位外國生

TopAbroad<-arrange(abroadAll,desc(正式修讀學位外國生加總))
head(select(TopAbroad,國別,正式修讀學位外國生加總),10)

school105 <- read_csv("C:/Users/s1042/OneDrive/Desktop/105school.csv")
school106 <- read_csv("C:/Users/s1042/OneDrive/Desktop/106school.csv")
school107 <- read_csv("C:/Users/s1042/OneDrive/Desktop/107school.csv")
school105<-select(school105,學校名稱:境外專班,-學校類型|-學校代碼)
school106<-select(school106,學校名稱:境外專班,-學校類型|-學校代碼)
school107<-select(school107,學校名稱:境外專班,-學校類型|-學校代碼)
sch1<-full_join(school105,school106,by="學校名稱")
schAll<-full_join(sch1,school107,by="學校名稱")
is.numeric(schAll$學位生_正式修讀學位外國生.x)
is.numeric(schAll$學位生_正式修讀學位外國生.y)
is.numeric(schAll$學位生_正式修讀學位外國生)
schAll$正式修讀學位外國生加總<-
  schAll$學位生_正式修讀學位外國生.x+
  schAll$學位生_正式修讀學位外國生.y+
  schAll$學位生_正式修讀學位外國生

TopSchool<-arrange(schAll,desc(正式修讀學位外國生加總))
head(select(TopSchool,學校名稱,正式修讀學位外國生加總),10)

library(ggplot2)
ggplot()+geom_bar(data=abroadAll,
                  aes(x=國別,y=正式修讀學位外國生加總),
                  stat="identity") +coord_flip()
#install.packages("choroplethr")
##第三題


#第四題
TWstu <- read_csv("C:/Users/s1042/OneDrive/Desktop/TWstu.csv")
group_by(TWstu,`進修交流國家(地區)別`)%>%
  summarise(台灣學生交換之總數=n()) %>%
  arrange(desc(台灣學生交換之總數))

group_by(TWstu,`進修交流國家(地區)別`)%>%
  summarise(台灣學生交換之總數=n()) %>%
  arrange(desc(台灣學生交換之總數))
is.numeric(TWstu$`本國學生出國進修交流至少1學期(修讀學分)以上人數小計`)
is.numeric(TWstu$`本國學生出國進修交流未滿1學期(修讀學分)人數小計`)
TWstu$學生交換總人數<-
  TWstu$`本國學生出國進修交流至少1學期(修讀學分)以上人數小計`+
  TWstu$`本國學生出國進修交流未滿1學期(修讀學分)人數小計`
group_by(TWstu,學校名稱)%>%
  summarise(台灣學生交換之總數=n()) %>%
  arrange(desc(台灣學生交換之總數))
ggplot()+geom_bar(data=TWstu,
                  aes(x=學校名稱,y=學生交換總人數),
                  stat="identity") +coord_flip()

worldTw <- read_csv("C:/Users/s1042/OneDrive/Desktop/worldTw.csv")
is.numeric(worldTw$總人數)
group_by(worldTw,國別)%>%
  summarise(總人數=sum(總人數))%>%
  arrange(desc(總人數))


group_by(worldTw,國別)%>%
  summarise(總人數=sum(總人數))%>%
  arrange(desc(總人數))
#q1面量
library(maps)
library(mapdata)
library(readxl)
library(purrr)
library(plotly)
world_map = map_data("world")
countryName = read_excel("C:\\Users\\s1042\\OneDrive\\Desktop\\regionchange.xls")
countryName = countryName[c(-2, -4)]
names(countryName)[2]  = "國別"
countryAll = left_join(TopAbroad, countryName, by = "國別")
countryAll$英文國名簡稱[countryAll$國別 == '大陸地區'] = 'China'
countryAll$英文國名簡稱[countryAll$國別 == '美國'] = 'USA'
countryAll$英文國名簡稱[countryAll$國別 == '日本'] = 'Japan'
countryAll$英文國名簡稱[countryAll$國別 == '香港'] = 'Hong Kong'
countryAll$英文國名簡稱[countryAll$國別 == '南韓'] = 'South Korea'
countryAll$英文國名簡稱[countryAll$國別 == '澳大利亞']= 'Australia'
countryAll$英文國名簡稱[countryAll$國別 =='聖文森']= 'Saint Vincent'
countryAll$英文國名簡稱[countryAll$國別 =='索羅門群島']= 'Solomon Islands'
countryAll$英文國名簡稱[countryAll$國別 =='馬紹爾群島共和國'] = 'Republic of the Marshall Islands'
countryAll$英文國名簡稱[countryAll$國別 =='巴布亞紐幾內亞'] = 'Papua New Guinea'
countryAll$英文國名簡稱[countryAll$國別 =='聖多美普林西比'] = 'Sao Tome and Principe'
countryAll$英文國名簡稱[countryAll$國別 =='巴勒斯坦'] = 'Palestine'
countryAll$英文國名簡稱[countryAll$國別 =='塞爾維亞共和國'] = 'Republic of Serbia'
countryAll$英文國名簡稱[countryAll$國別 =='沙烏地阿拉伯'] = 'Saudi Arabia'
countryAll$英文國名簡稱[countryAll$國別 =='剛果民主共和國'] = 'Democratic Republic of the Congo'
countryAll$英文國名簡稱[countryAll$國別 =='科索沃共和國'] = 'Kosovo Republic'
countryAll$英文國名簡稱[countryAll$國別 =='索馬利蘭共和國'] = 'Somaliland'
countryAll$英文國名簡稱[countryAll$國別 =='阿拉伯聯合大公國'] = 'United Arab Emirates'
countryAll$英文國名簡稱[countryAll$國別 =='波士尼亞與赫塞哥維納'] = 'Bosnia and Herzegovina'
countryAll$英文國名簡稱[countryAll$國別 =='獅子山共和國'] = 'Lion Rock Republic'

world_DT = full_join(countryAll, world_map, by = c("英文國名簡稱" = "region") )

world_map_goinside <- ggplot(world_DT, aes(x = long, y = lat, group = group, fill = log(正式修讀學位外國生加總))) +
  geom_polygon(colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray") +
  labs(title = "各國來台留學示意圖", x = "", y = "") +
  theme(text=element_text(family = "Kaiti TC Regular"))

world_map_goinside

View(TWstu)
#q2面量
total = data.frame(lapply(TWstu, function(x) {
  buf1 = gsub("王國", "", x)
  buf2 = gsub("聯邦", "", buf1)
  buf3 = gsub("社會主義", "", buf2)
  buf4 = gsub("和平之國", "", buf3)
  buf5 = gsub("侯國", "", buf4)
  buf6 = gsub("大公國", "", buf5)
  buf7 = gsub("人民", "", buf6)
  gsub("共和國", "", buf7)
}),stringsAsFactors = F)

countryAll = left_join(total, countryName, by = c("進修交流國家.地區.別" = "國別"))
countryAll$英文國名簡稱[countryAll$進修交流國家.地區.別 == '大陸地區'] = 'China'
countryAll$英文國名簡稱[countryAll$進修交流國家.地區. == '美國'] = 'USA'
countryAll$英文國名簡稱[countryAll$進修交流國家.地區. == '日本'] = 'Japan'
countryAll$英文國名簡稱[countryAll$進修交流國家.地區. == '香港'] = 'Hong Kong'
countryAll$英文國名簡稱[countryAll$進修交流國家.地區. == '大韓民國(南韓)'] = 'South Korea'
countryAll$英文國名簡稱[countryAll$進修交流國家.地區. == '澳大利亞']= 'Australia'
world_DT = full_join(countryAll, world_map, by = c("英文國名簡稱" = "region") )
world_DT$學位生_正式修讀學位外國生 = as.numeric(world_DT$學位生_正式修讀學位外國生)

world_map_goinside <- ggplot(world_DT, aes(x = long, y = lat, group = group, fill = log(學位生_正式修讀學位外國生))) +
  geom_polygon(colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray") +
  labs(title = "台灣大學生去哪些國家進修面量圖", x = "", y = "") + #stat = "identity" 直接畫數字
  theme(text=element_text(family = "Kaiti TC Regular"))

world_map_goinside


#q3面量
countryAll = left_join(worldTw, countryName, by = c("國別"))

countryAll$英文國名簡稱[countryAll$國別 == '大陸地區'] = 'China'
countryAll$英文國名簡稱[countryAll$國別 == '美國'] = 'USA'
countryAll$英文國名簡稱[countryAll$國別 == '日本'] = 'Japan'
countryAll$英文國名簡稱[countryAll$國別 == '香港'] = 'Hong Kong'
countryAll$英文國名簡稱[countryAll$國別 == '韓國'] = 'South Korea'
countryAll$英文國名簡稱[countryAll$國別 == '澳大利亞']= 'Australia'
countryAll$英文國名簡稱[countryAll$國別 =='阿拉伯聯合大公國'] = 'United Arab Emirates'
countryAll$英文國名簡稱[countryAll$國別 =='阿拉伯聯合大公國(杜拜地區)'] = 'Saudi Arabia'

world_DT = full_join(countryAll, world_map, by = c("英文國名簡稱" = "region") )

world_map_goinside <- ggplot(world_DT, aes(x = long, y = lat, group = group, fill = log(總人數))) +
  geom_polygon(colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray") +
  labs(title = "台灣學生去各國留學面量圖", x = "", y = "") + #stat = "identity" 直接畫數字
  theme(text=element_text(family = "Kaiti TC Regular"))

world_map_goinside




