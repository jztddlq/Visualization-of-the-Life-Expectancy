library(shinythemes)
library(shiny)
library(corrplot)
library (dtwclust)
library(ggplot2)
library(plyr)
library(maptools)
library(rgdal)
library(circlize)
library(dplyr)
library(zoo)


#全局变量
Whodata=read.csv("data/16-1.csv",head=TRUE,sep=",")
Whodata$Statu_num=0
x<-colnames(Whodata)
x[1]<-"Country"
colnames(Whodata)<-x

for(i in 1:nrow(Whodata)){
  if (Whodata[i,'Status']== "Developed"){
    Whodata[i,'Statu_num']=1
  }}
#在原始数据中删除country-年份为1
country=c(unique(as.character(Whodata$Country)))
delete_countryname=c()
for( i in 1:length(country)){
  S1=Whodata[Whodata$Country==country[i],4:10]
  if(nrow(S1)!=16){
    delete_countryname=append(delete_countryname,country[i])
  }
}

Whodata<-Whodata[!(Whodata$Country %in% delete_countryname),]


# 计算相关系数


unnall_Whodata_cor=Whodata[rowSums(is.na(Whodata[,4:26]))==0,]
unnall_Whodata_cor=unnall_Whodata_cor[,4:26]
mycor <- cor(unnall_Whodata_cor)

select_x=function(c,m){
  # x:相关系数矩阵, m:相关系数
  #将相关性高于m的系数取1，即目标变量
  c<-ifelse(abs(c)>m,1,0)
  c<-as.data.frame(c)
  n<-nrow(c)
  #避免重复选取组合，将上三角矩阵全部设置成0(非目标变量)
  for(i in 1:n){
    for(j in 1:n){
      c[i,j]<-ifelse(i>j,c[i,j],0)
    }
  }
  #求出符合条件的索引
  index<-which(c==1)#[!(which(c==1) %in% c((1:n)^2))]
  #列号
  index_c<-ifelse(index %% n==0,n,index %% n)
  #行号
  index_r<-(index-index_c) %/% n+1
  #名称
  c$name<-row.names(c)
  #第二个变量名
  name_index_c<-c$name[index_c]
  #第一个变量名
  name_index_r<-c$name[index_r]
  #输出结果
  #cat("\n自变量中相关系数大于",m,"的所有组合如下所示：\n")
  #for(i in 1:length(name_index_r)){
  #cat("     ",i,":",name_index_r[i],"---",name_index_c[i],"\n")
  #}
  #返回需要删除的变量名
  return(unique(name_index_c))
}



# Define server logic required for the correlation application
shinyServer(function(input, output,session) {

  # delete_varname stores the variable name that should be filtered due to strong relation
  delete_varname<-reactive({
    select_x(mycor,input$threshhold)
  })
  
  Whodata1<-reactive({ 
    Whodata[,!(colnames(Whodata) %in% delete_varname())]
    })
  
   df<-reactive({
     colnames(Whodata1())[4:ncol(Whodata1())]
   })
   
   #update the remaining variable name to the selectinput box
   observe({
     updateSelectInput(session, "variable",label="Variable for histogram:",
                       choices = df()
     )})
   
   observe({
     updateSelectInput(session, "vvv",label="Variable for clustering:",
                       choices = df(),selected = df()[1]
     )})
   
   cnum<-reactive({
     length(input$vvv)
   })
   
   mapname<-reactive({
     if(cnum()==1){
       mapname<-c("World map","Circle plot")
     }else
     {
       mapname<-c("World map")
     }
     return(mapname)
   })
   
   observe({
     updateSelectInput(session, "map",label="map",
                       choices = mapname()
     )})
   
   newwhodata<-reactive({
     Whodata<-Whodata[,colnames(Whodata) %in% c("Country","Year",input$vvv)]
     unnall_Whodata_clu=Whodata[rowSums(is.na(Whodata))==0,]
     return(unnall_Whodata_clu)
   })
   
   series_list<-reactive({
     n = ncol(newwhodata())
     #建立数列
     country=c(unique(as.character(newwhodata()$Country)))
     for( i in 1:length(country)){
       S1=newwhodata()[newwhodata()$Country==country[i],3:n]
       #S1=t(S1)
       S1=as.matrix(S1)
       if(i==1){
         series_list= list(S1)
       }
       else{
         series_list[[i]]=S1
       }
     }
     return(series_list)
   })
   
   
   k_list<-reactive({
     for(i in 2:20){
       pc <- tsclust(series_list(), type = "partitional", k = i, 
                     distance = "dtw_basic", centroid = "pam", 
                     seed = 3247L, trace = TRUE,
                     args = tsclust_args(dist = list(window.size = i)))
       S2=pc@cldist
       S2=sum(S2)
       if(i==2){
         K_list= c(S2)
       }
       else{
         K_list=append(K_list,S2)
       }
     }
     return(K_list)
   })
   
   kdata<-reactive({
     data.frame(K = c(2:20),K_dist = k_list())
   })
   
   pc <- reactive({
     tsclust(series_list(), type = "partitional", k = input$k, 
             distance = "dtw_basic", centroid = "pam", 
             seed = 3247L, trace = TRUE,
             args = tsclust_args(dist = list(window.size = 7)))
   })
   
   country1<-reactive({
     c(unique(as.character(newwhodata()$Country)))
   })
   
   #map
   
   world_map <-readOGR('data/World_region.shp',layer ="World_region")
   
   x <- world_map@data    #读取行政信息
   
   xs <- data.frame(x,id=seq(0:250)-1)  #含岛屿共251个形状
   
   
   
   world_map1 <- fortify(world_map)       #转化为数据框
   
   world_map_data <- join(world_map1, xs, type = "full") #合并两个数据框
   
   
   
   
  # generate the Correlation Matrix Map
  output$corplot <- renderPlot({
    corrplot(mycor, method="circle",type="upper",order="hclust"
             # ,tl.cex = 1,tl.col = 'black'
             )
  },
  width=600,height = 400
  )
  
  #generate the Histogram of the selected variable
  output$hplot <- renderPlot({
    ggplot(data=Whodata,aes_string(x= input$variable))+
      geom_histogram(bins=20, color="black", fill="light blue") 
  }
  ,width=600,height = 400
  )
  
  # generate the Correlation Matrix Map
  output$kplot <- renderPlot({
    ggplot(data=kdata(),aes(x=K,y=K_dist))+
      geom_point(size=4)+
      geom_line(size=2,colour="light blue")
    
  },
  width=800,height = 400
  )
  
  #generate the Histogram of the selected variable
  output$cplot <- renderPlot({
    plot(pc())
  },
  width=800,height = 700
  )
  
  output$caption <- renderText({
    
   if(input$map=="World map"){
      input$map
    }else if(input$map=="Circle plot"){
      input$map
    }
    
  })
  # Generate the word map of the clustering result
  output$clustervisual<- renderPlot({
    clust_type=pc()@cluster
    clust_type=as.character(clust_type)
    
    mapdata=data.frame(COUNTRY=country1(),clust=clust_type)
    world_data <- join(world_map_data, mapdata, type="full")
    
 
    if(input$map=="World map"){
      ggplot(world_data, aes(x = long, y = lat, group = group,fill = clust)) +
        
        geom_polygon(colour="grey40") +
        
        
        scale_fill_brewer(palette="Spectral")+
        
        theme(               
          
          panel.grid = element_blank(),
          
          panel.background = element_blank(),
          
          axis.text = element_blank(),
          
          axis.ticks = element_blank(),
          
          axis.title = element_blank(),
          
          legend.position = c(0.2,0.3)
          
        )
    }else if(input$map=="Circle plot"){
      #Whodata= na.fill(Whodata,0)
      test<-Whodata[,colnames(Whodata) %in% c(input$vvv)]
      test=test[order(test)]
      medium=test[as.integer(length(test)*0.5)]
      maxnum<-max(test,na.rm=T)
      maxnum= as.integer(maxnum)
      cir_data=data.frame(COUNTRY=country1(),clust=clust_type)
      cir_data=cir_data[order(cir_data$clust),]
      country=c(unique(as.character(cir_data$COUNTRY)))
      clust_type=c(as.character(cir_data$clust))
      for( i in 1:length(country)){
        S1=Whodata[Whodata$Country==country[i],colnames(Whodata) %in% c("Year",input$vvv)]
        #S1=plyr::rename(S1,c("AM"=clust_type[i]))
        if(i==1){
          S2= S1
        }
        else{
          S2<- left_join(S2, S1, by="Year")
        }
      }
      S2=S2[,2:(length(country)+1)]
      S2=as.matrix(S2)
      for(i in 1:ncol(S2)){
        S2[,i]=rev(S2[,i])
      }
      col <- colorRamp2(c( 0,medium, maxnum), c("blue","white", "red"))
      colt <- colorRampPalette(c("blue", "red"))(input$k)
      
      circos.par(cell.padding = c(0,0, 0, 0),gap.degree = 0)
      
      circos.initialize(factors = country , xlim = cbind(c(0, 0), table(country)))
      circos.trackPlotRegion(
        ylim=c(1,16),
        track.height = 0.3,
        panel.fun = function(x,y){
          sector.index = get.cell.meta.data("sector.index")
          j=which(country==sector.index)
          t=which(clust_type[j]==c(unique(as.character(clust_type))))
          circos.text(CELL_META$xcenter, CELL_META$cell.ylim[1] + 10*uy(2, "mm"),
                      CELL_META$sector.index, facing = "reverse.clockwise", niceFacing = TRUE,
                      adj = c(1, 0.5), cex = 0.6,col=colt[t])
          col_data = col(S2)
          
          nr = nrow(S2)
          
          nc = ncol(S2)
          
          for (i in 1:nr) {
            
            circos.rect(nc , nr - i, nc+1, nr - i + 1,
                        
                        border = "black", col = col_data[i,j]) 
          }
        }
      )
      circos.clear()
    }
    
    
  })
})
