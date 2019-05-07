gain_from_agreement<-function(agreement.scope, 
                              agreement.members,
                              beneficiaries,
                              barrier.df, 
                              rel.imp.utility, 
                              participation.threshold,
                              total.import.values,
                              area.global.imports,
                              prize.allocation.scope,
                              lib.trade.growth){
  area.codes=agreement.scope
  coalition=agreement.members
  benefactors=beneficiaries
  
  eval(parse(text=paste("barrier.count=", barrier.df, sep="")))
  relative.import.utility=rel.imp.utility
  participation.threshold=participation.threshold
  
  eval(parse(text=paste("total.imports=", total.import.values, sep="")))
  eval(parse(text=paste("area.world.imports=", area.global.imports, sep="")))
  eval(parse(text=paste("prize.allocation.area=", prize.allocation.scope, sep="")))
  
  free.riders=data.frame()
  net.income=data.frame(result=-1)
  
  ## EU/EEU members
  eu.members=country.correspondence$un_code[country.correspondence$name=="EU-28"]
  eeu.members=country.correspondence$un_code[country.correspondence$name=="Eurasian Economic Union"]
  
  ## adding EU/EEU as a coalition member (to be aggregated over later)
  if(sum(as.numeric(eu.members %in% coalition))>0){
    coalition=unique(c(coalition[!coalition %in% eu.members], 10007))
  }
  
  if(sum(as.numeric(eeu.members %in% coalition))>0){
    coalition=unique(c(coalition[!coalition %in% eeu.members], 10008))
  }
  
  ur.coalition=coalition
  
  
  while(length(coalition)>0 & nrow(subset(net.income, result<participation.threshold))>0){
    ## expanding EU/EEU
    
    if(10007 %in% coalition){
      coalition=unique(c(coalition, eu.members))
    }
    
    if(10008 %in% coalition){
      coalition=unique(c(coalition, eeu.members))
    }
    
    ## assuming only GTA-recorded barriers are open for liberalisation
    the.prize=merge(subset(barrier.count, i.un %in% coalition & 
                             affected.product %in% area.codes), 
                    total.imports, 
                    by=c("i.un","affected.product"))
    
    the.prize=subset(the.prize, is.na(total.imports)==F)
    
    ## invoking post-liberalisation growth assumption
    the.prize$total.imports=the.prize$total.imports*growth
    
    
    ## extrapolating 2017 market shares for prize allocation
    p.a=subset(prize.allocation.area, a.un %in% benefactors)
    p.a=merge(p.a, aggregate(market.share ~ i.un + affected.product, p.a, sum), by=c("i.un","affected.product"), all.x=T)
    p.a$market.share=p.a$market.share.x/p.a$market.share.y
    p.a$market.share.x=NULL
    p.a$market.share.y=NULL
    
    prize.distribution=merge(p.a, the.prize, by=c("i.un","affected.product"))
    
    ## are the coalition partners also trading partners?
    if(nrow(prize.distribution)>0){
      prize.distribution$prize.earned=prize.distribution$market.share*prize.distribution$total.imports
      
      ## correcting for EU and EEU
      prize.distribution$a.un[prize.distribution$a.un %in% eu.members]=10007
      prize.distribution$i.un[prize.distribution$i.un %in% eu.members]=10007
      
      prize.distribution$a.un[prize.distribution$a.un %in% eeu.members]=10008
      prize.distribution$i.un[prize.distribution$i.un %in% eeu.members]=10008
      
      
      income.won=aggregate(prize.earned ~ a.un + affected.product, prize.distribution, sum)
      income.lost=aggregate(prize.earned ~ i.un + affected.product, prize.distribution, sum)
      
      setnames(income.won, "a.un","i.un")
      net.income.by.code=merge(income.won, income.lost, by=c("i.un", "affected.product"), all=T)
      net.income.by.code[is.na(net.income.by.code)]=0
      setnames(net.income.by.code, "prize.earned.x","new.exports")
      setnames(net.income.by.code, "prize.earned.y","new.imports")
      
      net.income=merge(aggregate(new.imports ~ i.un, net.income.by.code, sum),
                       aggregate(new.exports ~ i.un, net.income.by.code, sum),
                       by="i.un", all=T)
      net.income$result=net.income$new.exports+relative.import.utility*net.income$new.imports
      
      coalition=intersect(coalition, net.income$i.un[net.income$result>=participation.threshold])
      
      
      free.riders=rbind(free.riders, 
                        subset(net.income,result<participation.threshold)[,c("i.un","result")])
      
    } else {
      coalition=c()
      net.income=data.frame(result=-1)
    }
    
    

  }
  
  by.stander=setdiff(ur.coalition, c(coalition, free.riders$i.un))
  
  ### generating stats
  if(length(coalition)>0){
    m.count=length(coalition)
    f.count=length(unique(free.riders$i.un))
    b.count=length(by.stander)
    lib.count=nrow(subset(net.income, result>=participation.threshold & new.imports!=0))
    
    
    
    c.t.trade=sum(subset(total.imports, i.un %in% coalition & affected.product %in% area.codes)$total.imports)
    c.l.trade=sum(the.prize$total.imports)/growth
    intra.c.l.trade=sum(subset(prize.distribution, a.un %in% coalition)$prize.earned)/growth
    
    imp.share=c.t.trade/area.world.imports
    imp.share.liberalised=c.l.trade/area.world.imports
    
    

    
  } else {
    m.count=0
    lib.count=0
    f.count=0
    b.count=0
    
    c.t.trade=0
    c.l.trade=0
    intra.c.l.trade=0
    imp.share=0
    imp.share.liberalised=0
    
    
  }
  
  
  output.list<- list("coalition"=coalition, "free.riders"=free.riders, 
                     "by.stander"=by.stander,
                     "net.income"=net.income, 
                     "m.count"=m.count,
                     "lib.count"=lib.count,
                     "f.count"=f.count,
                     "b.count"=b.count,
                     "c.t.trade"=c.t.trade,
                     "c.l.trade"=c.l.trade,
                     "intra.c.l.trade"=intra.c.l.trade,
                     "imp.share"=imp.share,
                     "imp.share.liberalised"=imp.share.liberalised)

  return(output.list)
  
   
}






