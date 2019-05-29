gain_from_agreement<-function(agreement.scope, 
                              agreement.members,
                              beneficiaries,
                              prize.df, 
                              rel.imp.utility, 
                              participation.threshold,
                              total.import.values,
                              area.global.imports,
                              prize.dist.df,
                              lib.trade.growth){
  
  
  area.codes=agreement.scope
  coalition=agreement.members
  benefactors=beneficiaries
  
  relative.import.utility=rel.imp.utility
  participation.threshold=participation.threshold
  
  eval(parse(text=paste("total.imports=", total.import.values, sep="")))
  eval(parse(text=paste("area.world.imports=", area.global.imports, sep="")))
  eval(parse(text=paste("the.prize.area=", prize.df, sep="")))
  eval(parse(text=paste("prize.distribution.area=", prize.dist.df, sep="")))
  
  free.riders=data.frame()
  net.income=data.frame(result=-1)
  
  ## EU/EEU members
  eu.members=country.names$un_code[country.names$is.eu==T]
  eeu.members=country.names$un_code[country.names$is.eeu==T]
  
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
    the.prize=subset(the.prize.area, i.un %in% coalition)
    prize.distribution=subset(prize.distribution.area, i.un %in% coalition)
    
    
    
    ## are the coalition partners also trading partners?
    if(nrow(prize.distribution)>0){
      
      ## correcting for EU and EEU
      prize.distribution$a.un[prize.distribution$a.un %in% eu.members]=10007
      prize.distribution$i.un[prize.distribution$i.un %in% eu.members]=10007
      
      prize.distribution$a.un[prize.distribution$a.un %in% eeu.members]=10008
      prize.distribution$i.un[prize.distribution$i.un %in% eeu.members]=10008
      
      income.won=aggregate(prize.earned ~ a.un, prize.distribution, sum)
      income.lost=aggregate(prize.earned ~ i.un, prize.distribution, sum)
      
      setnames(income.won, "a.un","i.un")
      net.income=merge(income.won, income.lost, by=c("i.un"), all=T)
      net.income[is.na(net.income)]=0
      setnames(net.income, "prize.earned.x","new.exports")
      setnames(net.income, "prize.earned.y","new.imports")
      
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



gta_get_combos=function(recombine.me=NULL,
                        tuple.range=c(1,"nr.elements")){
  
  if(is.null(recombine.me)){
    stop("Please add a vector to generate the combinations out of (recombine.me).")
  }
  
  if(length(recombine.me)<2){
    stop("The supplied vector has less than two elements.")
  }
  
  ## setting the minimum number of elements inside a combination.
  combo.tuple.min=tuple.range[1]
  
  if(combo.tuple.min=="nr.elements"){
    combo.tuple.min=length(unique(recombine.me))
  } else {
    combo.tuple.min=as.numeric(as.character(combo.tuple.min))
  }
  
  if(is.na(combo.tuple.min)){
    stop("The minimum value for your range is neither an integer nor 'nr.elements'.")
  }
  
  ## setting the maximum number of elements inside a combination.
  combo.tuple.max=tuple.range[2]
  
  if(combo.tuple.max=="nr.elements"){
    combo.tuple.max=length(unique(recombine.me))
  }else {
    combo.tuple.max=as.numeric(as.character(combo.tuple.max))
  }
  
  if(is.na(combo.tuple.max)){
    stop("The maximum value for your range is neither an integer nor 'nr.elements'.")
  }
  
  
  combo.base=list(unique(recombine.me))
  
  combos=list()
  for(i in combo.tuple.min:combo.tuple.max){
    
    combo.cols=as.data.frame(combn(combo.base[[1]],i))
    
    for(j in 1:ncol(combo.cols)){
      combos=append(combos,
                    list(paste(combo.cols[,j])))
    }
  }
  
  return(combos)
  
  
  
}



### the grand coalition finder

gta_coalition_search <- function(excl.agri=T,
                                 sector.ranks=NULL,
                                 sector.tuple.min=NULL,
                                 sector.tuple.max=NULL,
                                 incl.bb=NULL,
                                 incl.atb=NULL,
                                 incl.subs=NULL,
                                 incl.td=NULL,
                                 instrument.tuple.min=NULL,
                                 instrument.tuple.max=NULL,
                                 excl.ets=NULL,
                                 excl.targeted=NULL,
                                 incl.flows="inward",
                                 growth.rates=NULL,
                                 participation.threshold=NULL,
                                 import.weights=NULL,
                                 trade.data.year=2017,
                                 store.folder=NULL,
                                 create.xlsx=F,
                                 store.folder.xlsx=NULL){
  
  
  ################### RUN WE DO
  ## deploy settings
  ets.interventions=c(63250,63254)
  agri.sectors=gta_cpc_code_expand(c(1,2,3,4,21,22,23))
  agri.products=cpc.to.hs$hs[cpc.to.hs$cpc %in% agri.sectors]
  
  ## Choosing trade data
  gta_trade_value_bilateral(trade.data=trade.data.year,df.name="trade")
  setnames(trade, "hs6","affected.product")
  
  total.imports=aggregate(trade.value ~ i.un + affected.product, trade, sum)
  
  ## EU/EEU members
  eu.members=country.names$un_code[country.names$is.eu==T]
  eeu.members=country.names$un_code[country.names$is.eeu==T]
  
  
  ti.eu=aggregate(trade.value ~ affected.product, subset(trade, i.un %in% eu.members), sum)
  ti.eu$i.un=10007
  ti.eeu=aggregate(trade.value ~ affected.product, subset(trade, i.un %in% eeu.members), sum)
  ti.eeu$i.un=10008
  
  total.imports=rbind(total.imports, ti.eu, ti.eeu)
  setnames(total.imports, "trade.value","total.imports")
  
  
  ## prepare underlying trade values
  total.sec=aggregate(trade.value ~ affected.product, trade, sum)
  setnames(total.sec, "trade.value","total.imports")
  
  hs2cpc=cpc.to.hs
  names(hs2cpc)=c("cpc","affected.product")
  
  total.sec=merge(total.sec, hs2cpc, by="affected.product", all.x=T)
  total.sec$cpc2=as.numeric(substr(sprintf(fmt = "%03i",total.sec$cpc),1,2))
  
  ## removing agriculture
  if(excl.agri){
    total.sec=subset(total.sec, ! cpc %in% agri.sectors)
  }
  
  sectoral.imports=aggregate(total.imports ~ cpc2, total.sec, sum)
  sectoral.imports=sectoral.imports[order(-sectoral.imports$total.imports),]
  
  ## restricting to top sectors
  if(sector.ranks[1]=="max"){
    sector.top=length(sectoral.imports$cpc2)
  } else{
    sector.top=sector.ranks[1]
  }
  
  if(sector.ranks[2]=="max"){
    sector.bottom=length(sectoral.imports$cpc2)
  } else {
    sector.bottom=sector.ranks[2]
  }
  
  top.sectors=sectoral.imports$cpc2[sector.top:sector.bottom]
  
  
  ## initialise records
  cm=data.frame()
  cs=data.frame(coalition.id=numeric(),
                instruments.incl=character(),
                sector.scope=character(),
                sector.level=numeric(),
                sector.name=character(),
                import.utility.weight=numeric(),
                member.size=numeric(),
                members.liberalising=numeric(),
                freerider.count=numeric(),
                bystander.count=numeric(),
                coalition.total.trade=numeric(),
                coalition.liberalised.trade=numeric(),
                intra.coalition.liberalised.trade=numeric(),
                share.world.imports=numeric(),
                share.world.imports.liberalised=numeric())
  # load("0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results - inward subsidies.Rdata")
  
  
  
  restrictions=c()
  if(excl.agri){
    restrictions=paste(restrictions, " - no agriculture" ,sep="")
  }
  if(excl.ets){
    restrictions=paste(restrictions, " - excl ETS" ,sep="")
  } else {
    restrictions=paste(restrictions, " - incl ETS" ,sep="")
  }
  
  ## find targeted interventions
  if(excl.targeted){
    gta.int=read.csv("data/database replica/gta_intervention.csv", stringsAsFactors = F)
    targeted.interventions=unique(subset(gta.int, aj_freeze==1)$id)
  } else {
    targeted.interventions=c()
  }
  
  
  ## at vs behind the border interventions
  atb.type=as.character(int.mast.types$intervention.type)[int.mast.types$is.at.the.border==1]
  
  if(incl.td==F){
    atb.type=setdiff(atb.type, as.character(int.mast.types$intervention.type)[int.mast.types$mast.chapter.id=="D"])
  }
  
  bb.type=as.character(int.mast.types$intervention.type)[int.mast.types$is.behind.border==1]
  sub.type=as.character(int.mast.types$intervention.type)[int.mast.types$mast.chapter.id=="L"]
  
  types.to.check=c()
  if(incl.atb){
    types.to.check=c(types.to.check, atb.type)
    
    if(incl.td){
      int.types="at-the-border interventions"
    } else {
      int.types="at-the-border interventions (excl trade defence)"
    }
  }
  
  if(incl.bb){
    int.types="behind-the-border interventions"
    types.to.check=c(types.to.check, bb.type)
  }
  
  if(incl.subs){
    int.types="inward subsidies only"
    types.to.check=c(types.to.check, sub.type)
  }
  
  if(incl.bb== T & incl.atb==T){
    if(incl.td){
      int.types="at & behind-the border interventions"
    } else {
      int.types="at & behind-the border interventions (excl trade defence)"
    }
  }
  types.to.check=unique(types.to.check)
  
  unwanted.instruments=c("Financial assistance in foreign market","Import incentive","Consumption subsidy")
  
  ## set of instruments to check out
  instrument.base=types.to.check
  instrument.base=setdiff(instrument.base, unwanted.instruments)
  
  if(instrument.tuple.min=="max"){
    instrument.tuple.min=length(instrument.base)
  }
  if(instrument.tuple.max=="max"){
    instrument.tuple.max=length(instrument.base)
  }
  
  instruments=gta_get_combos(instrument.base, c(instrument.tuple.min, instrument.tuple.max))
  
  
  
  min.i=1
  max.i=length(instruments)
  
  
  
  my.path=paste(store.folder, "/Coalition results - ", int.types,restrictions,".Rdata",sep="")
  my.path.xlsx=paste(store.folder.xlsx, "/Coalition results - ", int.types,restrictions,".xlsx",sep="")
  coalition.stats=data.frame()
  coalition.members=data.frame()
  save(coalition.stats, coalition.members, file=my.path)
  
  already.run=c()
  
  step=1
  
  if(nrow(coalition.stats)==0){
    max.cid=1
  } else {
    max.cid=max(coalition.stats$coalition.id)+1
  }
  
  
  for(growth in growth.rates){
    
    for(inst.count in min.i:max.i){
      inst=instruments[[inst.count]]
      print(paste("Starting instrument:",paste(inst, collapse=";")))
      
      ## What tariff lines are part of the prize? i.e currently discriminated against
      if(excl.ets){
        gta_data_slicer(gta.evaluation = c("Red","Amber"),
                        in.force.today = "yes",
                        affected.flow=incl.flows,
                        intervention.types = inst,
                        keep.type = T,
                        intervention.ids = c(targeted.interventions, ets.interventions),
                        keep.interventions = F)
      } else {
        gta_data_slicer(gta.evaluation = c("Red","Amber"),
                        in.force.today = "yes",
                        affected.flow=incl.flows,
                        intervention.types = inst,
                        keep.type = T,
                        intervention.ids = targeted.interventions,
                        keep.interventions = F)
      }
      
      if(nrow(master.sliced)>0){
        
        
        liberalisation.options=unique(subset(master.sliced, is.na(affected.product)==F)[,c("intervention.id","i.un","affected.product")])
        liberalisation.options=cSplit(liberalisation.options, which(names(liberalisation.options)=="affected.product"), sep=",", direction="long")
        
        ## applying intervention restrictions
        if(excl.agri){
          liberalisation.options=subset(liberalisation.options, ! affected.product %in% agri.products)
        }
        
        liberalisation.options=aggregate(intervention.id ~., liberalisation.options, function(x) length(unique(x)))
        
        
        ## setting up barrier count incl trade values
        barrier.count=unique(liberalisation.options[,c("i.un","affected.product","intervention.id")])
        setnames(barrier.count, "intervention.id","nr.of.hits")
        barrier.count=merge(barrier.count, 
                            total.imports, 
                            by=c("i.un","affected.product"))
        barrier.count=subset(barrier.count, is.na(total.imports)==F)
        
        
        liberalisation.options=merge(merge(liberalisation.options, trade, by=c("i.un","affected.product")), total.imports, by=c("i.un","affected.product"))
        liberalisation.options$market.share=liberalisation.options$trade.value/liberalisation.options$total.imports
        
        prize.allocation=unique(liberalisation.options[,c("i.un","affected.product","a.un","market.share")])
        
        
        ## area of cooperation flex
        aoc=unique(as.numeric(substr(sprintf(fmt = "%03i",
                                             cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(cSplit(master.sliced,
                                                                                           which(names(master.sliced)=="affected.product"), direction="long", sep=",")$affected.product)]),
                                     1,2)))
        
        ## restricting to top sectors
        aoc= intersect(aoc, top.sectors)
        
        
        ## creating tuples
        if(sector.tuple.min=="max"){
          sector.tuple.min=length(top.sectors)
        }
        if(sector.tuple.max=="max"){
          sector.tuple.max=length(top.sectors)
        }
        
        areas.of.cooperation=data.frame()
        for(i in sector.tuple.min:sector.tuple.max){
          
          combos=as.data.frame(combn(aoc,i))
          
          for(j in 1:ncol(combos)){
            areas.of.cooperation=rbind(areas.of.cooperation,
                                       data.frame(cpc=paste(combos[,j], collapse=","),
                                                  level="4",
                                                  stringsAsFactors = F))
          }
          print(i)
        }
        
        areas.of.cooperation$area.id=1:nrow(areas.of.cooperation)
        
        # set processing priorities for large number of sector combinations
        if(length(unique(areas.of.cooperation$cpc))>500){
          priority.area=cSplit(areas.of.cooperation[,c("area.id","cpc")], 2, sep=",",direction="long")
          names(priority.area)=c("area.id","cpc2")
          priority.area=merge(priority.area, sectoral.imports, by="cpc2", all.x=T)
          priority.area=aggregate(total.imports ~ area.id, priority.area, sum)
          priority.area=priority.area[order(-priority.area$total.imports),]
          priority.area$priority=1:nrow(priority.area)
          
          areas.of.cooperation=merge(areas.of.cooperation, priority.area[,c("area.id","priority")],by="area.id", all.x=T)
          areas.of.cooperation=areas.of.cooperation[order(areas.of.cooperation$priority),]
        } else {
          areas.of.cooperation$priority=1
        }
        
        for(i in 1:nrow(areas.of.cooperation)){
          
          # only run the ones not done
          if(! areas.of.cooperation$cpc[i] %in% already.run){
            
            
            if(areas.of.cooperation$level[i]==3){
              area=areas.of.cooperation$cpc[i]
              
              s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==3 & cpc.names$cpc %in% area])), collapse="")
              s.scope=paste(area, collapse = ";")
              
            } 
            
            if(areas.of.cooperation$level[i]==2){
              
              area=gta_cpc_code_expand(areas.of.cooperation$cpc[i])
              
              s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==2 & cpc.names$cpc %in% areas.of.cooperation$cpc[i]])), collapse="")
              s.scope=areas.of.cooperation$cpc[i]
            }
            
            if(areas.of.cooperation$level[i]==4){
              
              area=gta_cpc_code_expand(as.numeric(unlist(strsplit(areas.of.cooperation$cpc[i],","))))
              
              s.name="All affected by instrument(s)"
              s.scope=areas.of.cooperation$cpc[i]
            }
            
            
            
            print(paste("STARTING AREA",paste(area, collapse=";")))
            
            area.codes=intersect(subset(cpc.to.hs, cpc %in% area)$hs,unique(liberalisation.options$affected.product))
            exporters=unique(trade$a.un[trade$affected.product %in% area.codes])
            
            prize.allocation.area=subset(prize.allocation, affected.product %in% area.codes)
            area.world.imports=sum(subset(total.imports, affected.product %in% area.codes &
                                            i.un<10000)$total.imports)
            
            ## the prize in this area
            the.prize.area=subset(barrier.count, affected.product %in% area.codes)
            
            ## invoking post-liberalisation growth assumption
            the.prize.area$total.imports=the.prize.area$total.imports*growth
            
            ## extrapolating 2017 market shares for prize allocation
            benefactors=intersect(country.names$un_code[country.names$is.wto==T],exporters )
            p.a=subset(prize.allocation.area, a.un %in% benefactors)
            
            
            # removing intra-EU/EEU trade
            p.a=subset(p.a, !(i.un %in% eu.members & a.un %in% eu.members))
            p.a=subset(p.a, !(i.un %in% eeu.members & a.un %in% eeu.members))
            
            
            ## scaling by omitted exporters
            p.a=merge(p.a, aggregate(market.share ~ i.un + affected.product, p.a, sum), by=c("i.un","affected.product"), all.x=T)
            p.a$market.share=p.a$market.share.x/p.a$market.share.y
            p.a$market.share.x=NULL
            p.a$market.share.y=NULL
            
            
            
            prize.distribution.area=merge(p.a, the.prize.area, by=c("i.un","affected.product"))
            prize.distribution.area$prize.earned=prize.distribution.area$market.share*prize.distribution.area$total.imports
            prize.distribution.area=aggregate(prize.earned ~ i.un + a.un, prize.distribution.area, sum)
            
            w.count=1
            have.coalition=T
            
            while(have.coalition){
              i.weight=import.weights[w.count]
              
              
              ## calc
              ## initialise: only countries with an upside will join the coalition i.e. exporters
              print(paste("STARTING import weight",i.weight))
              
              area.codes<<-area.codes
              benefactors<<-benefactors
              the.prize.area<<-the.prize.area
              i.weight<<-i.weight
              participation.threshold<<-participation.threshold
              total.imports<<-total.imports
              area.world.imports<<-area.world.imports
              prize.distribution.area<<-prize.distribution.area
              growth<<-growth
              
              gains=gain_from_agreement(area.codes,
                                        benefactors,
                                        benefactors,
                                        "the.prize.area", 
                                        i.weight, 
                                        participation.threshold,
                                        "total.imports",
                                        "area.world.imports",
                                        "prize.distribution.area",
                                        growth)
              
              coalition=gains$coalition
              free.riders=gains$free.riders
              net.income=gains$net.income
              by.stander=gains$by.stander
              
              ## storing result
              c.id=max.cid+1
              if(length(coalition)>0){
                c.ms=data.frame(coalition.id=c.id,
                                i.un=net.income$i.un[net.income$i.un %in% coalition],
                                type="member")
                
                c.ms=merge(c.ms, subset(net.income, i.un %in% coalition)[,c("i.un","result")], all.x=T)
                c.ms[is.na(c.ms)]=0
                data.table::setnames(c.ms, "result","net.gain")
                cm=rbind(cm, 
                         c.ms)
                
              }
              
              if(nrow(free.riders)>0){
                
                
                c.ms=free.riders
                c.ms$coalition.id=c.id
                c.ms$type="freerider"
                data.table::setnames(c.ms, "result","net.gain")
                
                cm=rbind(cm, c.ms)
                
              }
              
              
              if(length(by.stander)>0){
                
                cm=rbind(cm, 
                         data.frame(i.un=by.stander,
                                    coalition.id=c.id,
                                    type="bystander",
                                    net.gain=0))
                
              }
              
              
              cs=rbind(cs,
                       data.frame(coalition.id=c.id,
                                  instruments.incl=paste(inst, collapse=";"),
                                  sector.scope=s.scope,
                                  sector.level=areas.of.cooperation$level[i],
                                  sector.name=s.name,
                                  import.utility.weight=i.weight,
                                  member.size=gains$m.count,
                                  members.liberalising=gains$lib.count,
                                  freerider.count=gains$f.count,
                                  bystander.count=gains$b.count,
                                  coalition.total.trade=gains$c.t.trade,
                                  coalition.liberalised.trade=gains$c.l.trade,
                                  intra.coalition.liberalised.trade=gains$intra.c.l.trade,
                                  share.world.imports=gains$imp.share,
                                  share.world.imports.liberalised=gains$imp.share.liberalised))
              
              
              
              ## updating counts & seeing whether to continue
              w.count=w.count+1
              have.coalition=length(coalition)>0
              if(w.count>=length(import.weights)){
                have.coalition=F
              }
              
              if(have.coalition==F){
                print(paste("no more coalitions here beyond",i.weight))
                
                remain.weights=import.weights[import.weights<i.weight]
                
                
                cs=rbind(cs,
                         data.frame(coalition.id=c((c.id+1):(c.id+length(remain.weights))),
                                    instruments.incl=paste(inst, collapse=";"),
                                    sector.scope=s.scope,
                                    sector.level=areas.of.cooperation$level[i],
                                    sector.name=s.name,
                                    import.utility.weight=remain.weights,
                                    member.size=0,
                                    members.liberalising=0,
                                    freerider.count=0,
                                    bystander.count=0,
                                    coalition.total.trade=0,
                                    coalition.liberalised.trade=0,
                                    intra.coalition.liberalised.trade=0,
                                    share.world.imports=0,
                                    share.world.imports.liberalised=0))
                
              }
              
              rm(gains, net.income, coalition, free.riders)
              rm(area.codes, benefactors,
                 the.prize.area, 
                 i.weight, 
                 participation.threshold,
                 total.imports,
                 area.world.imports,
                 prize.distribution.area,
                 growth)
              max.cid=max(cs$coalition.id)
            }
            
            
            print(paste("FINISHED AREA",paste(area, collapse=";"),"(",step,"out of",nrow(areas.of.cooperation)*length(instruments),")."))
            step=step+1
            
            
            
            if(step%%200==0){
              
              load(my.path)
              coalition.stats=rbind(coalition.stats, cs)
              coalition.members=rbind(coalition.members, cm)
              
              save(coalition.stats, coalition.members, file=my.path)
              
              max.cid=max(coalition.stats$coalition.id)
              rm(coalition.stats, coalition.members)
              
              ## re-initialise
              cm=data.frame()
              cs=data.frame(coalition.id=numeric(),
                            sector.scope=character(),
                            sector.level=numeric(),
                            sector.name=character(),
                            import.utility.weight=numeric(),
                            member.size=numeric(),
                            members.liberalising=numeric(),
                            freerider.count=numeric(),
                            bystander.count=numeric(),
                            coalition.total.trade=numeric(),
                            coalition.liberalised.trade=numeric(),
                            intra.coalition.liberalised.trade=numeric(),
                            share.world.imports=numeric(),
                            share.world.imports.liberalised=numeric())
              
              
            }
            
          }
          
        }
      }
      
      
      print(paste("FINISHED instrument:",paste(inst, collapse=";")))
      
    }
    
    
    
  }
  
  load(my.path)
  coalition.stats=rbind(coalition.stats, cs)
  coalition.members=rbind(coalition.members, cm)
  
  save(coalition.stats, coalition.members, file=my.path)
  
  if(create.xlsx){
    c.s.xlsx=coalition.stats
    names(c.s.xlsx)=c("Coalition ID","Included instruments", 
                      "Sectoral scope (CPC)","CPC level",
                      "Sector name","Import utility weight",
                      "Nr of coalition members",  "Nr of members which liberalise", 
                      "Nr of freeriding exporters","Nr of bystanding exporters",
                      "Total imports by coalition", "Total liberalised imports by coalition", 
                      "Intra-coalition liberalised imports",
                      "Share of coalition's imports in sectoral world trade", 
                      "Share of liberalised imports in sectoral world trade")
    xlsx::write.xlsx(c.s.xlsx, file=my.path.xlsx, row.names=F)
    
    
    
  }
  
  
  
}










