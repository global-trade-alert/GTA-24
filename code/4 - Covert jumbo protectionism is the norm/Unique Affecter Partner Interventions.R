affected.partner.per.intervention = master[master$gta.evaluation == c('Red','Amber'),c('intervention.id','affected.jurisdiction')]
affected.partner.per.intervention = affected.partner.per.intervention[complete.cases(affected.partner.per.intervention),]
affected.partner.per.intervention = affected.partner.per.intervention[!duplicated(affected.partner.per.intervention),]

## extract intervention ids which affect only 1 partner
unique.affected.partner.interventions = affected.partner.per.intervention[!duplicated(affected.partner.per.intervention$intervention.id)&
                                                                            !duplicated(affected.partner.per.intervention$intervention.id,fromLast=TRUE),]$intervention.id
